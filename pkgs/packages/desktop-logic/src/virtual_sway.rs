#![cfg(test)]

use const_str::concat_bytes;
use inotify::{Inotify, WatchMask};
use serde::Deserialize;
use std::{
    io::Write as _,
    os::{fd::IntoRawFd as _, unix::net::UnixStream},
    process::{Child, Command},
};
use tempfile::TempDir;

#[derive(Deserialize)]
pub enum SwayReply {
    Tree(Node),
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct Node {
    pub id: u32,
    pub name: String,
}

pub struct VirtualSway {
    tempdir: TempDir,
    process_handle: Child,

    wayland_socket: UnixStream,
    sway_socket: UnixStream,
}

impl VirtualSway {
    /// Start a virtual sway instance
    pub fn start() -> Result<Self> {
        let tempdir = TempDir::with_prefix("desktop-logic-test").map_err(Error::SocketCreation)?;
        let sway_socket_path = tempdir.path().join("sway.socket");

        let mut inotify = Inotify::init().map_err(Error::SocketCreation)?;
        inotify
            .watches()
            .add(tempdir.path(), WatchMask::CREATE)
            .map_err(Error::SwaySocketWait)?;

        let (wayland_socket, wayland_socket_sway_end) =
            UnixStream::pair().map_err(Error::SocketCreation)?;

        let process_handle = Command::new("sway")
            .env(
                "WAYLAND_SOCKET",
                wayland_socket_sway_end.into_raw_fd().to_string(),
            )
            .env("SWAYSOCK", &sway_socket_path)
            .env("WLR_BACKENDS", "headless")
            .args(["--config", "/dev/null"])
            .spawn()
            .map_err(Error::Sway)?;

        inotify
            .read_events_blocking(&mut [0; 1024])
            .map_err(Error::SwaySocketWait)?;

        let sway_socket = UnixStream::connect(sway_socket_path).map_err(Error::SocketCreation)?;

        Ok(Self {
            tempdir,
            process_handle,

            wayland_socket,
            sway_socket,
        })
    }

    /// Get the wayland socket
    pub fn wayland_socket(&self) -> Result<UnixStream> {
        self.wayland_socket
            .try_clone()
            .map_err(Error::SocketCreation)
    }

    fn send_message(&mut self, kind: u32, payload: &[u8]) -> Result<SwayReply> {
        let mut message = Vec::with_capacity(payload.len() + 32 + 32 + b"i3-ipc".len());
        message.extend_from_slice(b"i3-ipc");
        message.extend_from_slice(&u32::to_ne_bytes(payload.len().try_into().unwrap()));
        message.extend_from_slice(&u32::to_ne_bytes(kind));
        message.extend_from_slice(payload);

        self.sway_socket
            .write_all(&message)
            // .write_all(concat_bytes!(b"i3-ipc", u32::to_ne_bytes(kind)))
            .map_err(Error::Communication)?;

        serde_json::Deserializer::from_reader(&self.sway_socket)
            .into_iter()
            .next()
            .transpose()?
            .ok_or(VirtualSwayError::NoReply)
    }

    /// Get the sway tree
    pub fn get_tree(&mut self) -> Result<Node> {
        if let SwayReply::Tree(node) = self.send_message(4, b"")? {
            Ok(node)
        } else {
            Err(VirtualSwayError::NoReply)
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum VirtualSwayError {
    #[error("Failed to create socket")]
    SocketCreation(#[source] std::io::Error),
    #[error("Sway command failed")]
    Sway(#[source] std::io::Error),
    #[error("Failed to wait for sway socket to come online")]
    SwaySocketWait(#[source] std::io::Error),
    #[error("Failed to communicate with sway")]
    Communication(#[source] std::io::Error),
    #[error("Sway replied with an invalid message")]
    InvalidReply(#[from] serde_json::Error),
    #[error("Sway failed to reply with a message")]
    NoReply,
}

type Error = VirtualSwayError;
type Result<T> = std::result::Result<T, Error>;
