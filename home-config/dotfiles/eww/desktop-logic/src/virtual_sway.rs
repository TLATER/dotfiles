#![cfg(test)]

use inotify::{Inotify, WatchMask};
use std::{
    os::{fd::IntoRawFd as _, unix::net::UnixStream},
    process::{Child, Command},
};
use tempfile::TempDir;

pub struct VirtualSway {
    tempdir: TempDir,
    process_handle: Child,

    pub wayland_socket: UnixStream,
    sway_socket: UnixStream,
}

impl VirtualSway {
    /// Start a virtual sway instance
    pub fn start() -> Result<Self> {
        let tempdir =
            TempDir::with_prefix("desktop-logic-test").map_err(VirtualSwayError::SocketCreation)?;
        let sway_socket_path = tempdir.path().join("sway.socket");

        let mut inotify = Inotify::init().map_err(VirtualSwayError::SocketCreation)?;
        inotify
            .watches()
            .add(tempdir.path(), WatchMask::CREATE)
            .map_err(VirtualSwayError::SwaySocket)?;

        let (wayland_socket, wayland_socket_sway_end) =
            UnixStream::pair().map_err(VirtualSwayError::SocketCreation)?;

        let process_handle = Command::new("sway")
            .env(
                "WAYLAND_SOCKET",
                wayland_socket_sway_end.into_raw_fd().to_string(),
            )
            .env("SWAYSOCK", &sway_socket_path)
            .env("WLR_BACKENDS", "headless")
            .args(["--config", "/dev/null"])
            .spawn()
            .map_err(VirtualSwayError::Sway)?;

        inotify
            .read_events_blocking(&mut [0; 1024])
            .map_err(VirtualSwayError::SwaySocket)?;

        let sway_socket =
            UnixStream::connect(sway_socket_path).map_err(VirtualSwayError::SocketCreation)?;

        Ok(Self {
            tempdir,
            process_handle,

            wayland_socket,
            sway_socket,
        })
    }
}

#[derive(Debug, thiserror::Error)]
pub enum VirtualSwayError {
    #[error("Failed to create socket")]
    SocketCreation(#[source] std::io::Error),
    #[error("Sway command failed")]
    Sway(#[source] std::io::Error),
    #[error("Failed to wait for sway socket to come online")]
    SwaySocket(#[source] std::io::Error),
}

type Result<T> = std::result::Result<T, VirtualSwayError>;
