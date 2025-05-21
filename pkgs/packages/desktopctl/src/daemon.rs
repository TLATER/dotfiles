use futures::{pin_mut, StreamExt};
use std::env;
use tokio::{
    io::AsyncWriteExt,
    net::{unix::OwnedReadHalf, UnixStream},
};

use thiserror::Error;

use crate::sway_ipc::{reply_types::Node, stream_sway_socket, Event, SwayMessage, SwayReply};

// use crate::sway_ipc::iterate_socket;

struct Context {
    workspace_indices: Vec<u32>,
}

/// Start up the daemon, listening for commands
pub async fn listen() -> Result<()> {
    log::info!("Connecting to sway socket");
    let sway_socket =
        UnixStream::connect(env::var_os("SWAYSOCK").ok_or(Error::MissingSwaySocket)?).await?;
    let (reader, mut writer) = sway_socket.into_split();

    let reader = sway_reader(reader);

    writer
        .write_all(&SwayMessage::Subscribe(vec![Event::Workspace, Event::Binding]).into_bytes())
        .await
        .expect("failed to write to socket");

    reader.await;

    Ok(())
}

/// Read messages from the sway socket
pub async fn sway_reader(socket: OwnedReadHalf, context: &mut Context) {
    let stream = stream_sway_socket(socket);
    pin_mut!(stream);

    while let Some(message) = stream.next().await {
        match message {
            Ok(reply) => {
                match reply {
                    SwayReply::WorkspaceEvent(event) => {
                        if let Some(Node::Workspace { generic, .. }) = event.current {
                            {
                                if let Ok(index) = generic.name.parse::<u32>() {
                                    let row: u32 = index
                                        .try_into()
                                        .expect("index cannot be larger than usize")
                                        / 9;

                                    context.workspace_indices[row] = index % 9;
                                }
                            }
                        }
                    }
                    SwayReply::WorkspaceEvent(event) => {}
                    _ => log::info!("Received message from sway: {:?}", reply),
                }

                log::info!("Received reply from sway: {:?}", reply);
            }
            Err(error) => {
                log::error!("Failed to parse message from sway: {:?}", error);
            }
        }
    }
}

/// Custom errors
#[derive(Debug, Error)]
pub enum Error {
    /// Error thrown when the sway socket is missing
    #[error("Could not connect to the sway IPC socket")]
    MissingSwaySocket,
    /// Error thrown when the connection to the sway socket fails
    #[error("Could not connect ot the sway IPC socket")]
    SwaySocketConnectionFailure(#[from] std::io::Error),
}

type Result<T> = std::result::Result<T, Error>;
