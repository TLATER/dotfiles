use futures::{pin_mut, StreamExt};
use std::env;
use tokio::{
    io::AsyncWriteExt,
    net::{unix::OwnedReadHalf, UnixStream},
};

use thiserror::Error;

use crate::sway_ipc::{stream_sway_socket, Event, SwayMessage};

// use crate::sway_ipc::iterate_socket;

/// Start up the daemon, listening for commands
pub async fn listen() -> Result<()> {
    log::info!("Connecting to sway socket");
    let sway_socket =
        UnixStream::connect(env::var_os("SWAYSOCK").ok_or(Error::MissingSwaySocket)?).await?;
    let (reader, mut writer) = sway_socket.into_split();

    let reader = sway_reader(reader);

    writer
        .write_all(&SwayMessage::GetWorkspaces.into_bytes())
        .await
        .expect("failed to write to socket");

    writer
        .write_all(&SwayMessage::Subscribe(vec![Event::Workspace, Event::Tick]).into_bytes())
        .await
        .expect("failed to write to socket");

    writer
        .write_all(&SwayMessage::GetOutputs.into_bytes())
        .await
        .expect("failed to write to socket");

    writer
        .write_all(&SwayMessage::GetTree.into_bytes())
        .await
        .expect("failed to write to socket");

    writer
        .write_all(&SwayMessage::GetMarks.into_bytes())
        .await
        .expect("failed to write to socket");

    writer
        .write_all(&SwayMessage::GetBarConfig(None).into_bytes())
        .await
        .expect("failed to write to socket");

    writer
        .write_all(&SwayMessage::GetBarConfig(Some("id".to_owned())).into_bytes())
        .await
        .expect("failed to write to socket");

    writer
        .write_all(&SwayMessage::GetVersion.into_bytes())
        .await
        .expect("failed to write to socket");

    writer
        .write_all(&SwayMessage::GetBindingModes.into_bytes())
        .await
        .expect("failed to write to socket");

    writer
        .write_all(&SwayMessage::GetConfig.into_bytes())
        .await
        .expect("failed to write to socket");

    writer
        .write_all(&SwayMessage::SendTick("🦆".to_owned()).into_bytes())
        .await
        .expect("failed to write to socket");

    writer
        .write_all(&SwayMessage::GetBindingState.into_bytes())
        .await
        .expect("failed to write to socket");

    writer
        .write_all(&SwayMessage::GetInputs.into_bytes())
        .await
        .expect("failed to write to socket");

    writer
        .write_all(&SwayMessage::GetSeats.into_bytes())
        .await
        .expect("failed to write to socket");

    reader.await;

    Ok(())
}

/// Read messages from the sway socket
pub async fn sway_reader(socket: OwnedReadHalf) {
    let stream = stream_sway_socket(socket);
    pin_mut!(stream);

    while let Some(message) = stream.next().await {
        match message {
            Ok(reply) => {
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
