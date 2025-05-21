#![allow(dead_code)]
use std::ops::Range;

use async_stream::stream;
use futures::Stream;
use thiserror::Error;
use tokio::io::{AsyncRead, AsyncReadExt};

mod message_types;
mod reply_types;

pub use message_types::{Event, SwayMessage};
use reply_types::reply_from_bytes;
pub use reply_types::SwayReply;

const MAGIC_STRING: [u8; 6] = *b"i3-ipc";
const HEADER_SIZE: usize = MAGIC_STRING.len() + size_of::<u32>() * 2;
const LENGTH_SLICE: Range<usize> = MAGIC_STRING.len()..MAGIC_STRING.len() + size_of::<u32>();
const TYPE_SLICE: Range<usize> =
    MAGIC_STRING.len() + size_of::<u32>()..MAGIC_STRING.len() + size_of::<u32>() * 2;

// Limit the max payload size to 2MB
const MAX_BUFFER_LENGTH: u64 = 2_u64.pow(20);

/// Stream IPC messages from the sway socket
pub fn stream_sway_socket(
    mut socket: impl AsyncRead + Unpin,
) -> impl Stream<Item = Result<SwayReply>> {
    let mut header_buffer = [0; HEADER_SIZE];
    let mut payload_buffer = Vec::with_capacity(2usize.pow(10));

    stream! {
        loop {
            let message = read_message(&mut socket, &mut header_buffer, &mut payload_buffer).await;
            payload_buffer.clear();

            yield message
        }
    }
}

/// Read a single message from the socket
async fn read_message(
    socket: &mut (impl AsyncRead + Unpin),
    header_buffer: &mut [u8],
    payload_buffer: &mut Vec<u8>,
) -> Result<SwayReply> {
    // TODO(tlater): Implement handling io errors better; at the very
    // least, EOF should be handled gracefully
    socket.read_exact(header_buffer).await?;
    log::trace!("Read header: {:?}", header_buffer);

    let payload_length = header_buffer[LENGTH_SLICE]
        .try_into()
        .expect("Byte sizes are broken");
    let payload_length = u32::from_ne_bytes(payload_length);
    let payload_length: u64 = payload_length.into();

    log::debug!("Payload length: {}", payload_length);

    let message_type = header_buffer[TYPE_SLICE]
        .try_into()
        .expect("Byte sizes are broken");
    let message_type = u32::from_ne_bytes(message_type);
    log::debug!("Message type: {}", message_type);

    if payload_length > MAX_BUFFER_LENGTH {
        // Do a bunch of empty reading since rust doesn't implement a
        // better way to handle this
        let mut read = 0;
        while read < payload_length {
            let read_length = payload_length.clamp(0, MAX_BUFFER_LENGTH);
            socket.take(read_length).read_to_end(payload_buffer).await?;

            read += read_length;
        }

        return Err(Error::MessageTooLarge);
    };

    socket
        .take(payload_length)
        .read_to_end(payload_buffer)
        .await?;
    log::trace!("Payload: {}", String::from_utf8_lossy(payload_buffer));

    Ok(match message_type {
        0 => reply_from_bytes!(SwayReply::Command, payload_buffer)?,
        1 => reply_from_bytes!(SwayReply::GetWorkspaces, payload_buffer)?,
        2 => reply_from_bytes!(SwayReply::Subscribe, payload_buffer)?,
        3 => reply_from_bytes!(SwayReply::GetOutputs, payload_buffer)?,
        4 => reply_from_bytes!(SwayReply::GetTree, payload_buffer)?,
        5 => reply_from_bytes!(SwayReply::GetMarks, payload_buffer)?,
        6 => reply_from_bytes!(SwayReply::GetBarConfig, payload_buffer)?,
        7 => reply_from_bytes!(SwayReply::GetVersion, payload_buffer)?,
        8 => reply_from_bytes!(SwayReply::GetBindingModes, payload_buffer)?,
        9 => reply_from_bytes!(SwayReply::GetConfig, payload_buffer)?,
        10 => reply_from_bytes!(SwayReply::SendTick, payload_buffer)?,
        11 => reply_from_bytes!(SwayReply::Sync, payload_buffer)?,
        12 => reply_from_bytes!(SwayReply::GetBindingState, payload_buffer)?,
        100 => reply_from_bytes!(SwayReply::GetInputs, payload_buffer)?,
        101 => reply_from_bytes!(SwayReply::GetSeats, payload_buffer)?,
        0x8000_0000 => reply_from_bytes!(SwayReply::WorkspaceEvent, payload_buffer)?,
        0x8000_0001 => reply_from_bytes!(SwayReply::OutputEvent, payload_buffer)?,
        0x8000_0002 => reply_from_bytes!(SwayReply::ModeEvent, payload_buffer)?,
        0x8000_0003 => reply_from_bytes!(SwayReply::WindowEvent, payload_buffer)?,
        0x8000_0004 => reply_from_bytes!(SwayReply::BarconfigUpdateEvent, payload_buffer)?,
        0x8000_0005 => reply_from_bytes!(SwayReply::BindingEvent, payload_buffer)?,
        0x8000_0006 => reply_from_bytes!(SwayReply::ShutdownEvent, payload_buffer)?,
        0x8000_0007 => reply_from_bytes!(SwayReply::TickEvent, payload_buffer)?,
        0x8000_0014 => reply_from_bytes!(SwayReply::BarStateUpdateEvent, payload_buffer)?,
        0x8000_0015 => reply_from_bytes!(SwayReply::InputEvent, payload_buffer)?,
        _ => SwayReply::Unknown(payload_buffer.clone()),
    })
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("message payload is too large")]
    MessageTooLarge,
    #[error("sway socket closed unexpectedly")]
    UnexpectedSocketEnd(#[from] std::io::Error),
    #[error("the payload of a sway message could not be decoded")]
    InvalidJsonResponse(#[from] serde_json::Error),
}
type Result<T> = std::result::Result<T, Error>;

#[cfg(test)]
mod tests {}
