use serde::Serialize;

/// A message to send to sway
pub enum SwayMessage {
    RunCommand(String),
    GetWorkspaces,
    Subscribe(Vec<Event>),
    GetOutputs,
    GetTree,
    GetMarks,
    GetBarConfig(Option<String>),
    GetVersion,
    GetBindingModes,
    GetConfig,
    SendTick(String),
    GetBindingState,
    GetInputs,
    GetSeats,
}

impl SwayMessage {
    pub fn into_bytes(self) -> Vec<u8> {
        self.into()
    }
}

impl From<SwayMessage> for Vec<u8> {
    fn from(value: SwayMessage) -> Self {
        fn build_message(kind: u32, payload: &[u8]) -> Vec<u8> {
            let mut message = Vec::new();
            let payload_length = u32::try_from(payload.len()).expect("payload too large");

            message.extend_from_slice(b"i3-ipc");
            message.extend_from_slice(&payload_length.to_ne_bytes());
            message.extend_from_slice(&kind.to_ne_bytes());
            message.extend_from_slice(payload);

            message
        }

        match value {
            SwayMessage::RunCommand(payload) => build_message(0, payload.as_bytes()),
            SwayMessage::GetWorkspaces => build_message(1, b""),
            SwayMessage::Subscribe(payload) => build_message(
                2,
                &serde_json::to_vec(&payload).expect("could not serialize event IDs"),
            ),
            SwayMessage::GetOutputs => build_message(3, b""),
            SwayMessage::GetTree => build_message(4, b""),
            SwayMessage::GetMarks => build_message(5, b""),
            SwayMessage::GetBarConfig(None) => build_message(6, b""),
            SwayMessage::GetBarConfig(Some(bar_id)) => build_message(6, bar_id.as_bytes()),
            SwayMessage::GetVersion => build_message(7, b""),
            SwayMessage::GetBindingModes => build_message(8, b""),
            SwayMessage::GetConfig => build_message(9, b""),
            SwayMessage::SendTick(message) => build_message(10, message.as_bytes()),
            SwayMessage::GetBindingState => build_message(12, b""),
            SwayMessage::GetInputs => build_message(100, b""),
            SwayMessage::GetSeats => build_message(101, b""),
        }
    }
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Event {
    Workspace,
    Output,
    Mode,
    Window,
    BarconfigUpdate,
    Binding,
    Shutdown,
    Tick,
    BarStateUpdate,
    Input,
}
