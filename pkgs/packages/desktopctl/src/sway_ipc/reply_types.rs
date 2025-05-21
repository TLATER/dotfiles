use serde::Deserialize;
use serde_json::Value;
use serde_repr::Deserialize_repr;

macro_rules! reply_from_bytes {
    ($v:path, $s:expr) => {
        serde_json::from_slice($s).map(|j| $v(j))
    };
}

pub(crate) use reply_from_bytes;

/// A reply to a command from sway
#[derive(Debug)]
pub enum SwayReply {
    Command(Vec<CommandResult>),
    GetWorkspaces(Vec<Workspace>),
    Subscribe(SuccessReply),
    GetOutputs(Vec<Output>),
    GetTree(Node),
    GetMarks(Vec<String>),
    GetBarConfig(GetBarConfigReply),
    GetVersion(VersionInfo),
    GetBindingModes(Vec<String>),
    GetConfig(GetConfigReply),
    SendTick(SuccessReply),
    Sync(SuccessReply),
    GetBindingState(GetBindingReply),
    GetInputs(Vec<Input>),
    GetSeats(Value), // TODO(tlater): Replace with proper struct
    WorkspaceEvent(Box<WorkspaceEvent>),
    OutputEvent(OutputEvent),
    ModeEvent(ModeEvent),
    WindowEvent(WindowEvent),
    BarconfigUpdateEvent(Value), // TODO(tlater): Replace with proper struct
    BindingEvent(BindingEvent),
    ShutdownEvent(ShutdownEvent),
    TickEvent(TickEvent),
    BarStateUpdateEvent(BarStateUpdateEvent),
    InputEvent(InputEvent),
    Unknown(Vec<u8>),
}

/// The payload of a reply to a `Command` message
#[derive(Debug, Deserialize)]
pub struct CommandResult {
    success: bool,
    #[serde(default)]
    parse_error: bool,
    error: Option<String>,
}

/// The payload of a reply to a `GetWorkspace` message
#[derive(Debug, Deserialize)]
pub struct Workspace {
    num: u32,
    name: String,
    visible: bool,
    focused: bool,
    urgent: bool,
    rect: Rectangle,
    output: String,
}

/// A rectangle object, as used in some replies
#[derive(Debug, Deserialize)]
pub struct Rectangle {
    x: u32,
    y: u32,
    width: u32,
    height: u32,
}

/// The response to a subscribe message
#[derive(Debug, Deserialize)]
pub struct SuccessReply {
    success: bool,
}

/// The response to a `GetOutputs` message
#[derive(Debug, Deserialize)]
#[allow(clippy::struct_excessive_bools)]
pub struct Output {
    name: String,
    make: String,
    model: String,
    serial: String,
    active: bool,
    dpms: bool,
    power: bool,
    primary: bool,
    scale: f32,
    subpixel_hinting: SubpixelHinting,
    transform: String,
    current_workspace: String,
    modes: Vec<Mode>,
    current_mode: Mode,
    rect: Rectangle,
}

/// The subpixel hinting in use on the output
#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SubpixelHinting {
    Rgb,
    Bgr,
    Vrgb,
    Vbgr,
    None,
    Unknown,
}

/// A display mode
#[derive(Debug, Deserialize)]
pub struct Mode {
    width: u32,
    height: u32,
    refresh: u32,
}

/// The response to a `GetTree` message
#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum Node {
    Window {
        pid: u32,
        app_id: String,
        foreign_toplevel_identifier: String,
        visible: bool,
        shell: String,
        inhibit_idle: bool,
        idle_inhibitors: IdleInhibitors,

        // Only if a sandbox engine is used
        sandbox_engine: Option<String>,
        sandbox_app_id: Option<String>,
        sandbox_instance_id: Option<String>,

        // Only on X11 windows
        window: Option<u32>,
        window_properties: Option<X11WindowProperties>,

        #[serde(flatten)]
        generic: Box<GenericNode>,
    },

    Workspace {
        representation: String,

        #[serde(flatten)]
        generic: GenericNode,
    },

    Generic {
        #[serde(flatten)]
        generic: GenericNode,
    },
}

#[derive(Debug, Deserialize)]
pub struct GenericNode {
    id: u32,
    #[serde(rename = "type")]
    kind: NodeType,
    orientation: Orientation,
    percent: Option<f32>,
    urgent: bool,
    marks: Vec<String>,
    focused: bool,
    layout: Layout,
    border: BorderStyle,
    current_border_width: u32,
    rect: Rectangle,
    deco_rect: Rectangle,
    window_rect: Rectangle,
    geometry: Rectangle,
    name: String,
    nodes: Vec<Node>,
    floating_nodes: Vec<Node>,
    focus: Vec<u32>,
    fullscreen_mode: FullscreenMode,
    sticky: bool,
    floating: Option<FloatingState>,
    scratchpad_state: Option<ScratchpadState>,

    // Always null, except on window nodes
    #[serde(skip)]
    window: Option<u32>,
}

/// The type of a `Node`
#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum NodeType {
    Root,
    Output,
    Workspace,
    Con,
    FloatingCon,
}

/// The style of a node border
#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BorderStyle {
    Normal,
    None,
    Pixel,
    Csd,
}

/// The layout of the node
#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Layout {
    Splith,
    Splity,
    Stacked,
    Tabbed,
    Output,
    None,
}

/// The orientation of the node
#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Orientation {
    Vertical,
    Horizontal,
    None,
}

/// The fullscreen mode of the node
#[derive(Debug, Deserialize_repr)]
#[repr(u32)]
pub enum FullscreenMode {
    None = 0,
    FullWorkspace = 1,
    Global = 2,
}

/// The floating state of the node
#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FloatingState {
    AutoOff,
    UserOn,
}

/// The scratchpad state of the node
#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ScratchpadState {
    None,
    Fresh,
}

/// The idle inhibitors for a node
#[derive(Debug, Deserialize)]
pub struct IdleInhibitors {
    application: IdleInhibitor,
    user: IdleInhibitor,
}

/// The types of idle inhibitors that can be set
#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum IdleInhibitor {
    Focus,
    Fullscreen,
    Open,
    None,
    Visible,
}

/// The X11 window properties of a node
#[derive(Debug, Deserialize)]
pub struct X11WindowProperties {
    title: String,
    class: String,
    instance: String,
    transient_for: Option<String>,
}

/// The result of a `GetBarConfig` message
#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum GetBarConfigReply {
    NoPayload(Vec<String>),
    Payload(Value), // TODO(tlater): Replace with proper struct
}

/// Information about the sway version
#[derive(Debug, Deserialize)]
pub struct VersionInfo {
    major: u32,
    minor: u32,
    patch: u32,
    human_readable: String,
    loaded_config_file_name: String,
}

/// The reply to a `GetConfig` message
#[derive(Debug, Deserialize)]
pub struct GetConfigReply {
    config: String,
}

/// The currently active binding
#[derive(Debug, Deserialize)]
pub struct GetBindingReply {
    name: String,
}

/// A workspace event
#[derive(Debug, Deserialize)]
pub struct WorkspaceEvent {
    change: WorkspaceChangeType,
    current: Option<Node>,
    old: Option<Node>,
}

/// The change that occurred to the workspace
#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum WorkspaceChangeType {
    Init,
    Empty,
    Focus,
    Move,
    Rename,
    Urgent,
    Reload,
}

/// An output event
#[derive(Debug, Deserialize)]
pub struct OutputEvent {
    change: OutputEventReason,
}

/// The reason for the output event
#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OutputEventReason {
    Unspecified,
}

/// A mode event
#[derive(Debug, Deserialize)]
pub struct ModeEvent {
    /// The name of the binding mode that is now active
    change: String,
    /// Whether the mode should be parsed as pango markup
    pango_markup: bool,
}

/// A window event
#[derive(Debug, Deserialize)]
pub struct WindowEvent {
    change: WindowChangeType,
    container: Node,
}

/// The change that occurred to the window
#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum WindowChangeType {
    New,
    Close,
    Focus,
    Title,
    FullscreenMode,
    Move,
    Floating,
    Urgent,
    Mark,
}

/// A key binding event
#[derive(Debug, Deserialize)]
pub struct BindingEvent {
    change: BindingChangeType,
    command: String,
    /// Modifiers on the binding
    event_state_mask: Vec<String>,
    /// Keyboard bind code
    input_code: u32,
    /// The symbol of the bindsym for the binding
    symbol: Option<String>,
    input_type: BindingInputType,
}

/// The event that occurred on the binding
#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BindingChangeType {
    Run,
}

/// The input type
#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BindingInputType {
    Keyboard,
    Mouse,
}

/// A shutdown event
#[derive(Debug, Deserialize)]
pub struct ShutdownEvent {
    change: ShutdownType,
}

/// The type of the shutdown event
#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ShutdownType {
    Exit,
}

/// A tick event
#[derive(Debug, Deserialize)]
pub struct TickEvent {
    first: bool,
    payload: String,
}

/// A bar state update
#[derive(Debug, Deserialize)]
pub struct BarStateUpdateEvent {
    id: String,
    visible_by_modifier: bool,
}

/// An input event
#[derive(Debug, Deserialize)]
pub struct InputEvent {
    change: InputEventType,
    input: Input,
}

/// The type of the input event
#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum InputEventType {
    Added,
    Removed,
    XkbKeymap,
    XkbLayout,
    LibinputConfig,
}

/// An input method
#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum Input {
    Keyboard {
        xkb_active_layout_name: String,
        xkb_layout_names: Vec<String>,
        xkb_active_layout_index: usize,

        #[serde(flatten)]
        generic: GenericInput,
    },

    Pointer {
        scroll_factor: f32,

        #[serde(flatten)]
        generic: GenericInput,
    },
}

#[derive(Debug, Deserialize)]
pub struct GenericInput {
    identifier: String,
    name: String,
    vendor: u32,
    product: u32,
    #[serde(rename = "type")]
    kind: InputType,
    libinput: Value, // TODO(tlater): Replace with proper struct
}

/// The type of the input
#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum InputType {
    Keyboard,
    Pointer,
    Touch,
    TabletTool,
    TabletPad,
    Switch,
}
