//! A custom tray for Wayland compositors that implement `wlr_shell`

use smithay_client_toolkit::{
    compositor::{CompositorHandler, CompositorState},
    delegate_compositor, delegate_layer, delegate_output, delegate_registry, delegate_shm,
    output::{OutputHandler, OutputState},
    registry::{ProvidesRegistryState, RegistryState},
    registry_handlers,
    shell::{
        WaylandSurface as _,
        wlr_layer::{Anchor, Layer, LayerShell, LayerShellHandler},
    },
    shm::{CreatePoolError, Shm, ShmHandler, slot::SlotPool},
};
use thiserror::Error;
use wayland_client::{
    ConnectError, Connection, DispatchError,
    globals::{BindError, GlobalError, GlobalList, registry_queue_init},
};

// See upstream example: https://github.com/Smithay/client-toolkit/blob/master/examples/simple_layer.rs
// This may also help: https://gaultier.github.io/blog/wayland_from_scratch.html

pub struct Tray;

impl Tray {
    /// Constructor for the tray
    pub fn new() -> Self {
        Self
    }

    /// Run the tray
    pub fn run(&self) -> Result<()> {
        let connection = Connection::connect_to_env()?;
        let (globals, mut event_queue) = registry_queue_init(&connection)?;
        let queue_handle = event_queue.handle();

        let compositor = CompositorState::bind(&globals, &queue_handle)?;
        let layer_shell = LayerShell::bind(&globals, &queue_handle)?;
        let shm = Shm::bind(&globals, &queue_handle)?;

        let surface = compositor.create_surface(&queue_handle);
        let layer = layer_shell.create_layer_surface(
            &queue_handle,
            surface,
            Layer::Top,
            Some("tray"),
            None,
        );

        layer.set_anchor(Anchor::BOTTOM);
        layer.set_keyboard_interactivity(
            smithay_client_toolkit::shell::wlr_layer::KeyboardInteractivity::OnDemand,
        );
        layer.set_size(256, 256);

        // In order for the layer surface to be mapped, we need to perform an initial commit with no attached\
        // buffer. For more info, see WaylandSurface::commit
        //
        // The compositor will respond with an initial configure that we can then use to present to the layer
        // surface with the correct options.
        layer.commit();

        // We don't know how large the window will be yet, so lets assume the minimum size we suggested for the
        // initial memory allocation.
        let _pool = SlotPool::new(256 * 256 * 4, &shm)?;

        let mut state = TrayState::new(&globals, shm);

        loop {
            event_queue.blocking_dispatch(&mut state)?;
        }
    }
}

/// Tray state
struct TrayState {
    /// The registry which tracks various Wayland globals
    registry_state: RegistryState,
    /// The shared memory into which we draw
    shm: Shm,
}

impl TrayState {
    /// Constructor for the tray state
    fn new(globals: &GlobalList, shm: Shm) -> Self {
        Self {
            registry_state: RegistryState::new(globals),
            shm,
        }
    }
}

impl CompositorHandler for TrayState {
    fn scale_factor_changed(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _surface: &wayland_client::protocol::wl_surface::WlSurface,
        _new_factor: i32,
    ) {
        todo!()
    }

    fn transform_changed(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _surface: &wayland_client::protocol::wl_surface::WlSurface,
        _new_transform: wayland_client::protocol::wl_output::Transform,
    ) {
        todo!()
    }

    fn frame(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _surface: &wayland_client::protocol::wl_surface::WlSurface,
        _time: u32,
    ) {
        todo!()
    }

    fn surface_enter(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _surface: &wayland_client::protocol::wl_surface::WlSurface,
        _output: &wayland_client::protocol::wl_output::WlOutput,
    ) {
        todo!()
    }

    fn surface_leave(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _surface: &wayland_client::protocol::wl_surface::WlSurface,
        _output: &wayland_client::protocol::wl_output::WlOutput,
    ) {
        todo!()
    }
}

impl OutputHandler for TrayState {
    fn output_state(&mut self) -> &mut smithay_client_toolkit::output::OutputState {
        todo!()
    }

    fn new_output(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _output: wayland_client::protocol::wl_output::WlOutput,
    ) {
        todo!()
    }

    fn update_output(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _output: wayland_client::protocol::wl_output::WlOutput,
    ) {
        todo!()
    }

    fn output_destroyed(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _output: wayland_client::protocol::wl_output::WlOutput,
    ) {
        todo!()
    }
}

impl LayerShellHandler for TrayState {
    fn closed(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _layer: &smithay_client_toolkit::shell::wlr_layer::LayerSurface,
    ) {
        todo!()
    }

    fn configure(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _layer: &smithay_client_toolkit::shell::wlr_layer::LayerSurface,
        _configure: smithay_client_toolkit::shell::wlr_layer::LayerSurfaceConfigure,
        _serial: u32,
    ) {
        todo!()
    }
}

impl ShmHandler for TrayState {
    fn shm_state(&mut self) -> &mut Shm {
        &mut self.shm
    }
}

delegate_compositor!(TrayState);
delegate_output!(TrayState);
delegate_shm!(TrayState);
delegate_layer!(TrayState);
delegate_registry!(TrayState);

impl ProvidesRegistryState for TrayState {
    fn registry(&mut self) -> &mut smithay_client_toolkit::registry::RegistryState {
        &mut self.registry_state
    }

    registry_handlers![OutputState];
}

/// Error type for this module
#[derive(Debug, Error)]
pub enum TrayError {
    /// Failed to set up a Wayland connection
    #[error(transparent)]
    Conect(#[from] ConnectError),
    /// Failed to dispatch a Wayland protocol message
    #[error(transparent)]
    Dispatch(#[from] DispatchError),
    /// TODO
    #[error(transparent)]
    Global(#[from] GlobalError),
    /// TODO
    #[error(transparent)]
    Bind(#[from] BindError),
    /// TODO
    #[error(transparent)]
    CreatePool(#[from] CreatePoolError),
}

type Result<T> = std::result::Result<T, TrayError>;
