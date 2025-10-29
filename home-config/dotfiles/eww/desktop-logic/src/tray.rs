//! A custom tray for Wayland compositors that implement `wlr_shell`

use std::num::{NonZeroI32, TryFromIntError};

use smithay_client_toolkit::{
    compositor::{CompositorHandler, CompositorState},
    delegate_compositor, delegate_layer, delegate_output, delegate_registry, delegate_shm,
    output::{OutputHandler, OutputState},
    registry::{ProvidesRegistryState, RegistryState},
    registry_handlers,
    shell::{
        WaylandSurface as _,
        wlr_layer::{Anchor, Layer, LayerShell, LayerShellHandler, LayerSurface},
    },
    shm::{CreatePoolError, Shm, ShmHandler, multi::MultiPool, slot::CreateBufferError},
};
use thiserror::Error;
use wayland_client::{
    ConnectError, Connection, DispatchError, QueueHandle,
    globals::{BindError, GlobalError, GlobalList, registry_queue_init},
    protocol::wl_shm,
};

// See upstream example: https://github.com/Smithay/client-toolkit/blob/master/examples/simple_layer.rs
// This may also help: https://gaultier.github.io/blog/wayland_from_scratch.html

/// A simple tray
pub struct Tray;

impl Tray {
    /// Run the tray
    pub fn run() -> Result<()> {
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

        let pool = MultiPool::new(&shm)?;
        let mut state = TrayState::new(&globals, &queue_handle, shm, pool, layer);

        loop {
            event_queue.blocking_dispatch(&mut state)?;
            if state.closing {
                break Ok(());
            }
        }
    }
}

/// Tray state
struct TrayState {
    /// Tracks various Wayland globals
    registry_state: RegistryState,
    /// Tracks the state of outputs that we might create layers on
    output_state: OutputState,
    /// The shared memory into which we draw
    shm: Shm,
    /// The width of the tray
    width: i32,
    /// The height of the tray
    height: i32,
    /// The shared memory pool
    pool: MultiPool<usize>,
    /// The current buffer index
    current_buffer: usize,
    /// The layer to draw on
    layer: LayerSurface,

    /// Whether we are closing
    closing: bool,
    /// Whether the initial config has been completed
    initial_config: bool,
}

impl TrayState {
    /// Constructor for the tray state
    fn new(
        globals: &GlobalList,
        queue_handle: &QueueHandle<Self>,
        shm: Shm,
        pool: MultiPool<usize>,
        layer: LayerSurface,
    ) -> Self {
        Self {
            registry_state: RegistryState::new(globals),
            output_state: OutputState::new(globals, queue_handle),
            shm,
            width: 256,
            height: 256,
            pool,
            current_buffer: 0,
            layer,

            closing: false,
            initial_config: true,
        }
    }

    /// Draw the tray
    fn draw(&mut self, queue_handle: &QueueHandle<Self>) {
        let stride = 4;

        // i.e., double buffering
        for i in 0..=1 {
            self.current_buffer = i;

            if let Ok((_offset, buffer, canvas)) = self.pool.create_buffer(
                self.width,
                self.width * stride,
                self.height,
                &self.current_buffer,
                wl_shm::Format::Argb8888,
            ) {
                log::info!("Drawing frame!");

                // Draw to the window:
                {
                    for byte in canvas.iter_mut() {
                        *byte = 0xBB;
                    }
                }

                self.layer
                    .wl_surface()
                    .damage_buffer(0, 0, self.width, self.height);
                self.layer
                    .wl_surface()
                    .frame(queue_handle, self.layer.wl_surface().clone());
                self.layer.wl_surface().attach(Some(buffer), 0, 0);
                self.layer.commit();
            } else {
                log::warn!("Frame dropped!");
            }
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
        // TODO(tlater)
    }

    fn transform_changed(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _surface: &wayland_client::protocol::wl_surface::WlSurface,
        _new_transform: wayland_client::protocol::wl_output::Transform,
    ) {
        // TODO(tlater)
    }

    fn frame(
        &mut self,
        _conn: &Connection,
        qh: &wayland_client::QueueHandle<Self>,
        _surface: &wayland_client::protocol::wl_surface::WlSurface,
        _time: u32,
    ) {
        self.draw(qh);
    }

    fn surface_enter(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _surface: &wayland_client::protocol::wl_surface::WlSurface,
        _output: &wayland_client::protocol::wl_output::WlOutput,
    ) {
        // TODO(tlater)
    }

    fn surface_leave(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _surface: &wayland_client::protocol::wl_surface::WlSurface,
        _output: &wayland_client::protocol::wl_output::WlOutput,
    ) {
        // TODO(tlater)
    }
}

impl OutputHandler for TrayState {
    fn output_state(&mut self) -> &mut smithay_client_toolkit::output::OutputState {
        &mut self.output_state
    }

    fn new_output(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _output: wayland_client::protocol::wl_output::WlOutput,
    ) {
        // TODO(tlater): Some logic to shift the layer to the
        // "primary" output
    }

    fn update_output(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _output: wayland_client::protocol::wl_output::WlOutput,
    ) {
        // TODO(tlater): Some logic to update window sizes and such?
        // Idk
    }

    fn output_destroyed(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _output: wayland_client::protocol::wl_output::WlOutput,
    ) {
        // TODO(tlater): Some logic to shift the layer to the
        // "primary" output
    }
}

impl LayerShellHandler for TrayState {
    fn closed(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _layer: &smithay_client_toolkit::shell::wlr_layer::LayerSurface,
    ) {
        self.closing = true;
    }

    #[expect(
        clippy::expect_used,
        reason = "No option but to do an unclean u32 -> i32 conversion here"
    )]
    fn configure(
        &mut self,
        _conn: &Connection,
        qh: &wayland_client::QueueHandle<Self>,
        _layer: &smithay_client_toolkit::shell::wlr_layer::LayerSurface,
        configure: smithay_client_toolkit::shell::wlr_layer::LayerSurfaceConfigure,
        _serial: u32,
    ) {
        self.width = NonZeroI32::new(
            configure
                .new_size
                .0
                .try_into()
                .expect("must be a valid i32"),
        )
        .map_or(256, NonZeroI32::get);
        self.height = NonZeroI32::new(
            configure
                .new_size
                .1
                .try_into()
                .expect("must be a valid i32"),
        )
        .map_or(256, NonZeroI32::get);

        if self.initial_config {
            self.initial_config = false;
            self.draw(qh);
        }
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
    /// TODO
    #[error(transparent)]
    CreateBuffer(#[from] CreateBufferError),
    /// TODO
    #[error(transparent)]
    BufferSize(#[from] TryFromIntError),
}

type Result<T> = std::result::Result<T, TrayError>;
