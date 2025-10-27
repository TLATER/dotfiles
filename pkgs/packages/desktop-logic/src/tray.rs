//! A custom tray for Wayland compositors that implement `wlr_shell`

use std::num::{NonZeroI32, TryFromIntError};

use fontdue;
use rust_fontconfig::{FcFontCache, FontFallbackChain};
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
use time::{OffsetDateTime, macros::format_description};
use wayland_client::{
    ConnectError, Connection, DispatchError, QueueHandle,
    globals::{BindError, GlobalError, GlobalList, registry_queue_init},
    protocol::{wl_output::WlOutput, wl_shm, wl_surface::WlSurface},
};

// See upstream example: https://github.com/Smithay/client-toolkit/blob/master/examples/simple_layer.rs
// This may also help: https://gaultier.github.io/blog/wayland_from_scratch.html

/// A simple tray
pub struct Tray;

impl Tray {
    /// Run the tray
    pub fn run() -> Result<()> {
        let connection = Connection::connect_to_env()?;
        Self::run_with_connection(&connection)
    }

    /// Run the tray, using the given wayland connection
    fn run_with_connection(connection: &Connection) -> Result<()> {
        let (globals, mut event_queue) = registry_queue_init(connection)?;
        let queue_handle = event_queue.handle();

        let compositor = CompositorState::bind(&globals, &queue_handle)?;
        let layer_shell = LayerShell::bind(&globals, &queue_handle)?;
        let shm = Shm::bind(&globals, &queue_handle)?;

        let pool = MultiPool::new(&shm)?;
        let mut state = TrayState::new(&globals, &queue_handle, compositor, layer_shell, shm, pool);

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
    /// The id of the output we are currently displaying on
    current_output: Option<WlOutput>,

    /// Reference to the compositor, to create new layers
    compositor_state: CompositorState,
    /// Reference to the layer shell
    layer_shell: LayerShell,
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
    layer: Option<LayerSurface>,

    /// Whether we are closing
    closing: bool,
    /// Whether the initial config has been completed
    initial_config: bool,

    // The font cache
    font_cache: FcFontCache,
    font_chain: FontFallbackChain,
}

impl TrayState {
    /// Constructor for the tray state
    fn new(
        globals: &GlobalList,
        queue_handle: &QueueHandle<Self>,
        compositor_state: CompositorState,
        layer_shell: LayerShell,
        shm: Shm,
        pool: MultiPool<usize>,
    ) -> Self {
        let output_state = OutputState::new(globals, queue_handle);

        let font_cache = FcFontCache::build();
        let font_chain = font_cache.resolve_font_chain(
            &["sans-serif".to_owned()],
            rust_fontconfig::FcWeight::Normal,
            rust_fontconfig::PatternMatch::False,
            rust_fontconfig::PatternMatch::False,
            &mut Vec::new(),
        );

        let now = OffsetDateTime::now_local()
            .expect("to be able to get the time")
            .format(format_description!("[hour]:[minute]"))
            .expect("to be able to format the time");

        let font_runs = font_chain.query_for_text(&font_cache, &now);

        for run in &font_runs {
            match font_info {
                Some((font_id, css_source)) => {
                    let settings = fontdue::FontSettings::default();

                    match font_cache.get_font_by_id(&font_id) {
                        Some(rust_fontconfig::FontSource::Memory(font)) => {
                            let font = fontdue::Font::from_bytes(font.bytes.clone(), settings);
                            println!("{:?}", font);
                        }
                        Some(rust_fontconfig::FontSource::Disk(font)) => {
                            let font =
                                std::fs::read(font.path.clone()).expect("file must be readable");
                            let font = fontdue::Font::from_bytes(font, settings);
                            println!("{:?}", font);
                        }
                        None => {
                            todo!()
                        }
                    };
                }
                None => println!("'{}' -> NO FONT FOUND", ch),
            }
        }

        Self {
            current_output: None,

            compositor_state,
            layer_shell,

            registry_state: RegistryState::new(globals),
            output_state,
            shm,
            width: 500,
            height: 20,
            pool,
            current_buffer: 0,
            layer: None,

            closing: false,
            initial_config: true,
            font_cache,
            font_chain,
        }
    }

    /// Get the priority of an output
    fn get_output_priority(&self, output: &WlOutput) -> usize {
        match self
            .output_state
            .info(output)
            .and_then(|output| output.name)
            .as_deref()
        {
            Some("eDP-1") => 0,
            _ => usize::MAX,
        }
    }

    /// Create the layer onto which we draw the tray
    fn create_layer(&mut self, queue_handle: &QueueHandle<Self>, output: &WlOutput) {
        self.width = self
            .output_state
            .info(output)
            .and_then(|info| info.logical_size.map(|xy| xy.0))
            .unwrap_or(200);

        let surface = self.compositor_state.create_surface(queue_handle);
        let layer = self.layer_shell.create_layer_surface(
            queue_handle,
            surface,
            Layer::Top,
            Some("tray"),
            Some(output),
        );

        layer.set_anchor(Anchor::BOTTOM);
        layer.set_size(self.width.try_into().unwrap_or(200), 20);

        // In order for the layer surface to be mapped, we need to perform an initial commit with no attached\
        // buffer. For more info, see WaylandSurface::commit
        //
        // The compositor will respond with an initial configure that we can then use to present to the layer
        // surface with the correct options.
        layer.commit();

        self.layer = Some(layer);
    }

    /// Draw the tray
    fn draw(&mut self, queue_handle: &QueueHandle<Self>) {
        if let Some(layer) = self.layer.as_ref() {
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
                    // log::info!("Drawing frame!");

                    // Draw to the window:
                    {
                        for byte in canvas.iter_mut() {
                            *byte = 0xBB;
                        }
                    }

                    layer
                        .wl_surface()
                        .damage_buffer(0, 0, self.width, self.height);
                    layer
                        .wl_surface()
                        .frame(queue_handle, layer.wl_surface().clone());
                    layer.wl_surface().attach(Some(buffer), 0, 0);
                    layer.commit();
                } else {
                    // log::warn!("Frame dropped!");
                }
            }
        }
    }
}

impl CompositorHandler for TrayState {
    fn scale_factor_changed(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _surface: &WlSurface,
        _new_factor: i32,
    ) {
        // TODO(tlater)
    }

    fn transform_changed(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _surface: &WlSurface,
        _new_transform: wayland_client::protocol::wl_output::Transform,
    ) {
        // TODO(tlater)
    }

    fn frame(
        &mut self,
        _conn: &Connection,
        qh: &wayland_client::QueueHandle<Self>,
        _surface: &WlSurface,
        _time: u32,
    ) {
        self.draw(qh);
    }

    fn surface_enter(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _surface: &WlSurface,
        _output: &WlOutput,
    ) {
        // TODO(tlater)
    }

    fn surface_leave(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _surface: &WlSurface,
        _output: &WlOutput,
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
        qh: &wayland_client::QueueHandle<Self>,
        output: WlOutput,
    ) {
        log::debug!("New monitor connected");

        if self.current_output.as_ref().is_none_or(|current_output| {
            self.get_output_priority(&output) < self.get_output_priority(current_output)
        }) {
            log::info!("Moving tray to new monitor");
            self.create_layer(qh, &output);
            self.current_output = Some(output);
        }
    }

    fn update_output(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        output: WlOutput,
    ) {
        if Some(output) == self.current_output {
            log::info!("Current output was disconnected");
            // TODO(tlater): Implement logic for shifting to another output
            self.current_output = None;
        }
    }

    fn output_destroyed(
        &mut self,
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
        _output: WlOutput,
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
        // if let Some(primary) = self.get_primary_display_info() {
        //     self.width = primary.logical_size.0;
        // };

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

#[cfg(test)]
mod tests {
    #![allow(
        clippy::expect_used,
        clippy::unwrap_used,
        reason = "These are fine in tests"
    )]

    use std::thread;

    use wayland_client::Connection;

    use crate::virtual_sway::{Node, VirtualSway};

    use super::Tray;

    #[test]
    fn open_tray() {
        let mut sway = VirtualSway::start().unwrap();

        let connection = Connection::from_socket(sway.wayland_socket().unwrap()).unwrap();

        thread::spawn(move || {
            Tray::run_with_connection(&connection).unwrap();
        });

        let tree = sway.get_tree().unwrap();
        assert_eq!(
            tree,
            Node {
                id: 1,
                name: "root".to_owned()
            }
        );
    }
}
