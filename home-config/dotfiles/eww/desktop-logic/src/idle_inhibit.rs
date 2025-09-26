//! Handle idle inhibit with eww

use std::sync::Arc;
use tokio::sync::Mutex;
use zbus::{Connection, interface, object_server::SignalEmitter, proxy, zvariant::OwnedFd};

/// A ``DBus`` service that manages inhibiting screen idling
pub struct IdleInhibitor<'login_manager> {
    /// The file descriptor returned by the freedesktop idle
    /// interface; when this file descriptor is closed (freed), the
    /// idle lock is released.
    inhibit_lock: Arc<Mutex<Option<OwnedFd>>>,
    /// A reference to the freedesktop login manager interface.
    login_manager: LoginManagerProxy<'login_manager>,
}

impl IdleInhibitor<'_> {
    /// Constructor for the `IdleInhibitor`
    pub async fn try_new() -> zbus::Result<Self> {
        let system_bus = Connection::system().await?;
        let login_manager: LoginManagerProxy = LoginManagerProxy::new(&system_bus).await?;

        Ok(Self {
            inhibit_lock: Arc::new(Mutex::new(None)),
            login_manager,
        })
    }
}

#[interface(name = "net.tlater.DesktopLogic.IdleInhibitor")]
impl IdleInhibitor<'static> {
    /// Toggle the screen idle inhibit
    async fn toggle_inhibit(
        &self,
        #[zbus(signal_emitter)] emitter: SignalEmitter<'_>,
    ) -> zbus::fdo::Result<()> {
        {
            let mut inhibit_lock = self.inhibit_lock.lock().await;

            if inhibit_lock.is_some() {
                // Dropping the old file descriptor will close it, so
                // this should signal to systemd that we're no longer
                // holding the lock.
                *inhibit_lock = None;
            } else {
                let fd: OwnedFd = self
                    .login_manager
                    .inhibit(
                        "idle",
                        "desktop-logic",
                        "Asked by the user to pause idling so that the screen doesn't go blank",
                        "block",
                    )
                    .await?;

                *inhibit_lock = Some(fd);
            }
        }

        self.is_inhibiting_changed(&emitter).await?;

        Ok(())
    }

    /// Whether screen idling is currently inhibited
    #[zbus(property)]
    async fn is_inhibiting(&self) -> bool {
        let inhibit_lock = self.inhibit_lock.lock().await;
        inhibit_lock.is_some()
    }
}

/// A proxy for the logind D-Bus API.
///
/// See docs here: https://www.freedesktop.org/software/systemd/man/latest/org.freedesktop.login1.html
#[proxy(
    default_service = "org.freedesktop.login1",
    default_path = "/org/freedesktop/login1",
    interface = "org.freedesktop.login1.Manager"
)]
trait LoginManager {
    fn inhibit(&self, what: &str, who: &str, why: &str, mode: &str) -> zbus::Result<OwnedFd>;
}
