use std::{
    collections::BTreeMap,
    sync::{Arc, RwLock},
};

use serde::Serialize;
use time::{Duration, OffsetDateTime};
use tokio::time::interval;
use tokio_stream::{StreamExt, wrappers::IntervalStream};
use zbus::{Connection, interface, object_server::SignalEmitter};
use zvariant::{OwnedValue, Type, Value};

/// The version of the binary
const VERSION: &str = env!("CARGO_PKG_VERSION");
/// The default interval at which notifications are checked for expiry
const DEFAULT_EXPIRE_INTERVAL: std::time::Duration = std::time::Duration::from_secs(1);
/// The default timeout after which notifications with no specified
/// expiry time expire
const DEFAULT_TIMEOUT: Duration = Duration::seconds(3);
/// The variable name under which the notifications are exposed inside
/// eww
const EWW_VARIABLE_NAME: &str = "DL_NOTIFICATIONS";

/// The type of the main, shared notification map
type NotificationMap = BTreeMap<u32, Notification>;

/// Implementation of the `org.freedesktop.Notifications` DBus
/// interface.
///
/// Call `run()` to listen for and track notifications received on the
/// DBus interface.
#[derive(Clone)]
pub struct NotificationHandler {
    /// The connection to the DBus
    connection: Arc<Connection>,
    /// The set of notifications currently tracked by this handler
    notifications: Arc<RwLock<NotificationMap>>,
}

impl NotificationHandler {
    /// Constructor for the `NotificationHandler`
    pub async fn try_new() -> zbus::Result<Self> {
        let notifications = Arc::new(RwLock::new(BTreeMap::new()));

        let connection = zbus::connection::Builder::session()?
            .name("org.freedesktop.Notifications")?
            .serve_at(
                "/org/freedesktop/Notifications",
                FdoNotificationInterface::new(notifications.clone()),
            )?
            .build()
            .await?;

        Ok(Self {
            connection: Arc::new(connection),
            notifications,
        })
    }

    /// Run the `NotificationHandler`'s periodic tasks indefinitely
    pub async fn run(&self) {
        Self::run_periodic_tasks(self.connection.clone(), self.notifications.clone()).await;
    }

    /// Run the periodic tasks
    async fn run_periodic_tasks(
        connection: Arc<Connection>,
        notifications: Arc<RwLock<NotificationMap>>,
    ) {
        let mut interval = IntervalStream::new(interval(DEFAULT_EXPIRE_INTERVAL));

        while (interval.next().await).is_some() {
            FdoNotificationInterface::expire(connection.clone(), notifications.clone()).await;

            let notifications = notifications
                .read()
                .expect("Failed to acquire lock on notification map; poisoned mutex")
                .values()
                .cloned()
                .collect();

            if let Err(e) = eww_update(notifications).await {
                log::error!("Failed to update eww variable: {:?}", e);
            };
        }
    }
}

/// An implementation of the Freedesktop notification service.
struct FdoNotificationInterface {
    /// Reference to the tracked notifications
    notifications: Arc<RwLock<NotificationMap>>,
}

impl FdoNotificationInterface {
    /// Constructor for the Freedesktop notification service.
    fn new(notifications: Arc<RwLock<NotificationMap>>) -> Self {
        Self { notifications }
    }

    /// Run the notification expiration routine
    async fn expire(connection: Arc<Connection>, notifications: Arc<RwLock<NotificationMap>>) {
        let now = OffsetDateTime::now_utc();

        let expired: Vec<u32> = {
            let mut notifications = notifications
                .write()
                .expect("Failed to acquire lock on notification map; poisoned mutex");

            notifications
                .extract_if(.., |_, notification| {
                    notification.timeout.is_some_and(|timeout| timeout < now)
                })
                .map(|(id, _)| id)
                .collect()
        };

        for id in expired {
            connection
                .object_server()
                .interface("/org/freedesktop/Notifications")
                .await
                .expect("failed to get interface")
                .notification_closed(id, NotificationClosedReason::Expired)
                .await
                .expect("failed to close notification");
        }
    }
}

#[interface(name = "org.freedesktop.Notifications")]
impl FdoNotificationInterface {
    /// Get the server information; See FDO spec
    #[zbus(out_args("name", "vendor", "version", "spec_version"))]
    async fn get_server_information(&self) -> (String, String, String, String) {
        (
            "desktop-logic-notifications".to_owned(),
            "net.tlater".to_owned(),
            VERSION.to_owned(),
            "1.3".to_owned(),
        )
    }

    /// Get server capabilities; See FDO spec
    async fn get_capabilities(&self) -> Vec<String> {
        vec![
            // "icon-multi"  // Vow to display an animated image
            // "icon-static" // Vow to display a static image
            "persistence".to_owned(),
        ]
    }

    /// Send a notification; See FDO spec
    #[allow(clippy::too_many_arguments)]
    async fn notify(
        &mut self,
        app_name: &str,
        replaces_id: u32,
        app_icon: &str,
        summary: &str,
        _body: &str,
        _actions: Vec<&str>,
        _hints: BTreeMap<&str, zvariant::OwnedValue>,
        expire_timeout: i32,
    ) -> zbus::fdo::Result<u32> {
        log::info!("Received notification from `{}`", app_name);

        let notification = Notification::new(app_icon, summary, expire_timeout);

        let mut notifications = self
            .notifications
            .write()
            .expect("Failed to acquire lock on notification map; poisoned mutex");

        let id = if replaces_id > 0 {
            if !notifications.contains_key(&replaces_id) {
                log::warn!(
                    "Asked to replace a notification that does not exist by `{}`",
                    app_name
                );

                return Err(zbus::fdo::Error::InvalidArgs(
                    "Notification to replace does not exist".to_owned(),
                ));
            };

            replaces_id
        } else {
            let last_id = notifications
                .last_entry()
                .map(|entry| *entry.key())
                .unwrap_or_default();

            last_id + 1
        };

        notifications.insert(id, notification);
        Ok(id)
    }

    /// Close a notification; See FDO spec
    async fn close_notification(
        &mut self,
        id: u32,
        #[zbus(signal_emitter)] emitter: SignalEmitter<'_>,
    ) -> zbus::fdo::Result<()> {
        {
            let mut notifications = self
                .notifications
                .write()
                .expect("Failed to acquire lock on notification map; poisoned mutex");
            notifications.remove_entry(&id);
        }

        emitter
            .notification_closed(id, NotificationClosedReason::ClosedByCall)
            .await?;

        Ok(())
    }

    #[zbus(signal)]
    async fn notification_closed(
        emitter: &SignalEmitter<'_>,
        id: u32,
        reason: NotificationClosedReason,
    ) -> zbus::Result<()>;
}

/// The reason a notification was closed; See FDO spec
#[derive(Serialize, Type, Value, OwnedValue)]
#[repr(u32)]
enum NotificationClosedReason {
    Expired = 1,
    Dismissed = 2,
    ClosedByCall = 3,
    Undefined = 4,
}

/// Internal representation of a DBus notification.
///
/// The FDO spec does not require implementing *all* possible
/// features, and allows ignoring most notification contents.
///
/// We only implement showing the summary string with an icon, as well
/// as the notification persistence features.
#[derive(Clone, Debug, Serialize)]
pub struct Notification {
    /// The icon of the notification
    app_icon: String,
    /// The notification summary
    summary: String,
    /// The date/time at which the notification should naturally
    /// expire, or `None` if it should not expire
    timeout: Option<OffsetDateTime>,
}

impl Notification {
    /// Constructor of a `Notification`
    fn new(app_icon: impl AsRef<str>, summary: impl AsRef<str>, timeout: i32) -> Self {
        let timeout = match timeout {
            0 => None,
            -1 => Some(OffsetDateTime::now_utc().saturating_add(DEFAULT_TIMEOUT)),
            _ => Some(
                OffsetDateTime::now_utc().saturating_add(Duration::milliseconds(timeout.into())),
            ),
        };

        Self {
            app_icon: app_icon.as_ref().to_owned(),
            summary: summary.as_ref().to_owned(),
            timeout,
        }
    }
}

/// Update the eww variable
async fn eww_update(notifications: Vec<Notification>) -> std::result::Result<(), EwwError> {
    let var_string = format!(
        "{}={}",
        EWW_VARIABLE_NAME,
        serde_json::to_string(&notifications)?
    );

    log::debug!("Setting eww variable: {}", var_string);
    std::process::Command::new("eww")
        .args(["update", &var_string])
        .status()?;

    Ok(())
}

/// Errors returned by the eww update function
#[derive(Debug, thiserror::Error)]
enum EwwError {
    /// JSON encoding errors
    #[error("failed to encode notifications as JSON")]
    Encoding(#[from] serde_json::Error),
    /// EWW command errors
    #[error("failed to update eww")]
    Command(#[from] std::io::Error),
}
