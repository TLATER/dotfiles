use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use serde::Serialize;
use time::{Duration, OffsetDateTime};
use tokio::time::interval;
use tokio_stream::{StreamExt, wrappers::IntervalStream};
use zbus::{Result, interface, object_server::SignalEmitter};
use zvariant::{OwnedValue, Type, Value};

/// The version of the binary
const VERSION: &str = env!("CARGO_PKG_VERSION");
/// The default interval at which notifications are checked for expiry
const DEFAULT_EXPIRE_INTERVAL: std::time::Duration = std::time::Duration::from_secs(1);
/// The default timeout after which notifications with no specified
/// expiry time expire
const DEFAULT_TIMEOUT: Duration = Duration::seconds(3);

/// Implementation of the `org.freedesktop.Notifications` DBus
/// interface.
///
/// Call `run()` to listen for and track notifications received on the
/// DBus interface.
pub struct NotificationHandler {
    /// The set of notifications currently tracked by this handler
    notifications: Arc<Mutex<HashMap<u32, Notification>>>,
}

impl NotificationHandler {
    /// Constructor for the `NotificationHandler`
    pub fn new() -> Self {
        Self {
            notifications: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Run the `NotificationHandler`
    pub async fn run(self) -> zbus::Result<()> {
        let notifications = self.notifications.clone();

        let connection = zbus::connection::Builder::session()?
            .name("org.freedesktop.Notifications")?
            .serve_at("/org/freedesktop/Notifications", self)?
            .build()
            .await?;

        let mut interval = IntervalStream::new(interval(DEFAULT_EXPIRE_INTERVAL));

        while (interval.next().await).is_some() {
            let now = OffsetDateTime::now_utc();

            let expired: Vec<u32> = {
                let mut notifications = notifications
                    .lock()
                    .expect("Failed to acquire lock on notification map; poisoned mutex");

                notifications
                    .extract_if(|_, notification| {
                        notification.timeout.is_some_and(|timeout| timeout < now)
                    })
                    .map(|(id, _)| id)
                    .collect()
            };

            for id in expired {
                connection
                    .object_server()
                    .interface("/org/freedesktop/Notifications")
                    .await?
                    .notification_closed(id, NotificationClosedReason::Expired)
                    .await?;
            }
        }

        Ok(())
    }
}

impl Default for NotificationHandler {
    fn default() -> Self {
        Self::new()
    }
}

#[interface(name = "org.freedesktop.Notifications")]
impl NotificationHandler {
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
        _hints: HashMap<&str, zvariant::OwnedValue>,
        expire_timeout: i32,
    ) -> zbus::fdo::Result<u32> {
        log::info!("Received notification from `{}`", app_name);

        let notification = Notification::new(app_icon, summary, expire_timeout);

        let mut notifications = self
            .notifications
            .lock()
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
            (notifications.len() + 1).try_into().unwrap_or_else(|_| {
                log::warn!(
                    "Exceeded maximum number of notifications for `{}`; wrapping around",
                    app_name
                );

                1
            })
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
                .lock()
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
    ) -> Result<()>;
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
struct Notification {
    /// The icon of the notification
    _app_icon: String,
    /// The notification summary
    _summary: String,
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
            _app_icon: app_icon.as_ref().to_owned(),
            _summary: summary.as_ref().to_owned(),
            timeout,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[tokio::test]
    async fn send_notification() {
        let mut handler = NotificationHandler::new();

        let id = handler
            .notify(
                "test",
                0,
                "",
                "Test Notification",
                "",
                vec![],
                HashMap::new(),
                0,
            )
            .await
            .expect("Notification must successfully send");

        assert_eq!(id, 1);
    }

    #[tokio::test]
    async fn overwrite_notification() {
        let mut handler = NotificationHandler::new();

        let id = handler
            .notify(
                "test",
                0,
                "",
                "Test Notification",
                "",
                vec![],
                HashMap::new(),
                0,
            )
            .await
            .expect("Notification must successfully send");

        assert_eq!(id, 1);

        let id = handler
            .notify(
                "test",
                0,
                "",
                "Another Notification",
                "",
                vec![],
                HashMap::new(),
                0,
            )
            .await
            .expect("Notification must successfully send");

        assert_eq!(id, 2);

        let id = handler
            .notify(
                "test",
                1,
                "",
                "Overwritten Notification",
                "",
                vec![],
                HashMap::new(),
                0,
            )
            .await
            .expect("Notification must successfully send");

        assert_eq!(id, 1);
    }

    #[tokio::test]
    async fn overwrite_nonexistent_notification() {
        let mut handler = NotificationHandler::new();

        let id = handler
            .notify(
                "test",
                1,
                "",
                "Test Notification",
                "",
                vec![],
                HashMap::new(),
                0,
            )
            .await;

        assert_eq!(
            id,
            Err(zbus::fdo::Error::InvalidArgs(
                "Notification to replace does not exist".to_owned()
            ))
        );
    }
}
