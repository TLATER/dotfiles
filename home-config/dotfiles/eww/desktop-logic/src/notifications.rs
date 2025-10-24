//! Eww-based notification daemon
#![allow(dead_code, reason = "Currently in-dev")]

use std::{
    collections::BTreeMap,
    fmt::Write as _,
    sync::{Arc, RwLock},
};

use time::OffsetDateTime;
use tokio::time::interval;
use tokio_stream::{StreamExt as _, wrappers::IntervalStream};

/// The default timeout after which notifications with no specified
/// expiry time expire
const DEFAULT_EXPIRE_INTERVAL: std::time::Duration = std::time::Duration::from_secs(1);
/// The variable name under which the notifications are exposed inside
/// eww
const EWW_NOTIFICATION_VARIABLE: &str = "DL_NOTIFICATIONS";
const EWW_NOTIFICATION_COUNT_VARIABLE: &str = "DL_NOTIFICATION_COUNT";

/// Internal representation of a ``DBus`` notification.
///
/// The FDO spec does not require implementing *all* possible
/// features, and allows ignoring most notification contents.
///
/// We only implement showing the summary string with an icon, as well
/// as the notification persistence features.
#[derive(Clone, Debug)]
pub struct Notification {
    /// The notification summary
    summary: String,
    /// The date/time at which the notification should naturally
    /// expire, or `None` if it should not expire
    timeout: Option<OffsetDateTime>,
}

/// Implementation of the `org.freedesktop.Notifications` ``DBus``
/// interface.
///
/// Call `run()` to listen for and track notifications received on the
/// ``DBus`` interface according to the spec, and report them as a
/// widget to an eww variable.
pub struct NotificationHandler {
    /// The set of notifications currently tracked by this handler
    notifications: Arc<RwLock<BTreeMap<u32, Notification>>>,
}

impl NotificationHandler {
    /// Constructor for the `NotificationHandler`
    pub fn try_new() -> Self {
        let mut sample_notifications = BTreeMap::new();
        sample_notifications.insert(
            1,
            Notification {
                summary: String::from("First test notification"),
                timeout: None,
            },
        );
        sample_notifications.insert(
            2,
            Notification {
                summary: String::from("Second test notification"),
                timeout: None,
            },
        );

        Self {
            notifications: Arc::new(RwLock::new(sample_notifications)),
        }
    }

    /// Run the notification handler
    pub async fn run(&self) {
        let mut interval = IntervalStream::new(interval(DEFAULT_EXPIRE_INTERVAL));

        while (interval.next().await).is_some() {
            #[expect(
                clippy::expect_used,
                reason = "Mutex locks are pretty much impossible to handle"
            )]
            let notifications: Vec<Notification> = self
                .notifications
                .read()
                .expect("Failed to acquire lock on notification map; poisoned mutex")
                .values()
                .cloned()
                .collect();

            if let Err(err) = eww_update(&notifications) {
                log::error!("Failed to update eww variable: {err:?}");
            }
        }
    }
}

/// Update the eww variable
fn eww_update(notifications: &[Notification]) -> std::result::Result<(), std::io::Error> {
    let widgets = notifications
        .iter()
        .fold(String::new(), |mut acc, notification| {
            let _ = write!(acc, "(label :text \"{}\")", notification.summary);
            acc
        });
    let var_string = format!("{EWW_NOTIFICATION_VARIABLE}=(stack :selected notification_index {widgets})");

    log::debug!("Setting eww variable: {var_string}");
    std::process::Command::new("eww")
        .args([
            "--config",
            "/home/tlater/.local/src/dotfiles/home-config/dotfiles/eww",
            "update",
            &var_string,
            &format!("{EWW_NOTIFICATION_COUNT_VARIABLE}={}", notifications.len()),
        ])
        .status()?;

    Ok(())
}
