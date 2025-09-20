use zbus::{Result, interface};

use desktop_logic::notifications::NotificationHandler;

struct NotificationWidget;

#[interface(name = "net.tlater.NotificationWidget")]
impl NotificationWidget {}

#[tokio::main]
async fn main() -> Result<()> {
    env_logger::init();

    let notification_handler = NotificationHandler::new();

    let _connection = zbus::connection::Builder::session()?
        .name("net.tlater.NotificationWidget")?
        .serve_at("/net/tlater/NotificationWidget", NotificationWidget)?
        .build()
        .await?;

    notification_handler.run().await?;

    Ok(())
}
