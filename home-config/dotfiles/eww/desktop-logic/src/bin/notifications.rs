use zbus::Result;

use desktop_logic::notifications::NotificationHandler;

#[tokio::main]
async fn main() -> Result<()> {
    env_logger::init();

    let notification_handler = NotificationHandler::try_new().await?;
    notification_handler.run().await;

    Ok(())
}
