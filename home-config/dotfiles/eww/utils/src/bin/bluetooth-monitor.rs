use log::LevelFilter;
use tokio_stream::StreamExt;
use utils::bluetooth_monitor::BluetoothMonitor;

#[tokio::main(flavor = "current_thread")]
async fn main() {
    pretty_env_logger::formatted_builder()
        .filter_level(LevelFilter::Info)
        .init();

    let monitor = BluetoothMonitor::new(10);
    let mut stream = monitor.monitor().await.unwrap();
    while let Some(event) = stream.next().await {
        println!("{:?}", event);
    }
}
