use clap::{Subcommand, Parser};
use log::LevelFilter;

use desktopctl::daemon;

#[derive(Parser, Debug)]
struct Args {
    #[command(subcommand)]
    command: Commands,
    #[arg(long, default_value_t = LevelFilter::Info)]
    log_level: LevelFilter
}

#[derive(Debug, Subcommand)]
enum Commands {
    /// Start the desktopctl daemon
    Daemon
}

#[tokio::main]
async fn main() {
    let args = Args::parse();

    // Disable other crate logs *except* when we set trace logging
    let log_module = match args.log_level {
        LevelFilter::Trace => None,
        _ => Some("desktopctl")
    };

    pretty_env_logger::formatted_builder()
        .filter(log_module, args.log_level).init();

    daemon::listen().await.expect("aaaa");
}
