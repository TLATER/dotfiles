use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    hash::Hash,
    sync::Arc,
};

/// Bluetooth device/status monitoring.
use log::{error, warn};
use thiserror::Error;
use tokio::sync::{
    broadcast::{Receiver, Sender},
    RwLock,
};
use tokio_stream::{
    wrappers::{errors::BroadcastStreamRecvError, BroadcastStream},
    Stream, StreamExt,
};
use zbus::{
    fdo::{InterfacesAddedStream, ObjectManagerProxy},
    proxy,
    zvariant::{OwnedObjectPath, OwnedValue},
    Connection,
};

use crate::MacAddress;

/// Enum to hold the current Bluetooth state
#[derive(Clone, Debug)]
pub enum BluetoothState {
    NoService,
    NoAdapter,
    ConnectedDevices(Vec<MacAddress>),
}

/// Simple handler for tracking BlueZ state
pub struct BluetoothMonitor {
    sender: Sender<BluetoothState>,
    receiver: Receiver<BluetoothState>,

    known_devices: HashSet<OwnedObjectPath>,
}

impl BluetoothMonitor {
    pub fn new(buffer_size: usize) -> Self {
        let (sender, receiver) = tokio::sync::broadcast::channel(buffer_size);

        Self {
            sender,
            receiver,
            known_devices: Vec::new(),
        }
    }

    /// Monitor the Bluetooth state, emitting the current state
    /// whenever something changes
    pub async fn monitor(
        self,
    ) -> Result<impl Stream<Item = std::result::Result<BluetoothState, BroadcastStreamRecvError>>>
    {
        let receiver = self.sender.subscribe();

        let connection = Connection::system().await?;
        let object_manager = ObjectManagerProxy::builder(&connection)
            .path("/")?
            .destination("org.bluez")?
            .build()
            .await?;

        // Set up monitoring for new devices early so we don't miss
        // any events
        let interfaces_added = object_manager.receive_interfaces_added().await?;
        let sender = self.sender.clone();
        let updater = BluetoothStateUpdater::new(sender, self.state.clone());
        tokio::spawn(async { monitor_added_interfaces(interfaces_added, updater).await });
        // let interfaces_removed = object_manager.receive_interfaces_removed().await?;

        // Figure out what the state is when we boot up so we report
        // already connected devices (or lack of an adapter)
        self.send_current_state(object_manager).await?;

        Ok(BroadcastStream::new(receiver))
    }

    async fn add_device(&mut self, device: OwnedObjectPath) {
        if self.known_devices.insert(device) {
            self.sender
                .send(BluetoothState::ConnectedDevices(vec!["00:00:00:00:00:00"]));
        }
    }

    /// Get the current Bluetooth state by explicitly calling methods
    /// to find out what BlueZ is currently managing.
    async fn send_current_state<'a>(
        &mut self,
        object_manager: ObjectManagerProxy<'a>,
    ) -> Result<()> {
        match object_manager.get_managed_objects().await {
            Ok(objects) => {
                if !objects
                    .iter()
                    .any(|(_, object)| object.contains_key("org.bluez.Adapter1"))
                {
                    self.sender.send(BluetoothState::NoAdapter);
                }

                let devices: Vec<_> = objects
                    .into_iter()
                    .filter_map(|(path, object)| {
                        if object.contains_key("org.bluez.Device1") {
                            Some(path)
                        } else {
                            None
                        }
                    })
                    .collect();

                for device in devices {
                    self.add_device(device);
                }
            }
            Err(error) => {
                error!(error:?; "Error connecting to BlueZ: {}", error);
                self.sender.send(BluetoothState::NoService);
            }
        }

        Ok(())
    }

    //     async fn monitor_state_changes(
    //         &self,
    //         mut interfaces_added: InterfacesAddedStream,
    //         mut interfaces_removed: InterfacesRemovedStream,
    //     ) {
    //         // let added = interfaces_added.filter_map(|interface| async {
    //         //     // interface.args().filter(|args| {
    //         //     //     args.interfaces_and_properties
    //         //     //         .keys()
    //         //     //         .any(|key| key == "org.bluez.Device1" || key == "org.bluez.Adapter1")
    //         //     // });
    //         // });
    //         // let removed = interfaces_removed;
    //     }
    // }
}

async fn monitor_added_interfaces(
    mut interfaces_added: InterfacesAddedStream,
    updater: BluetoothStateUpdater,
) {
    while let Some(interface) = interfaces_added.next().await {
        match interface.args() {
            Ok(args) => {
                if args
                    .interfaces_and_properties
                    .contains_key("org.bluez.Adapter1")
                {
                    updater
                        .add_adapter()
                        .await
                        .unwrap_or_else(|error| error!("{}", error));
                    continue;
                }

                let Some(device) = args.interfaces_and_properties.get("org.bluez.Device1") else {
                    continue;
                };

                if !is_device_connected(&(args.object_path.clone(), device)) {
                    continue;
                }

                let Some(address) = get_device_address(&(args.object_path.clone(), device)) else {
                    warn!(
                        "Missing device address for Symbol’s value as variable is void: {}",
                        args.object_path
                    );
                    continue;
                };

                updater
                    .add_device(address)
                    .await
                    .unwrap_or_else(|error| error!("{}", error));
            }
            Err(error) => {
                error!(error:?; "Failed to get interface args: {}", error);
            }
        };
    }
}

/// BlueZ device interface
#[proxy(default_service = "org.bluez", interface = "org.bluez.Device1")]
trait BluezDevice {
    #[zbus(property)]
    fn connected(&self) -> zbus::Result<bool>;
}

/// Get the address of a device
fn get_device_address<P, V>(device: &(P, &HashMap<&str, V>)) -> Option<MacAddress>
where
    P: From<OwnedObjectPath> + std::fmt::Display,
    V: From<zbus::zvariant::OwnedValue> + std::fmt::Debug,
    for<'a> bool: TryFrom<&'a V>,
    for<'a> &'a str: TryFrom<&'a V>,
{
    let address = device.1.get("Address")?;

    let Ok(address) = <&str>::try_from(address) else {
        error!("Invalid MAC address for device {}: {:?}", device.0, address);
        return None;
    };

    let Ok(address) = MacAddress::try_from(address.to_owned()) else {
        error!("Invalid MAC address for device {}: {:?}", device.0, address);
        return None;
    };

    Some(address)
}

/// Test if a device is connected
fn is_device_connected<P, V>(device: &(P, &HashMap<&str, V>)) -> bool
where
    P: From<OwnedObjectPath> + std::fmt::Display,
    V: From<zbus::zvariant::OwnedValue> + std::fmt::Debug,
    for<'a> bool: TryFrom<&'a V>,
{
    let Some(connected) = device.1.get("Connected") else {
        error!("Connection property missing for device {}", device.0);
        return false;
    };

    let Ok(connected) = <bool>::try_from(connected) else {
        error!(
            "Invalid connection property for device {}: {:?}",
            device.0, connected
        );
        return false;
    };

    connected
}

/// Module error type
#[derive(Debug, Error)]
pub enum BluetoothMonitorError {
    #[error(transparent)]
    DBusError(#[from] zbus::Error),
    #[error(transparent)]
    ThreadCommunicationError(#[from] tokio::sync::broadcast::error::SendError<BluetoothState>),
}
type Result<T> = std::result::Result<T, BluetoothMonitorError>;
