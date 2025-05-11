def get-devices [] {
  (dbus call --system --dest=org.bluez / org.freedesktop.DBus.ObjectManager GetManagedObjects |
     values |
     filter {|dev| "org.bluez.Device1" in ($dev | columns)} |
     get "org.bluez.Device1")
}

get-devices | get Icon

# TODO(tlater): Systemd 257 will get `busctl wait` which will let us
# wait for updates
