const SERVICE = {
  name: net.tlater.DesktopLogic
  path: /net/tlater/desktoplogic
}

const IDLE_INTERFACE = {
  name: $"($SERVICE.name).IdleInhibitor"
  path: ([$SERVICE.path "idleinhibitor"] | path join)
}

def "main monitor-notifications" [] {
  # We only get notifications for things that happen while we're
  # listening, so we fake a notification on startup
  let inhibiting = (
    busctl -j --user get-property
    $SERVICE.name $IDLE_INTERFACE.path $IDLE_INTERFACE.name IsInhibiting
    | from json
    | $in.data
  )

  print ({"payload": {"data": ["" {"IsInhibiting": {"data": $inhibiting}}]}} | to json --raw)

  let filter = {
    type: signal
    sender: $SERVICE.name
    path: $IDLE_INTERFACE.path
    interface: org.freedesktop.DBus.Properties
    member: PropertiesChanged
  } | items {|key, val| $"($key)='($val)'" } | str join ","

  busctl -j --user monitor $'--match=($filter)'
}

def "main toggle-inhibit" [] {
  busctl --user call $SERVICE.name $IDLE_INTERFACE.path $IDLE_INTERFACE.name ToggleInhibit
}

def main [] { }
