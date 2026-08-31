use std/log

def main [config: path] {
  let options = open $config

  let address = (
    open --raw $'($env.XDG_STATE_HOME? | default "~/.local/state")/syncthing/config.xml'
    | query xml 'string(configuration/gui/address)'
  )

  let api_key = (
    open --raw $'($env.XDG_STATE_HOME? | default "~/.local/state")/syncthing/config.xml'
    | query xml 'string(configuration/gui/apikey)'
  )

  # Wait for syncthing to be fully online
  mut online = false

  while not $online {
    try {
      http get $'http://($address)'
      $online = true
    }

    sleep 5sec
    log warning "Syncthing is a bit slow to start, retrying..."
  }

  def request [method: string path: string] {
    let body = $in

    match $method {
      # This ridiculous construct is required because apparently
      # subcommand choices can't be made with variables
      delete => ($body | http delete --headers {X-API-Key: $api_key} $'http://($address)($path)')
      get => ($body | http get --headers {X-API-Key: $api_key} $'http://($address)($path)')
      head => ($body | http head --headers {X-API-Key: $api_key} $'http://($address)($path)')
      options => ($body | http options --headers {X-API-Key: $api_key} $'http://($address)($path)')
      patch => ($body | http patch --headers {X-API-Key: $api_key} $'http://($address)($path)')
      post => ($body | http post --headers {X-API-Key: $api_key} $'http://($address)($path)')
      put => ($body | http put --headers {X-API-Key: $api_key} $'http://($address)($path)')
    }
  }

  # Write the settings
  $options | to json | request put /rest/config/options

  if (request get /rest/config/restart-required | $in.requiresRestart) {
    log info "Configuration changes require a restart; restarting"
    null | to json | request post /rest/system/restart
  }
}
