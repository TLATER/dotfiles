use std/log

def main [interface: string, action: string] {
  match $interface {
    "wgt-pvpn" => {
      match $action {
        "up" => {
          log info 'Enabling Proton DNS server'

          unbound-control -q forward_remove .
          unbound-control -q forward_add . 10.2.0.1
        }
        "down" => {
          log info 'Disabling Proton DNS server'

          unbound-control -q forward_remove .
          unbound-control -q forward_add +t . ...($env.NIXOS_DNS_SERVERS | split row _ | reverse)
        }
      }
    }

    "wgt-tlaternet" => {
      match $action {
        "up" => {
          log info 'Setting up DNS redirect to tlater.net via the VPN'

          unbound-control -q local_zone tlater.net. redirect
          unbound-control -q local_data tlater.net. A 10.45.249.1
        }

        "down" => {
          log info 'Removing DNS redirect to tlater.net'

          unbound-control -q local_zone_remove tlater.net.
        }
      }
    }
  }
}
