use std/log

def main [] {
  log info "???"
}

def "main relax-dns" [] {
  let active_connection = (nmcli c show --active
    | parse --regex '^(?P<name>.+?)\s+(?P<uuid>.+?)\s+(?P<type>.+?)\s+(?P<device>.+?)$'
    | where type in ["wifi", "ethernet"]
    | get --optional 0)

  if ($active_connection == null) {
    log error 'No active connections'
    exit 1
  }

  # TODO(tlater): Figure out if the string split is correct
  let dhcp_4_dns = (nmcli -f 'DHCP4.OPTION' c show $active_connection.name
    | parse --regex 'DHCP4\.OPTION\[.+\]:\s+(?P<attribute>.+) = (?P<value>.*)'
    | transpose --header-row
    | $in.domain_name_servers
    | split row ',')
  let dhcp_6_dns = (nmcli -f 'DHCP6.OPTION' c show $active_connection.name
    | parse --regex 'DHCP4\.OPTION\[.+\]:\s+(?P<attribute>.+) = (?P<value>.*)'
    | transpose --header-row
    | $in.domain_name_servers
    | split row ',')

  let nameservers = $dhcp_4_dns ++ $dhcp_6_dns

  if ($nameservers | is-empty) {
    log error 'DHCP reported no nameservers'
    exit 1
  }

  unbound-control -q forward_remove .

  for server in $nameservers {
    unbound-control -q forward_add . $server
  }
}
