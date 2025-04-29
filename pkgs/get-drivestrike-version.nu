#!/usr/bin/env nu

let scratch_dir = (mktemp --directory nvchecker.XXXXXX)
let config = $scratch_dir + '/config.toml'

{
  drivestrike: {
    source: "apt"
    pkg: "drivestrike"
    mirror: "https://app.drivestrike.com/static/apt/"
    suite: "stretch"
  }
} | to toml | save --force $config

let version = (nvchecker --logger json --file $config |
  from json --objects |
  where event == updated and name == drivestrike |
  get 0.version)

rm -r $scratch_dir

print $version
