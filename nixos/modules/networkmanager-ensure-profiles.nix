/**
  A systemd-creds compatible replacement for the upstream
  NetworkManager-ensure-profiles service.
*/
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.networking.networkmanager;
  ini = pkgs.formats.ini { };

  profiles = lib.mapAttrs' (id: profile: {
    name = "/run/NetworkManager/system-connections/${id}.nmconnection";
    value = ini.generate (lib.escapeShellArg id) profile;
  }) cfg.ensureProfiles.profiles;
in
{
  systemd.services.NetworkManager-ensure-profiles = lib.mkIf (cfg.ensureProfiles.profiles != { }) {
    script = lib.mkForce "";

    serviceConfig.EnvironmentFile = lib.mkForce [ ];
    serviceConfig.ExecStart = pkgs.writers.writeNu "NetworkManager-ensure-profiles" { } ''
      let environment = (
        '${builtins.toJSON cfg.ensureProfiles.environmentFiles}'
        | from json
        | each { open --raw $in | from json }
        | reduce {|it acc| $acc | merge $it }
      )

      let profiles = open --raw '${pkgs.writers.writeJSON "networkmanager-profiles.json" profiles}' | from json

      mkdir /run/NetworkManager/system-connections

      for profile in ($profiles | items {|target source| [$target $source] }) {
        # Substitute all variables from the merged environment files
        let substituted_profile = (
          $environment
          | items {|name value| [$name $value] }
          | reduce --fold (open --raw $profile.1) {|variable profile| $profile | str replace --all $'$($variable.0)' $variable.1 }
        )

        $substituted_profile | save -f $profile.0
      }

      ${lib.getExe' pkgs.networkmanager "nmcli"} connection reload
    '';
  };
}
