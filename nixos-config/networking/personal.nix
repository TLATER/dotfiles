{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.networking.networkmanager;
  ini = pkgs.formats.ini { };
in
{
  networking.networkmanager.ensureProfiles.profiles = {
    bond = {
      connection = {
        id = "bond";
        type = "bond";
        interface-name = "bond0";
      };

      bond = {
        miimon = 100;
        mode = "active-backup";
        primary_reselect = "always";
        fail_over_mac = "active";
        updelay = 200;
      };

      ipv4.method = "auto";
      ipv6 = {
        addr-gen-mode = "default";
        method = "auto";
      };
    };

    ethernet.connection = {
      id = "ethernet";
      type = "ethernet";

      controller = "bond0";
      port-type = "bond";
    };

    lala = {
      connection = {
        id = "lala";
        type = "wifi";

        controller = "bond0";
        port-type = "bond";
      };

      wifi = {
        mode = "infrastructure";
        ssid = "lala";
      };

      wifi-security = {
        key-mgmt = "sae";
        psk = "%psk-lala%";
      };
    };
  };

  systemd.services.NetworkManager-ensure-profiles.serviceConfig =
    let
      profiles = lib.mapAttrs (name: ini.generate (lib.escapeShellArg name)) cfg.ensureProfiles.profiles;
    in
    {
      LoadCredentialEncrypted = [ "personal-wifi-passwords" ];

      # Since systemd doesn't support reading `EnvironmentFile` from a
      # cred, we replace the full thing with our own code.
      ExecStart = lib.mkForce (
        pkgs.writers.writeNu "ensure-profiles"
          {
            makeWrapperArgs = [
              "--prefix"
              "PATH"
              ":"
              "${lib.makeBinPath [ cfg.package ]}"
            ];
          }
          ''
            mkdir /run/NetworkManager/system-connections

            let profiles = '${builtins.toJSON profiles}' | from json
            let passwords = open --raw $'($env.CREDENTIALS_DIRECTORY)/personal-wifi-passwords' | from json

            (
              $profiles | items {|name template|
                $passwords | items {|password substitute|
                  (
                    open --raw $template
                    | str replace --all $'%($password)%' $substitute
                    | save -f $'/run/NetworkManager/system-connections/($name).nmconnection'
                  )
                }
              }
            )

            nmcli connection reload
          ''
      );
    };

  system.preSwitchChecks.ensureWifiPasswordsFileExists = ''
    # This isn't actually a mistake here, but in the way the file gets
    # stitched together.
    #
    # shellcheck disable=SC2234
    test -e /etc/credstore.encrypted/personal-wifi-passwords
  '';
}
