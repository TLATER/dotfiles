{
  lib,
  pkgs,
  flake-inputs,
  ...
}:
let
  inherit (flake-inputs.self.packages.${pkgs.system}) nextcloudcmd;
  inherit (flake-inputs.self.pkgs-lib.${pkgs.system}) writeNuWith;
in
{
  home.packages = with pkgs; [ keepassxc ];

  systemd.user.services.keepass-sync = {
    Unit = {
      Description = "KeepassXC synchronization";
      ConditionEnvironment = [ "DBUS_SESSION_BUS_ADDRESS" ];
    };
    Service = {
      Type = "oneshot";

      ExecCondition = "${lib.getExe' pkgs.systemd "busctl"} --user status org.freedesktop.secrets";

      ExecStart =
        writeNuWith
          {
            packages = [
              nextcloudcmd
              pkgs.libsecret
            ];
          }
          "sync-keepassxc"
          ''
            const url = 'https://nextcloud.tlater.net'
            const nextcloud_dir = 'Backups/keepass'
            let local_dir = $'($env.XDG_DATA_HOME | default ~/.local/share)/keepassxc/synced'

            let attributes = secret-tool search URL $url o+e>| parse "{attribute} = {value}" | transpose -rid
            let password = secret-tool lookup URL $url

            $"($attributes.'attribute.UserName')\n($password)" | nextcloudcmd --path $nextcloud_dir $local_dir $url
          '';
    };
  };

  systemd.user.timers.keepass-sync = {
    Unit.Description = "Periodic KeepassXC synchronization";
    Timer.OnCalendar = "hourly";
    Install.WantedBy = [ "timers.target" ];
  };
}
