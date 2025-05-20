{
  lib,
  pkgs,
  flake-inputs,
  ...
}:
let
  inherit (flake-inputs.self.packages.${pkgs.system}) nextcloudcmd;
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
        builtins.toString (
          pkgs.writeShellApplication {
            name = "sync-keepassxc";
            runtimeInputs = with pkgs; [
              coreutils
              gnugrep
              libsecret
              nextcloudcmd
            ];

            text = ''
              (secret-tool search URL 'https://nextcloud.tlater.net' 2>&1 \
                | grep UserName \
                | cut -d' ' -f3; \
               secret-tool lookup URL 'https://nextcloud.tlater.net') | \
               nextcloudcmd \
               --path Backups/keepass \
               "$HOME/.local/share/keepassxc/synced/" \
               'https://nextcloud.tlater.net'
            '';
          }
        )
        + "/bin/sync-keepassxc";
    };
  };

  systemd.user.timers.keepass-sync = {
    Unit.Description = "Periodic KeepassXC synchronization";
    Timer.OnCalendar = "hourly";
    Install.WantedBy = [ "timers.target" ];
  };
}
