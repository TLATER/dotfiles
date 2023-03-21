{
  config,
  lib,
  pkgs,
  ...
}: let
  screen-config = ''
    section: screens
    	yui:
    		halfDuplexCapsLock = false
    		halfDuplexNumLock = false
    		halfDuplexScrollLock = false
    		xtestIsXineramaUnaware = false
    		preserveFocus = false
    		switchCorners = none
    		switchCornerSize = 0
    end

    section: aliases
    end

    section: links
    end

    section: options
    	relativeMouseMoves = false
    	screenSaverSync = true
    	win32KeepForeground = false
    	clipboardSharing = true
    	switchCorners = none
    	switchCornerSize = 0
    	keystroke(Alt+Left) = switchInDirection(left)
    	keystroke(Alt+Right) = switchInDirection(right)
    end
  '';
in {
  config = lib.mkIf config.custom.software-kvm {
    systemd.user.services."barrier-client@" = {
      Unit = {
        Description = "Barrier: Open-source KVM software";
        Documentation = "https://github.com/debauchee/barrier/wiki";
        BindsTo = ["graphical-session.target"];
        After = ["graphical-session.target"];
      };

      Service = {
        ExecStart = builtins.concatStringsSep " " [
          "${pkgs.barrier}/bin/barrierc"
          "--enable-crypto"

          # These are best handled by systemd
          "--no-daemon"
          "--no-restart"
          "%i"
        ];
        Restart = "always";
      };
    };

    systemd.user.services.barrier-server = {
      Unit = {
        Description = "Barrier: Open-source KVM software";
        Documentation = "https://github.com/debauchee/barrier/wiki";
        BindsTo = ["graphical-session.target"];
        After = ["graphical-session.target"];
      };

      Service = {
        ExecStart = builtins.concatStringsSep " " [
          "${pkgs.barrier}/bin/barriers"
          "-c ${pkgs.writeText "barrier-screen.cfg" screen-config}"
          "--address 0.0.0.0:24800"
          "--enable-crypto"

          # These are best handled by systemd
          "--no-daemon"
          "--no-restart"
        ];
        Restart = "on-failure";
      };
    };
  };
}
