{pkgs, ...}: let
  screen-config = ''
    section: screens
    	ct-lt-02052:
    		halfDuplexCapsLock = false
    		halfDuplexNumLock = false
    		halfDuplexScrollLock = false
    		xtestIsXineramaUnaware = false
    		preserveFocus = false
    		switchCorners = none +top-left +bottom-left
    		switchCornerSize = 5
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
    	ct-lt-02052:
    		right = yui
    	yui:
    		left = ct-lt-02052
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
}
