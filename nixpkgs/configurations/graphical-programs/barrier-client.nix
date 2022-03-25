{pkgs, ...}: {
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
}
