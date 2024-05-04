{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) concatStringsSep;
  inherit (pkgs) runtimeShell writeScript;
  inherit (config.xdg) configHome dataHome;
  aria2-bin = "${pkgs.aria2}/bin/aria2c";
  coreutils-bin = "${pkgs.coreutils}/bin";
  sessionFile = "${dataHome}/aria2/session";
in
{
  programs.aria2 = {
    enable = true;
    settings = {
      # Download directory
      dir = "${config.xdg.userDirs.download}/aria";
      check-integrity = true;

      ## General optimization
      # Don't download files if they're already in the download directory
      conditional-get = true;
      file-allocation = "falloc"; # Assume ext4, this is faster there
      optimize-concurrent-downloads = true;
      disk-cache = "512M"; # In-memory cache to avoid fragmentation

      ## Torrent options
      bt-force-encryption = true;
      bt-detach-seed-only = true; # Don't block downloads when seeding
      seed-ratio = 2;
      seed-time = 60;
    };
  };

  systemd.user.services.aria2 = {
    Unit.Description = "aria2 download manager";
    Service = {
      ExecStartPre =
        let
          prestart = writeScript "aria2-prestart" ''
            #!${runtimeShell}
            ${coreutils-bin}/mkdir -p ${dataHome}/aria2

            if [ ! -e "${sessionFile}" ]; then
                ${coreutils-bin}/touch ${sessionFile}
            fi
          '';
        in
        "${prestart}";

      ExecStart = concatStringsSep " " [
        "${aria2-bin}"
        "--enable-rpc"
        "--conf-path=${configHome}/aria2/aria2.conf"
        "--save-session=${sessionFile}"
        "--save-session-interval=1800"
        "--input-file=${sessionFile}"
      ];

      ExecReload = "${coreutils-bin}/kill -HUP $MAINPID";

      # We don't want to class an exit before downloads finish as a
      # failure if we stop aria2c, since the entire point of it is
      # that it will resume the downloads.
      SuccessExitStatus = "7";

      # We use falloc, so if we use this unit on any other fs it will
      # cause issues
      Slice = "session.slice";
      ProtectSystem = "full";
    };

    Install.WantedBy = [ "graphical-session.target" ];
  };
}
