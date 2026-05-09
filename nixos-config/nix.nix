{
  config,
  lib,
  pkgs,
  ...
}:
{
  nix = {
    package = pkgs.lixPackageSets.stable.lix;

    settings = {
      auto-optimise-store = true;
      experimental-features = [
        "nix-command"
        "flakes"
      ];

      secret-key-files = [ "/run/credentials/nix-daemon.service/nix-signing-key" ];
    };

    gc = {
      automatic = true;
      dates = "Thu";
    };
  };

  systemd = {
    tmpfiles.settings."10-credstore" = {
      "/etc/credstore".d = {
        user = "root";
        group = "root";
        mode = "0700";
      };

      "/etc/credstore.encrypted".d = {
        user = "root";
        group = "root";
        mode = "0700";
      };
    };

    services = {
      nix-daemon = {
        after = [ "generate-nix-signing-key.service" ];
        requires = [ "generate-nix-signing-key.service" ];
        serviceConfig.LoadCredentialEncrypted = [ "nix-signing-key" ];
      };

      generate-nix-signing-key = {
        after = [ "systemd-random-seed.service" ];

        serviceConfig = {
          Type = "oneshot";
          RuntimeDirectory = "generate-nix-signing-key";

          ExecStart =
            pkgs.writers.writeNu "generate-nix-signing-key"
              {
                makeWrapperArgs = [
                  "--prefix"
                  "PATH"
                  ":"
                  (lib.makeBinPath [
                    config.nix.package
                    config.systemd.package
                  ])
                ];
              }
              ''
                cd $env.RUNTIME_DIRECTORY

                if not ('/etc/credstore.encrypted/nix-signing-key' | path exists) {
                  nix-store --generate-binary-cache-key (uname).nodename key.private key.public
                  systemd-creds encrypt key.private /etc/credstore.encrypted/nix-signing-key
                  mv key.public /etc/credstore/nix-signing-cert

                  print 'done'
                } else {
                  print 'nothing to be done'
                }
              '';
        };
      };
    };
  };
}
