/**
  Configuration related to nix.
*/
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

  # Regularly clean up gcroots
  services.angrr = {
    enable = true;

    settings = {
      temporary-root-policies = {
        direnv = {
          path-regex = "/\\.direnv/";
          period = "14d";
        };

        result = {
          path-regex = "/result[^/]*$";
          period = "3d";
        };
      };

      profile-policies = {
        system = {
          profile-paths = [ "/nix/var/nix/profiles/system" ];
          keep-since = "14d";
          keep-latest-n = 5;
          keep-booted-system = true;
          keep-current-system = true;
        };

        user = {
          profile-paths = [
            "~/.local/state/nix/profiles/profile"
            "/nix/var/nix/profiles/per-user/root/profile"
          ];
          keep-since = "1d";
          keep-latest-n = 1;
        };
      };
    };
  };

  systemd.services = {
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
}
