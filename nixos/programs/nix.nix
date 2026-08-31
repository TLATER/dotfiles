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
      allowed-users = [ "@wheel" ];

      auto-optimise-store = true;
      experimental-features = [
        "nix-command"
        "flakes"
      ];

      secret-key-files = [ "/run/credentials/nix-daemon.service/nix-signing-key" ];

      substituters = lib.mkForce [ "http://${config.services.ncro.settings.server.listen}" ];
    };

    gc = {
      automatic = true;
      dates = "Thu";
    };
  };

  services = {
    # Fix nix' stupid cache resolution
    ncro = {
      enable = true;
      settings = {
        server.listen = "127.0.0.1:8080";
        cache.ttl = "1d";

        logging = {
          timestamps = false;
          format = "text";
        };

        upstreams = [
          {
            url = "https://cache.nixos.org";
            priority = 10;
            # The key for cache.nixos.org is provided by nixpkgs, so
            # we don't add it manually.
          }
          {
            url = "https://cache.nixos-cuda.org";
            priority = 20;
            filters = [
              {
                action = "allow";
                field = "name";
                pattern = "nvidia-x11";
              }
              {
                action = "allow";
                field = "name";
                pattern = "nvidia-settings";
              }
              {
                action = "allow";
                field = "name";
                pattern = "cuda-merged";
              }
              {
                action = "allow";
                field = "name";
                pattern = "libnpp";
              }
              {
                action = "allow";
                field = "name";
                pattern = "cuda_*";
              }
              {
                action = "allow";
                field = "name";
                pattern = "libcu_*";
              }
              {
                action = "allow";
                field = "name";
                pattern = "libnv_*";
              }
              {
                action = "allow";
                field = "name";
                pattern = "libnv_*";
              }
            ];

            public_key = "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M=";
          }
        ];
      };
    };

    # Regularly clean up gcroots
    angrr = {
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
