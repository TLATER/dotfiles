{
  lib,
  flake-inputs,
  ...
}: {
  imports = [
    flake-inputs.impermanence.nixosModules.impermanence
    flake-inputs.disko.nixosModules.disko

    ./hardware-configuration.nix
    ./nixos-hardware-precursor.nix
    ./disko.nix
    ../networks/personal.nix
  ];

  home-manager.users.tlater = import "${flake-inputs.self}/home-config/hosts/personal-desktop.nix";

  # In the future, set up a roolback in early init:
  # https://discourse.nixos.org/t/impermanence-vs-systemd-initrd-w-tpm-unlocking/25167
  boot.initrd.systemd.enable = true;

  fileSystems = {
    "/persist/data".neededForBoot = true;
    "/persist/state".neededForBoot = true;
  };

  # /etc/sops/ won't be available during early boot because
  # impermanence runs after the sops secrets setup.
  #
  # Hence, access it directly from the persistent volume.
  # sops.defaultSopsFile = lib.mkForce "/persist/state/etc/sops/secrets.yaml";

  environment.persistence = {
    "/persist/state" = {
      hideMounts = true;
      directories = [
        "/var/log"
        "/var/lib/alsa"
        "/var/lib/blueman"
        "/var/lib/bluetooth"
        "/var/lib/chrony"
        "/var/lib/flatpak"
        "/var/lib/fwupd"
        "/var/lib/nixos"
        # "/var/lib/sops"
        "/var/lib/systemd/coredump"
        # {
        #   directory = "/etc/sops";
        #   mode = "0700";
        # }
      ];
      files = [
        "/etc/machine-id"
        # "/etc/passwd"
        # "/etc/shadow"
      ];

      users.tlater = {
        directories = [
          ".local/src"
          ".local/state"
          ".config"
          ".cache"
          ".mozilla/firefox"
          ".pki"
          {
            directory = ".gnupg";
            mode = "0700";
          }
          {
            directory = ".ssh";
            mode = "0700";
          }
        ];

        files = [
          ".local/share/recently-used.xbel"
          ".local/share/zsh_history"
        ];
      };
    };

    "/persist/data" = {
      users.tlater.directories = [
        "Documents"
        "Downloads"
        "Scratch"
        ".local/share"
        {
          directory = ".var";
          mode = "0700";
        }
      ];
    };
  };

  networking = {
    hostName = "ren";
    hostId = "0d418d09";
    wireless.interfaces = ["wlp2s0"];
  };

  systemd.network = {
    netdevs = {
      "10-bond0" = {
        netdevConfig = {
          Name = "bond0";
          Kind = "bond";
        };

        bondConfig = {
          Mode = "active-backup";
          PrimaryReselectPolicy = "always";
          MIIMonitorSec = "100ms";
        };
      };
    };

    networks = {
      "10-bond0" = {
        matchConfig.Name = "bond0";
        networkConfig.DHCP = "yes";
      };

      "40-enp3s0" = {
        matchConfig.Name = "enp3s0";
        networkConfig = {
          Bond = "bond0";
          PrimarySlave = true;
        };
      };

      "40-wlp2s0" = {
        matchConfig.Name = "wlp2s0";
        networkConfig.Bond = "bond0";
      };
    };
  };

  programs.hyprland.xwayland.hidpi = true;

  # Automatic brightness/gamma adjustment
  #
  # Geolocation is used for gamma adjustment, permissions are quite
  # good so let's not block it.
  location.provider = "geoclue2";
  services.clight = {
    enable = true;
    settings = {
      resumedelay = 30;
      sensor.devname = "iio:device0";
    };
  };
}
