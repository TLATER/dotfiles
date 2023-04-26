{
  config,
  pkgs,
  lib,
  flake-inputs,
  ...
}: let
  inherit (lib.strings) concatStringsSep;
in {
  imports = [
    flake-inputs.nixos-hardware.nixosModules.common-pc
    flake-inputs.nixos-hardware.nixosModules.common-pc-ssd
    flake-inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
    flake-inputs.nixos-hardware.nixosModules.common-gpu-nvidia-nonprime

    ./hardware-configuration.nix
    ../networks/personal.nix
  ];

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (pkgs.lib.getName pkg) [
      # Required to get the steam controller to work (i.e., for hardware.steam-hardware)
      "steam-original"
      "nvidia-x11"
      "nvidia-settings"
      "nvidia-persistenced"
    ];

  home-manager.users.tlater = import "${flake-inputs.self}/home-config/hosts/personal-desktop.nix";

  boot = {
    blacklistedKernelModules = [
      # Used for IPMI (remote maintenance thing), but is unsupported
      # by motherboard.
      "sp5100_tco"
    ];

    kernelPackages = lib.mkForce flake-inputs.nixpkgs-unfree.legacyPackages.${pkgs.system}.linuxPackages_latest;

    # Star citizen needs more
    kernel.sysctl."vm.max_map_count" = 16777216;

    initrd.luks.devices = let
      ssdOptimization = {
        allowDiscards = true;
        bypassWorkqueues = true;
      };
    in {
      root =
        {
          device = "/dev/disk/by-uuid/3c0d48f6-f051-4328-9919-677a7fcddae7";
        }
        // ssdOptimization;
      storage =
        {
          device = "/dev/disk/by-uuid/dd17e735-fac4-467f-b1ee-8bb214bc2b08";
        }
        // ssdOptimization;
    };
  };

  networking = {
    hostName = "yui";
    wireless.interfaces = ["wlp6s0"];

    firewall.allowedTCPPorts = [
      # Allow barrier
      24800
      # Allow minecraft for when I'm running a minecraft server
      # locally
      25565
    ];

    firewall.allowedUDPPorts = [
      25565
    ];

    # Work around EAC
    hosts."127.0.0.1" = ["modules-cdn.eac-prod.on.epicgames.com"];
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

      "40-eno1" = {
        matchConfig.Name = "eno1";
        networkConfig = {
          Bond = "bond0";
          PrimarySlave = true;
        };
      };

      "40-wlp6s0" = {
        matchConfig.Name = "wlp6s0";
        networkConfig.Bond = "bond0";
      };
    };
  };

  hardware = {
    nvidia = {
      modesetting.enable = true;
      # Power management is required to get nvidia GPUs to behave on
      # suspend, due to firmware bugs. Aren't nvidia great?
      powerManagement.enable = true;
    };
    steam-hardware.enable = true;
  };

  services.joycond.enable = true;

  # For unruly applications that can't handle being run under tiling
  # window managers.
  services.xserver.windowManager.evilwm.enable = true;

  environment.variables = {
    # Necessary to correctly enable va-api (video codec hardware
    # acceleration). If this isn't set, the libvdpau backend will be
    # picked, and that one doesn't work with most things, including
    # Firefox.
    LIBVA_DRIVER_NAME = "nvidia";
    # Required to run the correct GBM backend for nvidia GPUs on wayland
    GBM_BACKEND = "nvidia-drm";
    # Apparently, without this nouveau may attempt to be used instead
    # (despite it being blacklisted)
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    # Hardware cursors are currently broken on nvidia
    WLR_NO_HARDWARE_CURSORS = "1";

    # Required to use va-api it in Firefox. See
    # https://github.com/elFarto/nvidia-vaapi-driver/issues/96
    MOZ_DISABLE_RDD_SANDBOX = "1";
  };

  sops.secrets = {
    "peerix/yui" = {
      owner = config.users.users.peerix.name;
      group = config.users.users.peerix.group;
    };
  };

  services.peerix = {
    privateKeyFile = config.sops.secrets."peerix/yui".path;
    publicKeyFile = ../../keys/peerix/yui.pub;
  };

  # Fix broken suspend on b550i motherboard
  #
  # The rule is a bit overzealous, as it disables wake from *either*
  # NVME drive, but I don't see why anyone would want to wake from
  # NVME drives anyway.
  #
  # At least I *think* that's what the GPP bridge maps to, at least
  # this fixes the immediate resume from suspend on my board.
  services.udev.extraRules = concatStringsSep ", " [
    ''ACTION=="add"''
    ''SUBSYSTEM=="pci"''
    ''ATTR{vendor}=="0x1022"''
    ''ATTR{device}=="0x1483"''
    ''ATTR{power/wakeup}="disabled"''
  ];

  # For random android-related things
  programs.adb.enable = true;
  # TODO(tlater): Swap out with whatever Android 14 introduces
  programs.droidcam.enable = true;
  users.users.tlater.extraGroups = ["adbusers"];
}
