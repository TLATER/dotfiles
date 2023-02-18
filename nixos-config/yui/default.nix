{
  config,
  pkgs,
  lib,
  flake-inputs,
  ...
}: {
  imports = [
    flake-inputs.nixos-hardware.nixosModules.common-pc
    flake-inputs.nixos-hardware.nixosModules.common-pc-ssd
    flake-inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
    flake-inputs.nixos-hardware.nixosModules.common-gpu-nvidia-nonprime

    ./hardware-configuration.nix
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
    interfaces = {
      eno1.useDHCP = true;
      wlp6s0.useDHCP = true;
    };

    wireless.interfaces = ["wlp6s0"];

    # Allow barrier
    firewall.allowedTCPPorts = [24800];
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

  # For random android-related things
  programs.adb.enable = true;
  users.users.tlater.extraGroups = ["adbusers"];
}
