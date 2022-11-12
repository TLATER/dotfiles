{
  pkgs,
  lib,
  flake-inputs,
  ...
}: {
  imports = [./hardware-configuration.nix ../bluetooth.nix ../wifi.nix];

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

    initrd = {
      availableKernelModules = ["hid_roccat_ryos"];
      luks.devices = {
        root.device = "/dev/disk/by-uuid/3c0d48f6-f051-4328-9919-677a7fcddae7";
        storage = {
          allowDiscards = true;
          device = "/dev/disk/by-uuid/dd17e735-fac4-467f-b1ee-8bb214bc2b08";
        };
      };
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

  services.xserver.videoDrivers = ["nvidia"];

  hardware = {
    nvidia.modesetting.enable = true;
    steam-hardware.enable = true;
    cpu.amd.updateMicrocode = true;
  };

  # Necessary to correctly enable va-api (video codec hardware
  # acceleration)
  environment.variables.LIBVA_DRIVER_NAME = "nvidia";

  # For random android-related things
  programs.adb.enable = true;
  users.users.tlater.extraGroups = ["adbusers"];
}
