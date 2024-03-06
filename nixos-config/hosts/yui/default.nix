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

    ./games.nix
    ./hardware-configuration.nix
    ../../networking/personal.nix
    ./wireguard.nix
    ../../wireguard.nix
    ./nvidia
    ./networking.nix
  ];

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (pkgs.lib.getName pkg) [
      "steam"
      "steam-run"
      # Required to get the steam controller to work (i.e., for hardware.steam-hardware)
      "steam-original"
      "nvidia-x11"
      "nvidia-settings"
      "nvidia-persistenced"
    ];

  home-manager.users.tlater = import "${flake-inputs.self}/home-config/hosts/yui.nix";

  sops.gnupg = {
    home = "/var/lib/sops";
    sshKeyPaths = [];
  };

  boot = {
    blacklistedKernelModules = [
      # Used for IPMI (remote maintenance thing), but is unsupported
      # by motherboard.
      "sp5100_tco"
    ];

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

  fileSystems."/nix".options = ["defaults" "noatime"];

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
  users.users.tlater.extraGroups = ["adbusers"];
}
