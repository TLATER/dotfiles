{
  pkgs,
  lib,
  flake-inputs,
  ...
}:
let
  inherit (lib.strings) concatStringsSep;
in
{
  imports = [
    flake-inputs.disko.nixosModules.disko

    flake-inputs.nixos-hardware.nixosModules.common-pc
    flake-inputs.nixos-hardware.nixosModules.common-pc-ssd
    flake-inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate

    ../../networking/personal.nix

    ./hardware-configuration.nix
    ./disko.nix

    ./games.nix
    ./wireguard.nix
    ./networking.nix
  ];

  nixpkgs.config.allowUnfreePredicate =
    pkg:
    builtins.elem (pkgs.lib.getName pkg) [
      "steam"
      "steam-run"
      # Required to get the steam controller to work (i.e., for hardware.steam-hardware)
      "steam-original"
      "steam-unwrapped"
      "nvidia-x11"
      "nvidia-settings"
    ];

  home-manager.users.tlater = import "${flake-inputs.self}/home-config/hosts/yui.nix";

  sops.gnupg = {
    home = "/var/lib/sops";
    sshKeyPaths = [ ];
  };

  easyNvidia = {
    enable = true;
    withIntegratedGPU = false;
    vaapi.firefox.av1Support = true;
  };

  boot = {
    blacklistedKernelModules = [
      # Used for IPMI (remote maintenance thing), but is unsupported
      # by motherboard.
      "sp5100_tco"
    ];
  };
  services = {
    btrfs.autoScrub.enable = true;

    # Fix broken suspend on b550i motherboard
    #
    # The rule is a bit overzealous, as it disables wake from *either*
    # NVME drive, but I don't see why anyone would want to wake from
    # NVME drives anyway.
    #
    # At least I *think* that's what the GPP bridge maps to, at least
    # this fixes the immediate resume from suspend on my board.
    udev.extraRules = concatStringsSep ", " [
      ''ACTION=="add"''
      ''SUBSYSTEM=="pci"''
      ''ATTR{vendor}=="0x1022"''
      ''ATTR{device}=="0x1483"''
      ''ATTR{power/wakeup}="disabled"''
    ];
  };

  # For random android-related things
  programs.adb.enable = true;
  users.users.tlater.extraGroups = [ "adbusers" ];
}
