/**
  Entrypoint for yui.
*/
{ inputs, ... }: {
  imports = [
    # flake-inputs.nixos-hardware.nixosModules.common-pc
    # flake-inputs.nixos-hardware.nixosModules.common-pc-ssd
    # flake-inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate

    ../.
    ../games.nix
    ../hardware/bluetooth.nix
    ../hardware/graphics-tablet.nix
    ../hardware/nvidia.nix
    ../hardware/sound.nix
    ../hardware/yubikey.nix

    ../networking/tailscale.nix

    ./yui/disko.nix
    ./yui/hardware.nix
    ./yui/programs.nix
    ./yui/tailscale.nix
  ];

  # nixpkgs.config.allowUnfreePredicate =
  #   pkg:
  #   (builtins.elem (lib.getName pkg) [
  #     "steam"
  #     "steam-run"
  #     # Required to get the steam controller to work (i.e., for hardware.steam-hardware)
  #     "steam-original"
  #     "steam-unwrapped"
  #     "nvidia-x11"
  #     "obsidian"
  #     "cuda-merged"
  #     "libnpp"
  #   ])
  #   || (lib.strings.hasPrefix "cuda_" (lib.getName pkg))
  #   || (lib.strings.hasPrefix "libcu" (lib.getName pkg))
  #   || (lib.strings.hasPrefix "libnv" (lib.getName pkg));


  # easyNvidia = {
  #   vaapi = {
  #     enable = true;
  #     firefox.av1Support = true;
  #   };
  # };

  networking.hostName = "yui";

  home-manager.users.tlater = import "${inputs.self}/home-config/hosts/yui.nix";
  hardware.facter.reportPaht = ./yui/facter.json;

  # For random android-related things
  programs.adb.enable = true;
  users.users.tlater.extraGroups = [ "adbusers" ];

  system.stateVersion = "20.09";
}
