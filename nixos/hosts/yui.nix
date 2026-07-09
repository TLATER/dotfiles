/**
  Entrypoint for yui.
*/
{ inputs, ... }:
{
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
    ./yui/sunshine.nix
    # ./yui/tailscale.nix
  ];

  unfree.allowUnfreePackages = [
    "nvidia-x11"
    "nvidia-settings"
    "obsidian"
    "steam"
    "steam-run"
    "steam-original"
    "steam-unwrapped"
  ];

  # easyNvidia = {
  #   vaapi = {
  #     enable = true;
  #     firefox.av1Support = true;
  #   };
  # };

  networking.hostName = "yui";

  home-manager.users.tlater = import "${inputs.self}/home-config/hosts/yui.nix";
  hardware.facter.reportPath = ./yui/facter.json;

  # For random android-related things
  users.users.tlater.extraGroups = [ "adbusers" ];

  system.stateVersion = "20.09";
}
