/**
  Entrypoint into general-purpose configuration.
*/
{ inputs, lib, ... }: {
  imports = [
    inputs.disko.nixosModules.disko
    inputs.nix-flatpak.nixosModules.nix-flatpak

    ./modules/networkmanager-ensure-profiles.nix
    ./modules/udev-rules.nix

    ./boot.nix
    ./desktop.nix
    ./maintenance.nix
    ./networking.nix
    ./programs.nix
    ./security.nix
  ];

  nixpkgs.config.allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "nvidia-x11"
      "nvidia-settings"
    ];

  # Remove some silly defaults.
  programs.nano.enable = false;
  environment.defaultPackages = lib.mkForce [ ];
}
