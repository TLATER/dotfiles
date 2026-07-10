/**
  Entrypoint into general-purpose configuration.
*/
{
  inputs,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    inputs.disko.nixosModules.disko
    inputs.ncro.nixosModules.ncro
    inputs.nix-flatpak.nixosModules.nix-flatpak

    ./modules/networkmanager-ensure-profiles.nix
    ./modules/udev-rules.nix
    ./modules/unfree-packages.nix

    ./boot.nix
    ./desktop.nix
    ./maintenance.nix
    ./networking.nix
    ./programs.nix
    ./security.nix
  ];

  _module.args.inputs' = lib.mapAttrs (
    _: outputs: lib.mapAttrs (_: output: output.${pkgs.stdenv.hostPlatform.system}) outputs
  ) inputs;

  hardware.facter.enable = true;

  # Remove some silly defaults.
  programs.nano.enable = false;
  environment.defaultPackages = lib.mkForce [ ];
}
