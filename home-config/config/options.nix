{lib, ...}: let
  inherit (lib) mkEnableOption;
in {
  options.custom = {
    desktop-environment = mkEnableOption "Set up a stumpwm-based desktop environment";
    download-manager = mkEnableOption "Configure an external download manager";
    graphical-applications = mkEnableOption "Configure applications that involve a GUI";
    has-yubikey = mkEnableOption "Set up settings that require my yubikey to work";
    is-work = mkEnableOption "Set up settings for work computers";
    software-kvm = mkEnableOption "Set up software kvm";
    is-nixos = mkEnableOption "Whether this is a NixOS configuration";
  };
}
