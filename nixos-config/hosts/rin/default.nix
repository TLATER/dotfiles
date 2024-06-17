{ pkgs, flake-inputs, ... }:
{
  imports = [
    flake-inputs.disko.nixosModules.disko
    ../../networking/work.nix

    ./hardware-configuration.nix
    ./disko.nix

    ./firefox.nix
    ./hardware-policy.nix

    flake-inputs.nixos-hardware.nixosModules.common-pc-laptop
    flake-inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    flake-inputs.nixos-hardware.nixosModules.common-cpu-amd
    flake-inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
    flake-inputs.nixos-hardware.nixosModules.common-cpu-amd-zenpower
  ];

  home-manager.users.tlater = import "${flake-inputs.self}/home-config/hosts/rin.nix";

  sops.age.keyFile = "/var/lib/sops/host.age";

  networking = {
    hostName = "rin";
    hostId = "e6aaf496";
  };

  # Used extensively for testing at work
  virtualisation.docker.enable = true;
  users.users.tlater.extraGroups = [ "docker" ];
  # Install rustup system-wide for convenience
  environment.systemPackages = [ pkgs.rustup ];
}
