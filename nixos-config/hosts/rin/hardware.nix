{ flake-inputs, pkgs, ... }:
{
  imports = [
    flake-inputs.disko.nixosModules.disko

    ./hardware-configuration.nix
    ./disko.nix

    flake-inputs.nixos-hardware.nixosModules.common-pc-laptop
    flake-inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    flake-inputs.nixos-hardware.nixosModules.common-cpu-amd
    flake-inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
    flake-inputs.nixos-hardware.nixosModules.common-cpu-amd-zenpower
  ];

  networking = {
    hostName = "rin";
    hostId = "e6aaf496";
  };

  boot.extraModprobeConfig = "options snd-hda-intel model=thinkpad,dmic-thinkpad\n";

  nixpkgs.config.allowUnfreePredicate =
    pkg:
    builtins.elem (pkgs.lib.getName pkg) [
      "nvidia-x11"
      "nvidia-settings"
    ];

  easyNvidia = {
    enable = true;
    withIntegratedGPU = true;
  };

  hardware.nvidia.prime = {
    nvidiaBusId = "PCI:1:0:0";
    # Apparently, xorg requires bus IDs to be *decimal*
    amdgpuBusId = "PCI:197:0:0";
  };

  services.xserver.videoDrivers = [ "amdgpu" ];
}
