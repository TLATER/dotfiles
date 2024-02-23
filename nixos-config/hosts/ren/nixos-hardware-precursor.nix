{flake-inputs, ...}: {
  imports = [
    flake-inputs.nixos-hardware.nixosModules.common-hidpi
    flake-inputs.nixos-hardware.nixosModules.common-pc-laptop
    flake-inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    flake-inputs.nixos-hardware.nixosModules.common-cpu-intel-sandy-bridge
  ];
}
