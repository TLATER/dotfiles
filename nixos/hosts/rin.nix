/**
  Entrypoint for rin.
*/
{ inputs, lib, ... }: {
  imports = [
    inputs.famedly-nixos.nixosModules.default

    ../.
    ../hardware/bluetooth.nix
    ../hardware/nvidia.nix
    ../hardware/powermanagement.nix
    ../hardware/sound.nix
    ../hardware/yubikey.nix

    ./rin/disko.nix
    ./rin/docker.nix
    # ./rin/hardware.nix
    ./rin/wifi.nix
  ];

  unfree.allowUnfreePackages = [
    "nvidia-x11"
    "nvidia-settings"
  ];

  networking = {
    hostName = "rin";
    hostId = "e6aaf496";
  };

  home-manager.users.tlater = import "${inputs.self}/home-config/hosts/rin.nix";
  hardware.facter.reportPath = ./rin/facter.json;

  # TODO: Upstream this hardware quirk to the facter modules
  boot.extraModprobeConfig = "options snd-hda-intel model=thinkpad,dmic-thinkpad\n";

  # TODO: Figure out why we disable this
  programs.gnupg.agent.enable = lib.mkForce false;

  system.stateVersion = "20.09";
}
