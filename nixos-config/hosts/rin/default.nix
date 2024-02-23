{lib, flake-inputs, ...}: {
  imports = [
    flake-inputs.disko.nixosModules.disko

    ../../networks/personal.nix

    ./hardware-configuration.nix
    ./disko.nix

    ./hardware-policy.nix
  ];

  home-manager.users.tlater = import "${flake-inputs.self}/home-config/hosts/rin";

  sops = {
    gnupg = lib.mkForce {};
    age.keyFile = "/var/lib/sops/host.age";
  };

  networking = {
    hostName = "rin";
    hostId = "e6aaf496";
    wireless.interfaces = ["wlp2s0"];
  };

  systemd.network = {
    networks = {
      "40-wlp2s0" = {
        matchConfig.Name = "wlp2s0";
        networkConfig.DHCP = "yes";
        linkConfig.RequiredForOnline = "yes";
      };
    };
  };
}
