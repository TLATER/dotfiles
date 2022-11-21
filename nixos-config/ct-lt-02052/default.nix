{
  pkgs,
  flake-inputs,
  ...
}: {
  imports = [
    flake-inputs.sops-nix.nixosModules.sops
    flake-inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t490
    flake-inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd

    ./hardware-configuration.nix
  ];

  home-manager.users.tlater = import "${flake-inputs.self}/home-config/hosts/work-desktop.nix";

  boot.initrd = {
    availableKernelModules = ["hid_roccat_ryos"];

    luks.devices.root.device = "/dev/disk/by-uuid/b3ac7dc6-cb0b-4350-bdfb-32329a5f61ff";
  };

  networking = {
    hostName = "ct-lt-02052";
    interfaces = {
      enp0s31f6.useDHCP = true;
      wlp0s20f3.useDHCP = true;
    };
    wireless.interfaces = ["wlp0s20f3"];
  };

  sops = {
    gnupg = {
      home = "/var/lib/sops";
      sshKeyPaths = [];
    };

    defaultSopsFile = "/etc/sops/secrets.yaml";
    validateSopsFiles = false;
  };

  sops.secrets = {
    codethink-vpn-ca = {};
    codethink-vpn-cert = {};
    codethink-vpn-key = {};
    codethink-vpn-static-key = {};
  };

  services.openvpn = {
    servers = {
      codethink = {
        autoStart = false;
        config = ''
          # This is an OpenVPN Client file.
          # Have a look at https://wiki.codethink.co.uk/base/operations/vpn/
          # to see how your VPN should be set up :)
          client
          dev tap
          proto udp
          remote vpn.codethink.co.uk 1194
          ca /run/secrets/codethink-vpn-ca
          cert /run/secrets/codethink-vpn-cert
          key /run/secrets/codethink-vpn-key
          resolv-retry infinite
          nobind
          compress lzo
          comp-lzo yes
          remote-cert-tls server
          tls-auth /run/secrets/codethink-vpn-static-key
          key-direction 1
        '';
        updateResolvConf = true;
      };
    };
  };

  environment.systemPackages = with pkgs; [
    docker-compose # Better to keep it in lock-step with the system docker
    fuse3 # For bst and related things
  ];

  # Virtualization is a lot more common at work...
  virtualisation = {
    docker.enable = true;
    libvirtd.enable = true;
  };
  users.users.tlater.extraGroups = ["docker" "libvirtd"];

  security.pki.certificates = [(builtins.readFile ./codethink-wifi.cert)];

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (pkgs.lib.getName pkg) ["steam-original"];
  hardware.steam-hardware.enable = true;
}
