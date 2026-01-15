{ pkgs, ... }:
let
  bridgeName = "br0";
in
{
  # Module to help with developing in VMs.
  # Sets up a bridge network and domain name for a qemu VM.
  #
  # qemu networking configuration for this set up:
  #
  # - -device virtio-net,netdev=n1
  # - -netdev bridge,id=n1,br=br0,helper=$(which qemu-bridge-helper)
  #
  # Also set up a static IP address of 192.168.9.2/24, with a gateway
  # of 192.168.9.1.

  # Enable qemu-bridge-helper for setting up bridged networking in VMs
  security.wrappers.qemu-bridge-helper = {
    setuid = true;
    owner = "root";
    group = "root";
    source = "${pkgs.qemu_kvm}/libexec/qemu-bridge-helper";
  };

  # Allow qemu to use the bridge network
  environment.etc."qemu/bridge.conf" = {
    user = "root";
    group = "qemu";
    mode = "0640";
    text = ''
      allow ${bridgeName}
    '';
  };

  users = {
    users.tlater.extraGroups = [ "qemu" ];
    groups.qemu = { };
  };

  # Set up firewall to forward requests to the internet from the
  # bridge network
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

  networking = {
    # Add bridge network to connect VMs to
    networkmanager.ensureProfiles.profiles.bridge = {
      connection = {
        id = bridgeName;
        type = "bridge";
        interface-name = bridgeName;
      };

      ipv4 = {
        method = "manual";
        address1 = "192.168.9.1/24";
      };
    };

    nftables.tables.dev-nat = {
      family = "inet";
      content = ''
        chain postrouting {
          type nat hook postrouting priority 100;
          ip saddr 192.168.9.0/24 oifname bond0 masquerade
        }
      '';
    };
  };

  # Add local DNS zone for VMs
  services.unbound.settings.server = {
    local-zone = ''"dev.local." redirect'';
    local-data = ''"dev.local. A 192.168.9.2"'';
  };
}
