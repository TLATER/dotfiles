{
  config,
  flake-inputs,
  lib,
  ...
}:
{
  imports = [
    flake-inputs.famedly-nixos.nixosModules.default

    ../../laptops.nix
    ../../networking/work.nix

    ./hardware.nix
  ];

  home-manager.users.tlater = import "${flake-inputs.self}/home-config/hosts/rin.nix";

  sops = {
    age.keyFile = "/var/lib/sops/host.age";
    secrets."osquery/enroll" = { };
  };

  famedly-hwp.osquery_secret_path = config.sops.secrets."osquery/enroll".path;
  programs.gnupg.agent.enable = lib.mkForce false;

  # Used extensively for testing at work
  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;

    autoPrune.enable = true;

    daemon.settings = {
      live-restore = false;
    };
  };

  users.users.tlater.extraGroups = [ "docker" ];

  # Allow docker containers to communicate
  networking.firewall.extraCommands =
    let
      # Either get the docker daemon setting *or* the default value
      dockerAddressPools =
        config.virtualisation.docker.daemon.settings.default-address-pools or [
          {
            base = "172.30.0.0/16";
            size = 24;
          }
          {
            base = "172.31.0.0/16";
            size = 24;
          }
        ];
      addresses = lib.concatMapStringsSep "," (pool: pool.base) dockerAddressPools;
    in
    ''
      iptables -A INPUT -s ${addresses} -d ${addresses},172.17.0.1 -j ACCEPT
    '';
}
