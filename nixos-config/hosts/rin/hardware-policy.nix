{
  flake-inputs,
  pkgs,
  lib,
  config,
  ...
}: {
  services.osquery = {
    enable = true;

    flags = {
      tls_hostname = "fleet.famedly.de";

      # Enrollment
      host_identifier = "instance";
      enroll_secret_path = config.sops.secrets."osquery/enroll".path;
      enroll_tls_endpoint = "/api/osquery/enroll";

      # Configuration
      config_plugin = "tls";
      config_tls_endpoint = "/api/v1/osquery/config";
      config_refresh = "10";

      # Live query
      disable_distributed = "false";
      distributed_plugin = "tls";
      distributed_interval = "10";
      distributed_tls_max_attempts = "3";
      distributed_tls_read_endpoint = "/api/v1/osquery/distributed/read";
      distributed_tls_write_endpoint = "/api/v1/osquery/distributed/write";

      # Logging
      logger_plugin = "tls";
      logger_tls_endpoint = "/api/v1/osquery/log";
      logger_tls_period = "10";

      # File carving
      disable_carver = "false";
      carver_start_endpoint = "/api/v1/osquery/carve/begin";
      carver_continue_endpoint = "/api/v1/osquery/carve/block";
      carver_block_size = "2000000";

      # Fix non-fhs paths
      tls_server_certs = "${pkgs.osquery}/share/osquery/certs/certs.pem";
    };
  };

  services.clamav = {
    updater.enable = true;
    daemon.enable = true;
  };

  systemd = {
    services = {
      drivestrike = {
        serviceConfig = {
          ExecStart = "${flake-inputs.self.packages.${pkgs.system}.drivestrike}/bin/drivestrike run";
          SyslogIdentifier = "drivestrike";
        };

        after = ["network.target" "drivestrike-lock.service"];
        wantedBy = ["multi-user.target"];
      };

      # Famedly's osquery checks for a process name of `clamd` and
      # `freshclam` - by default, however, systemd will use the filename,
      # which is an absolute path, and therefore this will look like clamd
      # is not running.
      #
      # TODO(tlater): Suggest an osquery that doesn't require this
      clamav-daemon.serviceConfig.ExecStart = lib.mkForce "@${pkgs.clamav}/bin/clamd clamd";

      clamav-freshclam = {
        serviceConfig = {
          Type = lib.mkForce "simple";
          ExecStart = lib.mkForce "@${pkgs.clamav}/bin/freshclam freshclam --daemon --foreground";
        };
        wantedBy = ["clamav-daemon.service"];
      };
    };

    # NixOS' freshclam (clamav updater) service is run as a timer by
    # default, but famedly expects it to run as a daemon.
    timers.clamav-freshclam.enable = false;
  };

  # NixOS enables an outgoing-only firewall by default anyway, but
  # this ensures it stays enabled even if an update turned it off or
  # something.
  networking.firewall = {
    enable = true;
    # Probably just don't allow pings either
    allowPing = false;
    # DO NOT USE NFTABLES

    # Allow docker containers to communicate
    extraCommands = let
      # Either get the docker daemon setting *or* the default value
      dockerAddressPools =
        config.virtualisation.docker.daemon.settings.default-address-pools
        or [
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
    in ''
      iptables -A INPUT -s ${addresses} -d ${addresses} -j ACCEPT
    '';
  };

  sops.secrets."osquery/enroll" = {};
}
