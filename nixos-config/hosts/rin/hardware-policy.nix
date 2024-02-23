{
  flake-inputs,
  pkgs,
  config,
  ...
}: {
  services.osquery = {
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
    };
  };

  systemd.packages = [flake-inputs.self.packages.${pkgs.system}.drivestrike];
  systemd.services.drivestrike.wantedBy = ["multi-user.target"];

  services.clamav = {
    updater.enable = true;
    daemon.enable = true;
  };

  # NixOS enables an incoming-only firewall by default anyway, but
  # this ensures it stays enabled even if an update turned it off or
  # something.
  networking.firewall.enable = true;
  # Don't use nftables to prevent issues with docker.
}
