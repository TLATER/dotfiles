{ pkgs, lib, ... }:

let
  inherit (pkgs) rnnoise-plugin pipewire;
  json = pkgs.formats.json { };
  pipewire-rnnoise-conf = ./filter-chain.conf;
in {
  systemd.user.services = {
    pipewire-rnnoise = {
      Unit = {
        After = [ "pipewire.service" ];
        BindsTo = [ "pipewire.service " ];
        Description = "rnnoise plugin for pipewire";
      };
      Service = {
        ExecStart = "${pipewire}/bin/pipewire -c ${pipewire-rnnoise-conf}";
      };
      Install = { WantedBy = [ "pipewire.service" ]; };
    };
  };
}
