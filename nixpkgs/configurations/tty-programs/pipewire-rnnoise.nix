{ pkgs, lib, ... }:

let
  inherit (pkgs) rnnoise-plugin pipewire writeText;
  inherit (builtins) readFile replaceStrings;
  pipewire-rnnoise-conf =
    replaceStrings [ "\${rnnoise-plugin}" ] [ "${rnnoise-plugin}" ]
    (readFile ./filter-chain.conf);
in {
  systemd.user.services = {
    pipewire-rnnoise = {
      Unit = {
        After = [ "pipewire.service" ];
        BindsTo = [ "pipewire.service " ];
        Description = "rnnoise plugin for pipewire";
      };
      Service = {
        ExecStart = "${pipewire}/bin/pipewire -c ${
            writeText "pipewire-rnnoise.conf" pipewire-rnnoise-conf
          }";
      };
      Install = { WantedBy = [ "pipewire.service" ]; };
    };
  };
}
