{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs) rnnoise-plugin pipewire;
  pipewire-rnnoise-conf =
    builtins.replaceStrings ["\${rnnoise-plugin}"] ["${rnnoise-plugin}"]
    (builtins.readFile "${config._dotfiles}/filter-chain.conf");
in {
  config = lib.mkIf config.custom.desktop-environment {
    systemd.user.services = {
      pipewire-rnnoise = {
        Unit = {
          After = ["pipewire.service"];
          BindsTo = ["pipewire.service "];
          Description = "rnnoise plugin for pipewire";
        };
        Service = {
          ExecStart = "${pipewire}/bin/pipewire -c ${
            pkgs.writeText "pipewire-rnnoise.conf" pipewire-rnnoise-conf
          }";
        };
        Install = {WantedBy = ["pipewire.service"];};
      };
    };
  };
}
