{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs) rnnoise-plugin pipewire;
  pipewire-rnnoise-conf = pkgs.writeText "pipewire-rnnoise.conf" (builtins.toJSON {
    "context.properties" = {
      "log.level" = 2;
    };

    "context.spa-libs" = {
      "audio.convert.*" = "audioconvert/libspa-audioconvert";
      "support.*" = "support/libspa-support";
    };

    "context.modules" = [
      {
        name = "libpipewire-module-rtkit";
        args = {};
        flags = ["ifexists" "nofail"];
      }
      {name = "libpipewire-module-protocol-native";}
      {name = "libpipewire-module-client-node";}
      {name = "libpipewire-module-adapter";}
      {
        name = "libpipewire-module-filter-chain";
        args = {
          "node.name" = "rnnoise_source";
          "node.description" = "Noise Canceling source";
          "media.name" = "Noise Canceling source";
          "filter.graph" = {
            nodes = [
              {
                type = "ladspa";
                name = "rnnoise";
                plugin = "${rnnoise-plugin}/lib/ladspa/librnnoise_ladspa.so";
                label = "noise_suppressor_stereo";
                control = {
                  "VAD Threshold (%)" = 80.0;
                };
              }
            ];
          };

          "capture.props" = {
            "node.passive" = true;
          };
          "playback.props" = {
            "media.class" = "Audio/Source";
          };
        };
      }
    ];
  });
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
          ExecStart = "${pipewire}/bin/pipewire -c ${pipewire-rnnoise-conf}";
        };
        Install = {WantedBy = ["pipewire.service"];};
      };
    };
  };
}
