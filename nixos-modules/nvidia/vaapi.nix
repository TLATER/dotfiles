{
  pkgs,
  config,
  lib,
  options,
  ...
}:
let
  cfg = config.easyNvidia.vaapi;
in
{
  options.easyNvidia.vaapi = with lib.types; {
    enable = lib.mkOption {
      type = bool;
      default = config.easyNvidia.enable && !config.easyNvidia.withIntegratedGPU;
      description = ''
        Whether to enable the NVIDIA vaapi driver.

        This allows using the NVIDIA GPU for decoding video streams
        instead of using software decoding on the CPU.

        This particularly makes sense for desktop computers without an
        iGPU, as on those software en/decoding will take a lot of
        processing power while the NVIDIA GPU's encoding capacity
        isn't doing anything, so this option is enabled by default
        there.

        However, on machines with an iGPU, the dGPU's en/decoding
        capabilities are often more limited than those of the iGPU,
        and require more power, so this is disabled there by default -
        it may still make sense from time to time, so feel free to
        experiment.
      '';
    };

    maxInstances = lib.mkOption {
      type = nullOr int;
      default = null;
      description = ''
        The maximum number of concurrent instances of the driver.

        Sometimes useful for graphics cards with little VRAM.
      '';
    };

    firefox = {
      enable = lib.mkOption {
        type = bool;
        default = config.programs.firefox.enable;
        description = ''
          Configure Firefox-based browsers to use the vaapi driver for video
          decoding.

          Note that this requires disabling the [RDD
          sandbox](https://firefox-source-docs.mozilla.org/dom/ipc/process_model.html#data-decoder-rdd-process).

          Includes home-manager integration, if home-manager is used to manage
          Firefox (or its forks, e.g. LibreWolf).
        '';
      };

      av1Support = lib.mkOption {
        type = bool;
        default = false;
        description = ''
          Whether to enable av1 support.

          This will not work on Turing (e.g. Geforce 2xxx) and
          earlier, and is therefore disabled by default there.
        '';
      };
    };
  };

  # See https://github.com/elFarto/nvidia-vaapi-driver#configuration
  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        environment = {
          systemPackages = [ pkgs.libva-utils ];

          variables = lib.mergeAttrsList (
            [
              {
                MOZ_DISABLE_RDD_SANDBOX = "1";
                NVD_BACKEND = "direct";
                LIBVA_DRIVER_NAME = "nvidia";
              }
            ]
            ++ lib.optional (cfg.maxInstances != null) { NVD_MAX_INSTANCES = toString cfg.maxInstances; }
          );
        };
      }

      # Options for various Firefox module implementations
      (
        let
          firefoxSettings =
            package:
            let
              ffVersion = package.version or config.programs.firefox.package.version;
            in
            {
              "media.ffmpeg.vaapi.enabled" = lib.versionOlder ffVersion "137.0.0";
              "media.hardware-video-decoding.force-enabled" = lib.versionAtLeast ffVersion "137.0.0";
              "media.rdd-ffmpeg.enabled" = lib.versionOlder ffVersion "97.0.0";
              "media.av1.enabled" = cfg.firefox.av1Support;
              "gfx.x11-egl.force-enabled" = true;
              "widget.dmabuf.force-enabled" = true;
            };
        in
        lib.mkMerge [
          { programs.firefox.preferences = firefoxSettings config.programs.firefox.package; }
          (lib.optionalAttrs (options ? home-manager) {
            home-manager.sharedModules = [
              (
                { config, ... }:
                {
                  programs.librewolf.settings = firefoxSettings config.programs.librewolf.package;
                  # TODO(tlater): Add settings for Firefox and floorp
                }
              )
            ];
          })
        ]
      )
    ]
  );
}
