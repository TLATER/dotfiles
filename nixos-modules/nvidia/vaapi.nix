{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.easyNvidia.vaapi;
  ffVersion = config.programs.firefox.package.version;
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
          Configure Firefox to used the vaapi driver for video decoding.

          Note that this requires disabling the [RDD
          sandbox](https://firefox-source-docs.mozilla.org/dom/ipc/process_model.html#data-decoder-rdd-process).
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
  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ pkgs.libva-utils ];
      variables =
        {
          NVD_BACKEND = "direct";
          LIBVA_DRIVER_NAME = "nvidia";
        }
        // lib.optionalAttrs (cfg.maxInstances != null) { NVD_MAX_INSTANCES = toString cfg.maxInstances; }
        // lib.optionalAttrs cfg.firefox.enable { MOZ_DISABLE_RDD_SANDBOX = "1"; };
    };

    programs.firefox.preferences = lib.mkIf cfg.firefox.enable {
      "media.ffmpeg.vaapi.enabled" = lib.versionOlder ffVersion "137.0.0";
      "media.hardware-video-decoding.force-enabled" = lib.versionAtLeast ffVersion "137.0.0";
      "media.rdd-ffmpeg.enabled" = lib.versionOlder ffVersion "97.0.0";
      "media.av1.enabled" = cfg.firefox.av1Support;
      "gfx.x11-egl.force-enabled" = true;
      "widget.dmabuf.force-enabled" = true;
    };
  };
}
