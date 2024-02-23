{
  config,
  lib,
  ...
}: let
  cfg = config.hardware.nvidia.vaapi;
in {
  options.hardware.nvidia.vaapi = with lib.types; {
    enable = lib.mkEnableOption "vaapi";

    maxInstances = lib.mkOption {
      type = nullOr int;
      default = null;
      description = ''
        The maximum number of concurrent instances of the driver.

        Sometimes useful for graphics cards with little VRAM.
      '';
    };

    firefox = {
      enable = lib.mkEnableOption "Firefox configuration";

      av1Support = lib.mkOption {
        type = bool;
        default = false;
        description = "Whether to enable av1 support. Should be disabled for GeForce 20 and earlier.";
      };
    };
  };

  # See https://github.com/elFarto/nvidia-vaapi-driver#configuration
  config = lib.mkIf cfg.enable {
    environment.variables =
      {
        NVD_BACKEND = "direct";
      }
      // lib.optionalAttrs (cfg.maxInstances != null) {
        NVD_MAX_INSTANCES = toString cfg.maxInstances;
      }
      // lib.optionalAttrs cfg.firefox.enable {
        MOZ_DISABLE_RDD_SANDBOX = "1";
      };

    # TODO(tlater): Find a way to properly integrate this so we can
    # upstream it.
    home-manager.users.tlater.programs.firefox.profiles.tlater.settings = lib.mkIf cfg.firefox.enable {
      "media.ffmpeg.vaapi.enabled" = true;
      "media.rdd-ffmpeg.enabled" = true;
      "media.av1.enabled" = cfg.firefox.av1Support;
      "gfx.x11-egl.force-enabled" = true;
      "widget.dmabuf.force-enabled" = true;
    };
  };
}
