{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.easyNvidia;
in
lib.mkMerge [

  (lib.mkIf (cfg.desktopEnvironment == "wlroots") {
    environment.variables = lib.mkMerge [
      (lib.mkIf (!cfg.offload.enable) {
        GBM_BACKEND = "nvidia-drm";
        # Apparently, without this nouveau may attempt to be used instead
        # (despite it being blacklisted)
        __GLX_VENDOR_LIBRARY_NAME = "nvidia";
        # Hardware cursors are currently broken on wlroots
        WLR_NO_HARDWARE_CURSORS = "1";
      })
      (lib.mkIf cfg.offload.enable { WLR_DRM_DEVICES = "/dev/dri/igpu1:/dev/dri/dgpu1"; })
    ];
  })

  # Both GNOME and Plasma guess which GPU should be used, so this
  # *may* not be necessary. However, given NixOS configuration, we
  # already should have an explicit setting from the user, and clearly
  # the guess isn't always correct, so we explicitly set the GPU to
  # use anyway.
  (lib.mkIf (cfg.desktopEnvironment == "gnome") {
    services.udev.packages = lib.mkIf cfg.offload.enable (
      pkgs.writeTextDir "lib/udev/rules.d/62-gnome-gpu-priority.rules" ''
        SYMLINK=="dri/igpu1", TAG+="mutter-device-preferred-primary"
      ''
    );
  })

  (lib.mkIf (cfg.desktopEnvironment == "plasma") {
    environment.variables.KWIN_DRM_DEVICES = lib.mkIf cfg.offload.enable "/dev/dri/igpu1:/dev/dri/dgpu1";
  })
]
