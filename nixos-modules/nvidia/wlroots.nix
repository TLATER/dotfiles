{ config, lib, ... }:
let
  cfg = config.easyNvidia;
in
lib.mkIf (cfg.desktopEnvironment == "wlroots") {
  environment.variables = lib.mkIf (!cfg.offload.enable) {
    # Required to run the correct GBM backend for nvidia GPUs on wayland
    GBM_BACKEND = "nvidia-drm";
    # Apparently, without this nouveau may attempt to be used instead
    # (despite it being blacklisted)
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    # Hardware cursors are currently broken on wlroots
    WLR_NO_HARDWARE_CURSORS = "1";
  };

  # TODO(tlater): Implement some fancy rule that does predictable
  # interface names but for GPUs.
  #
  # services.udev.packages =
  #   let
  #     linkCardRules = pkgs.writeTextDir "lib/udev/rules.d/80-link-graphics-cards.rules" '''';
  #   in
  #   [ linkCardRules ];
}
