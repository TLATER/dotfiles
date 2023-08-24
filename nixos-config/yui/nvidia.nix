{
  config,
  flake-inputs,
  lib,
  pkgs,
  ...
}: {
  hardware.nvidia = {
    modesetting.enable = true;
    # Power management is required to get nvidia GPUs to behave on
    # suspend, due to firmware bugs. Aren't nvidia great?
    powerManagement.enable = true;
  };

  boot.extraModprobeConfig =
    "options nvidia "
    + lib.concatStringsSep " " [
      # nvidia assume that by default your CPU does not support PAT,
      # but this is effectively never the case in 2023
      "NVreg_UsePageAttributeTable=1"
      # This may be a noop, but it's somewhat uncertain
      "NVreg_EnablePCIeGen3=1"
      # This is sometimes needed for ddc/ci support, see
      # https://www.ddcutil.com/nvidia/
      #
      # Current monitor does not support it, but this is useful for
      # the future
      "NVreg_RegistryDwords=RMUseSwI2c=0x01;RMI2cSpeed=100"
      # When (if!) I get another nvidia GPU, check for resizeable bar
      # settings
    ];

  environment.variables = {
    # Necessary to correctly enable va-api (video codec hardware
    # acceleration). If this isn't set, the libvdpau backend will be
    # picked, and that one doesn't work with most things, including
    # Firefox.
    LIBVA_DRIVER_NAME = "nvidia";
    # Required to run the correct GBM backend for nvidia GPUs on wayland
    GBM_BACKEND = "nvidia-drm";
    # Apparently, without this nouveau may attempt to be used instead
    # (despite it being blacklisted)
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    # Hardware cursors are currently broken on nvidia
    WLR_NO_HARDWARE_CURSORS = "1";

    # Required to use va-api it in Firefox. See
    # https://github.com/elFarto/nvidia-vaapi-driver/issues/96
    MOZ_DISABLE_RDD_SANDBOX = "1";
    # It appears that the normal rendering mode is broken on recent
    # nvidia drivers:
    # https://github.com/elFarto/nvidia-vaapi-driver/issues/213#issuecomment-1585584038
    NVD_BACKEND = "direct";
    # Required for firefox 98+, see:
    # https://github.com/elFarto/nvidia-vaapi-driver#firefox
    EGL_PLATFORM = "wayland";
  };

  programs.hyprland.enableNvidiaPatches = true;

  # Ugly hack to fix a bug in egl-wayland, see
  # https://github.com/NixOS/nixpkgs/issues/202454
  environment.etc."egl/egl_external_platform.d".source = let
    nvidia_wayland = pkgs.writeText "10_nvidia_wayland.json" ''
      {
          "file_format_version" : "1.0.0",
          "ICD" : {
              "library_path" : "${flake-inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.egl-wayland}/lib/libnvidia-egl-wayland.so"
          }
      }
    '';
    nvidia_gbm = pkgs.writeText "15_nvidia_gbm.json" ''
      {
          "file_format_version" : "1.0.0",
          "ICD" : {
              "library_path" : "${config.hardware.nvidia.package}/lib/libnvidia-egl-gbm.so.1"
          }
      }
    '';
  in
    lib.mkForce (pkgs.runCommandLocal "nvidia-egl-hack" {} ''
      mkdir -p $out
      cp ${nvidia_wayland} $out/10_nvidia_wayland.json
      cp ${nvidia_gbm} $out/15_nvidia_gbm.json
    '');
}
