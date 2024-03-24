{
  config,
  lib,
  ...
}: {
  imports = [
    ./vaapi.nix
  ];

  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
      version = "550.67";
      sha256_64bit = "sha256-mSAaCccc/w/QJh6w8Mva0oLrqB+cOSO1YMz1Se/32uI=";
      sha256_aarch64 = "sha256-+UuK0UniAsndN15VDb/xopjkdlc6ZGk5LIm/GNs5ivA=";
      openSha256 = "sha256-M/1qAQxTm61bznAtCoNQXICfThh3hLqfd0s1n1BFj2A=";
      settingsSha256 = "sha256-FUEwXpeUMH1DYH77/t76wF1UslkcW721x9BHasaRUaM=";
      persistencedSha256 = "sha256-ojHbmSAOYl3lOi2X6HOBlokTXhTCK6VNsH6+xfGQsyo=";
    };

    # The current stable nvidia driver is utterly broken. Use
    # production for now to work around stuff like this:
    # https://forums.developer.nvidia.com/t/535-86-05-low-framerate-vulkan-apps-stutter-under-wayland-xwayland/26147
    # package = config.boot.kernelPackages.nvidiaPackages.production;
    modesetting.enable = true;
    # Power management is required to get nvidia GPUs to behave on
    # suspend, due to firmware bugs. Aren't nvidia great?
    powerManagement.enable = true;
    open = true;

    vaapi = {
      enable = true;
      firefox.enable = true;
    };
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

  # Replace a glFlush() with a glFinish() - this prevents stuttering
  # and glitching in all kinds of circumstances for the moment.
  #
  # Apparently I'm waiting for "explicit sync" support, which needs to
  # land as a wayland thing. I've seen this work reasonably with VRR
  # before, but emacs continued to stutter, so for now this is
  # staying.
  nixpkgs.overlays = [
    (_: final: {
      wlroots_0_16 = final.wlroots_0_16.overrideAttrs (_: {
        patches = [
          ./wlroots-nvidia.patch
          ./wlroots-screenshare.patch
        ];
      });
    })
  ];

  environment.variables = {
    # Required to run the correct GBM backend for nvidia GPUs on wayland
    GBM_BACKEND = "nvidia-drm";
    # Apparently, without this nouveau may attempt to be used instead
    # (despite it being blacklisted)
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    # Hardware cursors are currently broken on nvidia
    WLR_NO_HARDWARE_CURSORS = "1";
  };
}
