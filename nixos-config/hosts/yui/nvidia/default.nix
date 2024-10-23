{
  pkgs,
  flake-inputs,
  config,
  lib,
  ...
}:
{
  imports = [ ./vaapi.nix ];

  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
      version = "565.57.01";
      sha256_64bit = "sha256-buvpTlheOF6IBPWnQVLfQUiHv4GcwhvZW3Ks0PsYLHo=";
      sha256_aarch64 = lib.fakeHash;
      openSha256 = "sha256-/tM3n9huz1MTE6KKtTCBglBMBGGL/GOHi5ZSUag4zXA=";
      settingsSha256 = lib.fakeHash;
      persistencedSha256 = lib.fakeHash;
    };

    # The nvidia-settings build is currently broken due to a missing
    # vulkan header; re-enable whenever
    # 0384602eac8bc57add3227688ec242667df3ffe3the hits stable.
    nvidiaSettings = false;

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

  programs.sway.package = pkgs.sway.override {
    inherit (flake-inputs.nixpkgs-wayland.packages.${pkgs.system}) sway-unwrapped;
  };

  boot = {
    kernelPackages = lib.mkForce pkgs.linuxKernel.packages.linux_xanmod;
    kernelParams = [ "nvidia-drm.fbdev=1" ];

    extraModprobeConfig =
      "options nvidia "
      + lib.concatStringsSep " " [
        # nvidia assume that by default your CPU does not support PAT,
        # but this is effectively never the case in 2023
        "NVreg_UsePageAttributeTable=1"
        # This is sometimes needed for ddc/ci support, see
        # https://www.ddcutil.com/nvidia/
        #
        # Current monitor does not support it, but this is useful for
        # the future
        "NVreg_RegistryDwords=RMUseSwI2c=0x01;RMI2cSpeed=100"
      ];
  };

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
