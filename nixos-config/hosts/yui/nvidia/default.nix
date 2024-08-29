{ pkgs, flake-inputs, config, lib, ... }:
let
  unstable = flake-inputs.nixpkgs-unstable.legacyPackages.${pkgs.system};
in
{
  imports = [ ./vaapi.nix ];

  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
      version = "560.35.03";
      sha256_64bit = "sha256-8pMskvrdQ8WyNBvkU/xPc/CtcYXCa7ekP73oGuKfH+M=";
      sha256_aarch64 = "sha256-s8ZAVKvRNXpjxRYqM3E5oss5FdqW+tv1qQC2pDjfG+s=";
      openSha256 = "sha256-/32Zf0dKrofTmPZ3Ratw4vDM7B+OgpC4p7s+RHUjCrg=";
      settingsSha256 = "sha256-kQsvDgnxis9ANFmwIwB7HX5MkIAcpEEAHc8IBOLdXvk=";
      persistencedSha256 = "sha256-E2J2wYYyRu7Kc3MMZz/8ZIemcZg68rkzvqEwFAL3fFs=";
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

  nixpkgs.overlays = [
    (final: _prev: {
      sway-unwrapped = unstable.sway-unwrapped.overrideAttrs (attrs: {
        version = "0-unstable-2024-08-28";
        src = final.fetchFromGitHub {
          owner = "swaywm";
          repo = "sway";
          rev = "980a4e02113789d0cca94aa023557c6f6e87ec73";
          hash = "sha256-qciZeQghlLV5aMuOnex3LvFU9vTa941RMlUkdvj0QTU=";
        };
        buildInputs = attrs.buildInputs ++ [ final.wlroots ];
        mesonFlags =
          let
            inherit (lib.strings) mesonEnable mesonOption;
          in
          [
            (mesonOption "sd-bus-provider" "libsystemd")
            (mesonEnable "tray" attrs.trayEnabled)
          ];
      });

      wlroots = unstable.wlroots.overrideAttrs (_attrs: {
        version = "0-unstable-2024-08-29";
        src = final.fetchFromGitLab {
          domain = "gitlab.freedesktop.org";
          owner = "wlroots";
          repo = "wlroots";
          rev = "beb9a9ad0a38867154b7606911c33ffa5ecf759f";
          hash = "sha256-ZlNFxwj3c5zKiSfokA27zhJ+Yar8cma4fj6N/ulI0VM=";
        };
      });
    })
  ];

  boot = {
    kernelParams = [
      "nvidia-drm.fbdev=1"
    ];

    extraModprobeConfig =
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
