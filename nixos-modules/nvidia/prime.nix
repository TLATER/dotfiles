{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.easyNvidia.offload;
in
{
  options.easyNvidia.offload = with lib.types; {
    enable = lib.mkOption {
      type = bool;
      default = config.easyNvidia.enable && config.easyNvidia.withIntegratedGPU;
      description = ''
        Whether to configure prime offload.

        This will allow on-demand offloading of rendering tasks to the
        NVIDIA GPU, all other rendering will happen on the GPU
        integrated in the CPU.

        The GPU *should* be turned off whenever it is not in use, so
        this shouldn't cause increased battery drain, but there are
        some reports floating around that this isn't always the case -
        likely especially for older devices. Feel free to turn it off
        if you find this doesn't work properly for you.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    hardware.nvidia = {
      prime.offload.enable = true;
      powerManagement.finegrained = true;
    };

    # Set up a udev rule to create named symlinks for the pci paths.
    #
    # This is necessary because wlroots splits the DRM_DEVICES on
    # `:`, which is part of the pci path.
    services.udev.packages =
      let
        pciPath =
          xorgBusId:
          let
            components = lib.drop 1 (lib.splitString ":" xorgBusId);
            toHex = i: lib.toLower (lib.toHexString (lib.toInt i));

            domain = "0000"; # Apparently the domain is practically always set to 0000
            bus = lib.fixedWidthString 2 "0" (toHex (builtins.elemAt components 0));
            device = lib.fixedWidthString 2 "0" (toHex (builtins.elemAt components 1));
            function = builtins.elemAt components 2; # The function is supposedly a decimal number
          in
          "dri/by-path/pci-${domain}:${bus}:${device}.${function}-card";

        pCfg = config.hardware.nvidia.prime;
        igpuPath = pciPath (if pCfg.intelBusId != "" then pCfg.intelBusId else pCfg.amdgpuBusId);
        dgpuPath = pciPath pCfg.nvidiaBusId;
      in
      lib.mkIf (config.easyNvidia.desktopEnvironment != "x11") (
        lib.singleton (
          pkgs.writeTextDir "lib/udev/rules.d/61-gpu-offload.rules" ''
            SYMLINK=="${igpuPath}", SYMLINK+="dri/igpu1"
            SYMLINK=="${dgpuPath}", SYMLINK+="dri/dgpu1"
          ''
        )
      );
  };
}
