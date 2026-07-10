/**
  Module to correctly configure an NVIDIA GPU.
*/
{
  config,
  inputs',
  lib,
  pkgs,
  ...
}:
let
  cpuSupportsPat = lib.elem "pat" (lib.head config.hardware.facter.report.hardware.cpu).features;

  amdGpus = lib.filter (
    gpu: gpu.driver == "amdgpu"
  ) config.hardware.facter.report.hardware.graphics_card;

  # TODO: Figure out what the output looks like for intel
  intelGpus = [ ];

  iGpus = amdGpus ++ intelGpus;

  nvidiaGpus = lib.filter (
    gpu: gpu.driver == "nvidia"
  ) config.hardware.facter.report.hardware.graphics_card;

  hasIGPU = iGpus != [ ];
in
lib.mkMerge [
  {
    services.xserver.videoDrivers = [ "nvidia" ];

    hardware.nvidia = {
      open = true;
      powerManagement.enable = true;
      moduleParams.nvidia.NVreg_UsePageAttributeTable = lib.mkIf cpuSupportsPat 1;

      package =
        let
          inherit (inputs'.self.packages) nvidia;
        in
        config.boot.kernelPackages.nvidiaPackages.mkDriver {
          inherit (nvidia) version;
          sha256_64bit = nvidia.src.outputHash;
          openSha256 = nvidia.open.src.outputHash;
          useSettings = false;
          usePersistenced = false;
        };

      # Disabled because I don't use it and I can't be bothered to
      # figure out how to get a hash for something nvidia don't seem to
      # publish consistently.
      nvidiaSettings = false;
    };
  }

  (lib.mkIf (!hasIGPU) {
    environment.variables = {
      GBM_BACKEND = "nvidia-drm";
      # Apparently, without this nouveau may attempt to be used instead
      # (despite it being blacklisted)
      __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      # Hardware cursors are currently broken on wlroots
      WLR_NO_HARDWARE_CURSORS = "1";
    };
  })

  (lib.mkIf hasIGPU (
    let
      dGpu = lib.head nvidiaGpus;
      iGpu = lib.head iGpus;

      toBusId =
        gpu: "PCI:${toString gpu.slot.bus}:${toString gpu.slot.number}:${toString gpu.detail.function}";
    in
    {
      hardware.nvidia = {
        dynamicBoost.enable = true;
        powerManagement.finegrained = true;

        prime = {
          offload.enable = true;

          nvidiaBusId = toBusId dGpu;
          amdgpuBusId = lib.mkIf (iGpu.driver == "amdgpu") (toBusId iGpu);
          intelBusId = lib.mkIf false (toBusId iGpu);
        };
      };

      # Set up a udev rule to create named symlinks for the pci paths.
      #
      # This is necessary because wlroots splits the DRM_DEVICES on
      # `:`, which is part of the pci path.
      #
      # TODO: Thanks to facter it's possible to enumerate all GPUs.
      services.udev.packages = [
        (pkgs.writeTextDir "lib/udev/rules.d/61-gpu-offload.rules" ''
          SYMLINK=="dri/by-path/pci-${iGpu.sysfs_bus_id}-card", SYMLINK+="dri/igpu1"
          SYMLINK=="dri/by-path/pci-${dGpu.sysfs_bus_id}-card", SYMLINK+="dri/dgpu1"
        '')

        (pkgs.writeTextDir "lib/udev/rules.d/62-gnome-gpu-priority.rules" ''
          SYMLINK=="dri/igpu1", TAG+="mutter-device-preferred-primary"
        '')
      ];

      environment.variables = {
        KWIN_DRM_DEVICES = "/dev/dri/igpu1:/dev/dri/dgpu1";
        WLR_DRM_DEVICES = "/dev/dri/igpu1:/dev/dri/dgpu1";
      };
    }
  ))
]
