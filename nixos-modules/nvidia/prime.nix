{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.easyNvidia.offload;

  # See
  # https://download.nvidia.com/XFree86/Linux-x86_64/565.77/README/dynamicpowermanagement.html#SystemSettings1be5e
  # for details
  enablePowerManagement =
    ''
      # Enable runtime PM for NVIDIA VGA/3D controller devices on driver bind
      ACTION=="bind", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x030000", TEST=="power/control", ATTR{power/control}="auto"
      ACTION=="bind", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x030200", TEST=="power/control", ATTR{power/control}="auto"

      # Disable runtime PM for NVIDIA VGA/3D controller devices on driver unbind
      ACTION=="unbind", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x030000", TEST=="power/control", ATTR{power/control}="on"
      ACTION=="unbind", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x030200", TEST=="power/control", ATTR{power/control}="on"
    ''
    + lib.optionalString (!(config.boot.kernelPackages.kernel.kernelAtLeast "5.5")) ''
      # Remove NVIDIA USB xHCI Host Controller devices, if present
      ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x0c0330", ATTR{remove}="1"

      # Remove NVIDIA USB Type-C UCSI devices, if present
      ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x0c8000", ATTR{remove}="1"

      # Remove NVIDIA Audio devices, if present
      ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x040300", ATTR{remove}="1"
    '';

  enablePowerManagementPackage = pkgs.writeTextDir "lib/udev/rules.d/80-nvidia-pm.rules" enablePowerManagement;

  nvidiaId = map (i: lib.toHexString (lib.toInt i)) (
    lib.drop 1 (lib.splitString ":" config.hardware.nvidia.prime.nvidiaBusId)
  );
  domain = "0000";
  bus = builtins.elemAt nvidiaId 0;
  device = builtins.elemAt nvidiaId 1;
  function = builtins.elemAt nvidiaId 2;
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
    hardware.nvidia.prime.offload.enable = true;

    # TODO(tlater): This is a workaround for the udev rules not
    # working
    powerManagement.powerUpCommands = "echo auto > /sys/bus/pci/devices/${domain}:${bus}:${device}.${function}/power/control";

    # We don't use the upstream option because its udev rules aren't
    # applied at stage 1 when modesetting is enabled, and cannot
    # reasonably be overridden. Instead, we define a proper package
    # and add it to the correct set of udev packages for the system.
    #
    # In addition, modern drivers automatically enable finegrained
    # mode on cards that properly support it, so there's no reason to
    # explicitly set its kernel arg, and hence all the things the
    # upstream `powermanagement.finegrained` does are pointless.
    #
    # TODO(tlater): This still don't work
    #
    # hardware.nvidia.powerManagement.finegrained = true;
    boot.initrd.services.udev = lib.mkIf config.hardware.nvidia.modesetting.enable {
      rules = lib.mkIf (!config.boot.initrd.systemd.enable) enablePowerManagement;

      packages = lib.mkIf config.boot.initrd.systemd.enable [ enablePowerManagementPackage ];
    };

    services.udev.packages = [ enablePowerManagementPackage ];
  };
}
