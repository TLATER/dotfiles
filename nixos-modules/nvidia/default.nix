{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.easyNvidia;
in
{
  imports = [
    ./vaapi.nix
    ./prime.nix
    ./de-compat.nix
  ];

  options.easyNvidia = with lib.types; {
    enable = lib.mkEnableOption "easyNvidia";

    withIntegratedGPU = lib.mkOption {
      type = bool;
      description = ''
        Whether the computer has a separate integrated GPU.

        This also configures the machine to use the integrated GPU for
        other things like software decoding, so keep this enabled even
        if you separately disable offload rendering.
      '';
    };

    # TODO(tlater): Think of a better way to integrate
    # this. Realistically, this should be added to the startup script
    # of each of these, but that requires upstream coordination.
    desktopEnvironment = lib.mkOption {
      type = lib.types.enum [
        "x11"
        "wlroots"
        "plasma"
        "gnome"
      ];
      description = ''
        The desktop environment that will be used. Each has subtly
        different semantics for setting GPU priority, so needs to be
        handled separately.

        If using a traditional X11-based DE or WM, always choose
        `x11`, even when using GNOME or KDE.

        Using multiple session types is not yet supported.
      '';
    };

    advanced = {
      usePageAttributeTable = lib.mkOption {
        default = true;
        type = bool;
        description = ''
          Whether to use the page attribute table. Nvidia is overly
          conservative with enabling this; practically all modern CPUs
          have support for this, and it can meaningfully impact
          performance.
        '';
      };

      monitorControlSupport = lib.mkOption {
        default = true;
        type = bool;
        description = ''
          Whether to add i2c support for communication with monitors.

          This isn't universally necessary, or even useful, but it
          enables configuring some monitor settings on some monitors
          with some GPUs, and shouldn't otherwise be harmful.

          See https://www.ddcutil.com/nvidia/ for details.
        '';
      };

      forceKernel = lib.mkOption {
        default = false;
        type = bool;
        description = ''
          Override the kernel version assertion. You're on your own.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = lib.mkIf (!cfg.advanced.forceKernel) (
      lib.singleton {
        assertion =
          config.boot.kernelPackages.kernel config.boot.kernelPackages.kernel.version
          == pkgs.linuxKernel.kernels.linux_default.version;
        message = "The nvidia driver can only support the LTS kernel. You can ignore this with `easyNvidia.advanced.forceKernel`.";
      }
    );

    services.xserver.videoDrivers = [ "nvidia" ];

    hardware.nvidia = {
      # This will no longer be necessary when
      # https://github.com/NixOS/nixpkgs/pull/326369 hits stable
      modesetting.enable = lib.mkDefault true;
      # Power management is nearly always required to get nvidia GPUs to
      # behave on suspend, due to firmware bugs.
      powerManagement.enable = true;
      # The open driver is recommended by nvidia now, see
      # https://download.nvidia.com/XFree86/Linux-x86_64/565.57.01/README/kernel_open.html
      open = true;

      dynamicBoost.enable = cfg.enable && cfg.withIntegratedGPU;
    };

    boot.extraModprobeConfig =
      let
        options =
          lib.optional cfg.advanced.usePageAttributeTable "NVreg_UsePageAttributeTable=1"
          ++ lib.optional cfg.advanced.monitorControlSupport "NVreg_RegistryDwords=RMUseSwI2c=0x01;RMI2cSpeed=100";
      in
      lib.mkIf (options != [ ]) "options nvidia ${lib.concatStringsSep " " options}";
  };
}
