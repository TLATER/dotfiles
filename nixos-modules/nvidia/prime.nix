{
  config,
  lib,
  ...
}:
let
  cfg = config.easyNvidia.prime;
in
{
  options.easyNvidia.offload = with lib.types; {
    enable = lib.mkOption {
      type = bool;
      enable = config.easyNvidia.enable && config.easyNvidia.prime.withIntegratedGPU;
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

  config = lib.mkIf cfg.easyNvidia.offload.enable {
    hardware.nvidia.prime.offload.enable = true;
    hardware.nvidia.powerManagement.finegrained = true;
  };
}
