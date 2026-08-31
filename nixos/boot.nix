/**
  Configuration related to the bootloader and initrd.
*/
{ pkgs, ... }: {
  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_xanmod;
    initrd.systemd.enable = true;
    plymouth.enable = true;

    loader = {
      timeout = 0;
      efi.canTouchEfiVariables = true;

      systemd-boot = {
        enable = true;
        configurationLimit = 5;
        editor = false;
      };
    };
  };

  fileSystems."/boot".options = [ "umask=0077" ];

  # My systems never have usable root accounts anyway, so emergency
  # mode just drops into a shell telling me it can't log into root
  systemd.enableEmergencyMode = false;

  # hardware.enableRedistributableFirmware = true;
}
