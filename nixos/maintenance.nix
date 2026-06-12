/**
  Maintenance related services.
*/
{
  boot.tmp.cleanOnBoot = true;

  services = {
    btrfs.autoScrub.enable = true;
    fstrim.enable = true;
    fwupd.enable = true;
  };
}
