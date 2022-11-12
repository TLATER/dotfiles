{
  pkgs,
  config,
  ...
}: {
  # Recommended, presumably because it handles the permission stuff
  # for screen recordings and such.
  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };
}
