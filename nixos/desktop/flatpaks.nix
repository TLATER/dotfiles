/**
  Flatpak-related configuration.
*/
{
  services = {
    flatpak.enable = true;
    # Necessary for opening links in gtk under certain conditions
    gvfs.enable = true;
  };

  xdg.portal.enable = true;
}
