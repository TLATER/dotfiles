{
  imports = [
    ./dunst.nix
    ./gtk.nix
    ./mime.nix
    ./rofi.nix
    ./sway.nix
  ];

  xsession.importedVariables = [ "PATH" ];
}
