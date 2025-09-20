{
  imports = [
    ./gtk.nix
    ./mime.nix
    ./sway.nix
  ];

  xsession.importedVariables = [ "PATH" ];
}
