{
  imports = [
    ./dunst.nix
    ./eww.nix
    ./gtk.nix
    ./mime.nix
    ./sway.nix
  ];

  xsession.importedVariables = [ "PATH" ];
}
