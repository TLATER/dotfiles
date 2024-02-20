{
  imports = [
    ./dunst.nix
    ./gtk.nix
    ./mime.nix
    ./ncmpcpp.nix
    ./pipewire.nix
    ./rofi.nix
    ./sway.nix
  ];

  services.caffeine.enable = true;
  xsession.importedVariables = ["PATH"];
}
