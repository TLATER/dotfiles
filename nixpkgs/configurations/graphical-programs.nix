{ config, pkgs, ... }:

let
  local-pkgs = import ../local-pkgs { inherit pkgs; };

in
{
  home.packages = with pkgs; [
    local-pkgs.cap

    webmacs
  ];

  xdg.dataFile."applications/webmacs.desktop".text = ''
    [Desktop Entry]
    Version=1.0
    Type=Application
    Name=webmacs
    Comment=webmacs - keyboard driven (emacs key bindings) browser, https://webmacs.readthedocs.io/en/latest/
    TryExec=webmacs
    Exec=webmacs
  '';
}
