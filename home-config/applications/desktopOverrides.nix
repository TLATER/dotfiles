{ lib, ... }:
let
  disableContents = ''
    [Desktop Entry]
    NoDisplay=true
  '';
in
{
  # It's impossible to override specific attributes without either
  # IFD or overriding the packages (and rebuilding them).
  xdg.dataFile = {
    "applications/emacs.desktop".text = disableContents;
    "applications/xterm.desktop".text = disableContents;
  }
  // (lib.mapAttrs' (p: _: {
    name = "applications/${p}";
    value = {
      source = ./desktopfiles + "/${p}";
    };
  }) (builtins.readDir ./desktopfiles));
}
