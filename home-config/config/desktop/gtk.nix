{ pkgs, ... }:
{
  gtk = {
    enable = true;

    font = {
      package = pkgs.noto-fonts;
      name = "NotoSans";
    };

    theme = {
      package = pkgs.magnetic-catppuccin-gtk;
      name = "Catppuccin-GTK-Dark";
    };

    cursorTheme = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Original-Ice";
      size = 24;
    };

    iconTheme = {
      package = pkgs.symlinkJoin {
        name = "papirus-dark++";
        paths = [ pkgs.papirus-icon-theme ];
        # TODO: Find appropriate icons
        postBuild = ''
          ln -s $out/share/icons/Papirus-Dark/24x24/apps/ $out/share/icons/Papirus-Dark/24x24/apps/btm.svg
          ln -s $out/share/icons/Papirus-Dark/24x24/apps/ $out/share/icons/Papirus-Dark/24x24/apps/EDOPro.svg
          ln -s $out/share/icons/Papirus-Dark/24x24/apps/ $out/share/icons/Papirus-Dark/24x24/apps/gcs.svg
          ln -s $out/share/icons/Papirus-Dark/24x24/apps/ $out/share/icons/Papirus-Dark/24x24/apps/Jazz2.svg
          ln -s $out/share/icons/Papirus-Dark/24x24/apps/ $out/share/icons/Papirus-Dark/24x24/apps/otd.svg
        '';
      };
      name = "Papirus-Dark";
    };
  };
}
