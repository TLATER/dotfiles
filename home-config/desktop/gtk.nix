{ flake-inputs, pkgs, ... }:
{
  dconf.settings."org/gnome/desktop/interface" = {
    color-scheme = "prefer-dark";
  };

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
        postBuild =
          let
            edopro = "${
              flake-inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.delta-icons
            }/share/icons/delta-icons/scalable/apps/EDOPro.svg";
          in
          ''
            ln -s $out/share/icons/Papirus-Dark/24x24/apps/utilities-system-monitor.svg \
              $out/share/icons/Papirus-Dark/24x24/apps/btm.svg
            ln -s ${edopro} \
              $out/share/icons/Papirus-Dark/24x24/apps/EDOPro.svg
            ln -s $out/share/icons/Papirus-Dark/24x24/devices/input-tablet.svg \
              $out/share/icons/Papirus-Dark/24x24/apps/otd.svg

            # TODO(tlater): Find more appropriate icons for these two
            # ln -s - \
            #   $out/share/icons/Papirus-Dark/24x24/apps/gcs.svg
            # ln -s - \
            #   $out/share/icons/Papirus-Dark/24x24/apps/Jazz2.svg
          '';
      };
      name = "Papirus-Dark";
    };
  };
}
