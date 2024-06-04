{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [ bibata-cursors ];

  theming.cursor.theme = "Bibata-Original-Ice";

  programs.sway = {
    enable = true;
    package = pkgs.swayfx.overrideAttrs (_: {
      passthru.providedSessions = [ "sway" ];
    });
    wrapperFeatures.gtk = true;
  };

  xdg.portal = {
    enable = true;
    wlr = {
      enable = true;
      settings.screencast = {
        output_name = "DP-1";
        max_fps = 30;
        chooser_type = "simple";
        chooser_cmd = "${pkgs.slurp}/bin/slurp -f %o -or -s '#99d1ce33'";
      };
    };
  };
}
