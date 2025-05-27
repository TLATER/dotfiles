{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [ bibata-cursors ];

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;

    # The existence of this option is definitely an anti-pattern
    extraPackages = [ ];
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
