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
      settings.screencast.max_fps = 30;
    };
  };
}
