{
  pkgs,
  lib,
  config,
  flake-inputs,
  ...
}:
{
  environment.systemPackages = with pkgs; [ bibata-cursors ];

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;

    # The existence of this option is definitely an anti-pattern
    extraPackages = [ ];

    package = lib.mkIf config.easyNvidia.enable (
      pkgs.sway.override {
        inherit (flake-inputs.nixpkgs-wayland.packages.${pkgs.system}) sway-unwrapped;
      }
    );
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
