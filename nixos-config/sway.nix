{pkgs, ...}: {
  nixpkgs.overlays = [
    (_: prev: {
      # Fix issues with nvidia screencapture bit depth
      # See https://github.com/emersion/xdg-desktop-portal-wlr/issues/190
      # TODO(tlater): stop doing this when there's a new release.
      xdg-desktop-portal-wlr = prev.xdg-desktop-portal-wlr.overrideAttrs (old: {
        src = prev.fetchFromGitHub {
          owner = "emersion";
          repo = old.pname;
          rev = "1eaa02eb18ab783b64dc89f1681909dc30baa805";
          hash = "sha256-vRMNkMFidNmSQkhz5n+EBg7IkRjMYqrhdhM80G3K3WI=";
        };
      });
    })
  ];

  environment.systemPackages = with pkgs; [
    bibata-cursors
  ];

  theming.cursor.theme = "Bibata-Original-Ice";

  programs.sway = {
    enable = true;
    package = pkgs.swayfx.overrideAttrs (_: {passthru.providedSessions = ["sway"];});
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
