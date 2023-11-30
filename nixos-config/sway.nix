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
          rev = "b5f387821800e32d4f82001504668d819f02bb4b";
          hash = "sha256-TeTDJVFW0NHZpmTm8PAXTzbzk6kolvY7Vd55YQfJV20=";
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
    wlr.enable = true;
  };
}
