{pkgs, ...}: {
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
