{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  inherit ((inputs.nix-webapps.overlays.default pkgs pkgs).nix-webapp-lib) mkFirefoxApp;
in
{
  imports = [ ./syncthing.nix ];

  services.syncthing.settings.options = {
    # Use tailscale to set up mesh networking, and disable all
    # built-in networking.
    listenAddresses = [
      "tcp://100.64.0.5:22000"
      "quic://100.64.0.5:22000"
    ];
    globalAnnounceEnabled = false;
    localAnnounceEnabled = false;
    relaysEnabled = false;
    natEnabled = false;
    urAccepted = (-1);
    announceLANAddresses = false;
    stunKeepaliveStartS = 0;
  };

  home.packages = [
    (mkFirefoxApp {
      name = "syncthing";
      url = "http://127.0.0.1:8384";
      firefoxBin = lib.getExe config.programs.librewolf.package;

      makeDesktopItemArgs = {
        comment = pkgs.syncthing.meta.description;
        icon = "${pkgs.syncthing}/share/icons/hicolor/scalable/apps/syncthing.svg";
      };
    })
  ];
}
