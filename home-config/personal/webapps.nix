{
  config,
  pkgs,
  lib,
  flake-inputs,
  ...
}:
let
  inherit ((flake-inputs.nix-webapps.overlays.default pkgs pkgs).nix-webapp-lib) mkFirefoxApp;
in
{
  home.packages = [
    (mkFirefoxApp {
      name = "discord";
      url = "https://discord.com/app";
      firefoxBin = lib.getExe config.programs.librewolf.package;

      makeDesktopItemArgs = {
        comment = pkgs.lib.replaceStrings [ "\n" ] [ " " ] ''
          All-in-one voice and text chat for gamers that's free, secure, and
          works on both your desktop and phone.
        '';
        icon = "discord";
        genericName = "Internet Messenger";
        categories = [
          "Network"
          "InstantMessaging"
        ];
      };

      prefs = {
        "media.gmp-provider.enabled" = true;
        "media.gmp-gmpopenh264.enabled" = true;
        "media.webrtc.hw.h264.enabled" = false;
      };
    })

    (mkFirefoxApp {
      name = "element";
      url = "https://app.element.io";
      firefoxBin = lib.getExe config.programs.librewolf.package;

      makeDesktopItemArgs = {
        icon = "Element";
        mimeTypes = [ "x-scheme-handler/element" ];
        categories = [
          "Network"
          "InstantMessaging"
          "Chat"
          "VideoConference"
        ];
      };
    })

    (mkFirefoxApp {
      name = "whatsapp";
      url = "https://web.whatsapp.com";
      firefoxBin = lib.getExe config.programs.librewolf.package;

      makeDesktopItemArgs = {
        icon = "whatsapp";
        categories = [
          "Network"
          "InstantMessaging"
        ];
      };
    })
  ];
}
