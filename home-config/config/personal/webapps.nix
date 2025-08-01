{ pkgs, flake-inputs, ... }:
let
  inherit ((flake-inputs.nix-webapps.overlays.default pkgs pkgs).nix-webapp-lib) mkFirefoxApp;
in
{
  home.packages = [
    (mkFirefoxApp {
      name = "discord";
      url = "https://discord.com/app";

      extensions =
        let
          inherit (pkgs.nur.repos.rycee) firefox-addons;
        in
        [
          firefox-addons.ublock-origin
          firefox-addons.decentraleyes
        ];

      makeDesktopItemArgs = {
        comment = pkgs.lib.replaceStrings [ "\n" ] [ " " ] ''
          All-in-one voice and text chat for gamers that's free, secure, and
          works on both your desktop and phone.
        '';
        genericName = "Internet Messenger";
        categories = [
          "Network"
          "InstantMessaging"
        ];
      };
    })

    (mkFirefoxApp {
      name = "element";
      url = "https://app.element.io";

      makeDesktopItemArgs = {
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

      makeDesktopItemArgs = {
        categories = [
          "Network"
          "InstantMessaging"
        ];
      };
    })
  ];
}
