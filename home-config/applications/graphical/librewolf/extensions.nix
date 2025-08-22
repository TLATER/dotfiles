{ config, lib, ... }:
let
  cfg = config.programs.librewolf;
in
{
  options.programs.librewolf.extensions.installByPolicy = lib.mkOption {
    description = ''
      Extensions to install at runtime via policy.
    '';

    type = lib.types.listOf lib.types.str;
    default = [ ];
  };

  config = {
    programs.librewolf = {
      policies = {
        Extensions.Install = map (
          ex: "https://addons.mozilla.org/firefox/downloads/latest/${ex}/latest.xpi"
        ) cfg.extensions.installByPolicy;
      };

      extensions.installByPolicy = lib.mkDefault [
        "canvasblocker"
        "aria2-integration"
        "indie-wiki-buddy"
        "keepassxc-browser"
        "libredirect"
        "ublock-origin"
      ];

      profiles.tlater.extensions = {
        force = true;

        settings = {
          "uBlock0@raymondhill.net".settings = {
            selectedFilterLists = [
              "user-filters"
              "ublock-filters"
              "ublock-badware"
              "ublock-privacy"
              "ublock-quick-fixes"
              "ublock-unbreak"
              "easylist"
              "easyprivacy"
              "LegitimateURLShortener"
              "adguard-spyware-url"
              "urlhaus-1"
              "curben-phishing"
              "plowe-0"
              "fanboy-cookiemonster"
              "ublock-cookies-easylist"
              "fanboy-social"
              "fanboy-thirdparty_social"
              "easylist-chat"
              "easylist-newsletters"
              "easylist-notifications"
              "easylist-annoyances"
              "ublock-annoyances"
            ];
          };

          # Nothing to configure for:
          # - Indie Wiki Buddy
          # - KeePassXC-Browser

          # TODO(tlater): Settings for these add-ons are currently
          # unsupported because of how most add-ons handle settings.
          #
          # All of them are open source, so I could well attempt to add
          # support.
          #
          # See
          # https://github.com/libredirect/browser_extension/issues/905

          # # Aria2 Integration
          # "{e2488817-3d73-4013-850d-b66c5e42d505}" = {
          #   # No settings import/export; need to add context menus,
          #   # User-Agent, and disable sound for the default RPC server
          #   # (as well as simply save the default settings once, since
          #   # the add-on refuses to just try the defaults).
          # };

          # # CanvasBlocker
          # "CanvasBlocker@kkapsner.de".settings = builtins.fromJSON (
          #   builtins.readFile ./extension-settings/canvasblocker.json
          # );

          # # LibRedirect
          # "7esoorv3@alefvanoon.anonaddy.me".settings = builtins.fromJSON (
          #   builtins.readFile ./extension-settings/libredirect.json
          # );
        };
      };
    };
  };
}
