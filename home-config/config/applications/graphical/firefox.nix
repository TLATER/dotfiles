{
  lib,
  config,
  pkgs,
  flake-inputs,
  ...
}: let
  inherit (pkgs) runCommandNoCC writeText;
  inherit (pkgs.lib.strings) concatStrings;
  inherit (pkgs.lib.attrsets) mapAttrsToList;

  tlaterpkgs = flake-inputs.self.packages.${pkgs.system};
  inherit (flake-inputs.self.packages.${pkgs.system}) firefox-ui-fix;

  settings = writeText "user.js" (concatStrings (mapAttrsToList (
      name: value: ''
        user_pref("${name}", ${builtins.toJSON value});
      ''
    )
    config.programs.firefox.profiles.tlater.settings));

  settings-file = runCommandNoCC "firefox-settings" {} ''
    cat '${firefox-ui-fix}/user.js' '${settings}' > $out
  '';

  thirdParty = config.programs.firefox.enableThirdPartyRepositories;
in {
  options.programs.firefox.enableThirdPartyRepositories = lib.mkEnableOption "third party repositories";

  config = {
    programs.firefox = {
      enable = true;
      package = lib.mkIf thirdParty (pkgs.firefox.override {
        nativeMessagingHosts = [
          pkgs.tridactyl-native
        ];
      });
      profiles."tlater" = {
        extensions = with pkgs.nur.repos.rycee.firefox-addons;
          lib.mkIf thirdParty [
            aria2-integration
            buster-captcha-solver
            clearurls
            decentraleyes
            indie-wiki-buddy
            keepassxc-browser
            libredirect
            no-pdf-download
            react-devtools
            reduxdevtools
            tridactyl
            ublock-origin

            # # Missing:
            # cloudhole
            # devtools-adb-extension
            # firefox-sticky-window-containers
            # warframe-reliquary-prime
          ];

        userChrome =
          lib.mkIf thirdParty
          (builtins.readFile "${firefox-ui-fix}/css/leptonChrome.css");
        userContent =
          lib.mkIf thirdParty
          (builtins.readFile "${firefox-ui-fix}/css/leptonContent.css");
        settings = {
          # Re-bind ctrl to super (would interfere with tridactyl otherwise)
          "ui.key.accelKey" = 91;

          # Keep the reader button enabled at all times; really don't
          # care if it doesn't work 20% of the time, most websites are
          # crap and unreadable without this
          "reader.parse-on-load.force-enabled" = true;

          # Hide the "sharing indicator", it's especially annoying
          # with tiling WMs on wayland
          "privacy.webrtc.legacyGlobalIndicator" = false;

          # Actual settings
          "app.shield.optoutstudies.enabled" = false;
          "app.update.auto" = false;
          "browser.bookmarks.restore_default_bookmarks" = false;
          "browser.contentblocking.category" = "strict";
          "browser.ctrlTab.recentlyUsedOrder" = false;
          "browser.discovery.enabled" = false;
          "browser.laterrun.enabled" = false;
          "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" =
            false;
          "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" =
            false;
          "browser.newtabpage.activity-stream.feeds.snippets" = false;
          "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.havePinned" = "";
          "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.searchEngines" = "";
          "browser.newtabpage.activity-stream.section.highlights.includePocket" =
            false;
          "browser.newtabpage.activity-stream.showSponsored" = false;
          "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
          "browser.newtabpage.pinned" = false;
          "browser.protections_panel.infoMessage.seen" = true;
          "browser.quitShortcut.disabled" = true;
          "browser.shell.checkDefaultBrowser" = false;
          "browser.ssb.enabled" = true;
          "browser.toolbars.bookmarks.visibility" = "never";
          "browser.urlbar.placeholderName" = "DuckDuckGo";
          "browser.urlbar.suggest.openpage" = false;
          "datareporting.policy.dataSubmissionEnable" = false;
          "datareporting.policy.dataSubmissionPolicyAcceptedVersion" = 2;
          "dom.security.https_only_mode" = true;
          "dom.security.https_only_mode_ever_enabled" = true;
          "extensions.getAddons.showPane" = false;
          "extensions.htmlaboutaddons.recommendations.enabled" = false;
          "extensions.pocket.enabled" = false;
          "identity.fxaccounts.enabled" = false;
          "privacy.trackingprotection.enabled" = true;
          "privacy.trackingprotection.socialtracking.enabled" = true;
        };
      };
    };

    home.file = let
      profileDir = ".mozilla/firefox/${config.programs.firefox.profiles.tlater.path}";
    in
      lib.mkIf thirdParty {
        "${profileDir}/user.js".source = settings-file;
        "${profileDir}/icons".source = "${firefox-ui-fix}/icons";
        "${profileDir}/css".source = "${firefox-ui-fix}/css";
      };

    xdg.configFile."tridactyl/tridactylrc" = lib.mkIf thirdParty {
      text = ''
        source ${tlaterpkgs.tridactyl-emacs}/share/tridactyl/emacs_bindings
        # Remove the update function; Really don't want this since it's nix-packaged
        comclear emacs-bindings-update
        # Remove annoying pre-defined "searchurls" - duckduckgo is just better
        jsb Object.keys(tri.config.get("searchurls")).reduce((prev, u) => prev.then(_ => tri.config.set("searchurls", u, null)), Promise.resolve())
      '';
    };
  };
}
