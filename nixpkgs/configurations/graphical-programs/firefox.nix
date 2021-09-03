{ config, pkgs, ... }:

{
  xdg.dataFile."applications/whatsapp.desktop".text = ''
    [Desktop Entry]
    Version=1.0
    Type=Application
    Name=WhatsApp
    Comment=Simple. Secure. Reliable messaging.
    Exec=firefox -ssb https://web.whatsapp.com/
    TryExec=firefox
  '';

  xdg.configFile."tridactyl/tridactylrc".text = ''
    source ${pkgs.local.tridactyl-emacs}/share/tridactyl/emacs_bindings
    # Remove the update function; Really don't want this since it's nix-packaged
    comclear emacs-bindings-update
    # Remove annoying pre-defined "searchurls" - duckduckgo is just better
    jsb Object.keys(tri.config.get("searchurls")).reduce((prev, u) => prev.then(_ => tri.config.set("searchurls", u, null)), Promise.resolve())
  '';

  home.file.".mozilla/firefox/tlater/chrome/icons" = {
    source = "${pkgs.local.firefox-ui-fix}/icons";
  };

  programs.firefox = {
    enable = true;
    package = pkgs.firefox.override { cfg.enableTridactylNative = true; };
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      buster-captcha-solver
      clearurls
      no-pdf-download
      react-devtools
      reduxdevtools
      translate-web-pages
      tridactyl
      ublock-origin
    ];
    profiles."tlater" = {
      userChrome =
        builtins.readFile "${pkgs.local.firefox-ui-fix}/userChrome.css";
      userContent =
        builtins.readFile "${pkgs.local.firefox-ui-fix}/userContent.css";

      settings = {
        # Required for firefox-ui-fix
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "svg.context-properties.content.enabled" = true;
        "layout.css.backdrop-filter.enabled" = true;
        "browser.compactmode.show" = true;

        # Re-bind ctrl to super (would interfere with tridactyl otherwise)
        "ui.key.accelKey" = 91;

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
        "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.havePinned" =
          "";
        "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.searchEngines" =
          "";
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
}
