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

  home.file.".mozilla/firefox/tlater/chrome/icons" = {
    source = "${pkgs.local.firefox-ui-fix}/icons";
  };

  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      buster-captcha-solver
      clearurls
      no-pdf-download
      react-devtools
      reduxdevtools
      translate-web-pages
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

        # Actual settings
        "app.shield.optoutstudies.enabled" = false;
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
        "browser.newtabpage.activity-stream.showSponsored" = false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
        "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.havePinned" =
          "";
        "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.searchEngines" =
          "";
        "browser.newtabpage.activity-stream.section.highlights.includePocket" =
          false;
        "browser.newtabpage.pinned" = false;
        "browser.protections_panel.infoMessage.seen" = true;
        "browser.quitShortcut.disabled" = true;
        "browser.shell.checkDefaultBrowser" = false;
        "browser.ssb.enabled" = true;
        "browser.toolbars.bookmarks.visibility" = "never";
        "browser.urlbar.placeholderName" = "DuckDuckGo";
        "browser.urlbar.suggest.openpage" = false;
        "datareporting.policy.dataSubmissionPolicyAcceptedVersion" = 2;
        "dom.security.https_only_mode" = true;
        "dom.security.https_only_mode_ever_enabled" = true;
        "extensions.pocket.enabled" = false;
        "identity.fxaccounts.enabled" = false;
        "privacy.trackingprotection.enabled" = true;
        "privacy.trackingprotection.socialtracking.enabled" = true;
      };
    };
  };
}
