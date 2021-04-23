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

  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      buster-captcha-solver
      clearurls
      no-pdf-download
      react-devtools
      reduxdevtools
      ublock-origin
    ];
    profiles."tlater" = {
      settings = {
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
        "extensions.pocket.enabled" = false;
        "privacy.trackingprotection.enabled" = true;
        "privacy.trackingprotection.socialtracking.enabled" = true;
      };
    };
  };
}
