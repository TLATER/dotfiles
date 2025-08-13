// Open last browser session on startup
pref("browser.startup.page", 3);

// Disable an annoying pop-up
pref("browser.protections_panel.infoMessage.seen", true);

// Don't quit the browser on C-q
pref("browser.quitShortcut.disabled", true);

// Don't show the bookmark toolbar
pref("browser.toolbars.bookmarks.visibility", "never");

// Use OS locale (for time, date, etc.)
pref("intl.regional_prefs.use_os_locales", true);

// Stuff for VAAPI support; See the easyNvidia module, repeated here
// because Firefox policies don't apply to librewolf
pref("media.hardware-video-decoding.force-enabled", true);
pref("media.av1.enabled", true);
pref("gfx.x11-egl.force-enabled", true);
pref("widget.dmabuf.force-enabled", true);

// Generally disable WebGL; this is the default setting, but I
// regularly need to globally enable WebGL. This resets it on browser
// launch.
//
// TODO(tlater): See if
// https://codeberg.org/librewolf/issues/issues/1917 can replace this
pref("webgl.disabled", true);

// I forgot what this does
// pref("browser.urlbar.suggest.openpage", false);

// Don't show the active microphone overlay warning
// pref("privacy.webrtc.legacyGlobalIndicator", false);
