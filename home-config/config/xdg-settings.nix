{
  pkgs,
  config,
  lib,
  ...
}: let
  inherit (pkgs) writeText;
  inherit (lib.strings) concatStringsSep;
  inherit (config) xdg;
in {
  home.activation = {
    xdg-dir-prep = lib.hm.dag.entryAfter ["writeBoundary"] ''
      $DRY_RUN_CMD mkdir $VERBOSE_ARG -p '${xdg.cacheHome}/less' '${xdg.cacheHome}/zsh'
    '';
  };

  xdg.userDirs = {
    enable = true;
    # Work around firefox creating a "Desktop" directory
    desktop = "${config.home.homeDirectory}";
    download = "${config.home.homeDirectory}/Downloads";
    documents = "${config.home.homeDirectory}/Documents";
    music = "${config.xdg.userDirs.documents}/Music";
    videos = "${config.xdg.userDirs.documents}/Videos";
    pictures = "${config.xdg.userDirs.documents}/Pictures";
    publicShare = "${config.xdg.userDirs.documents}/Public";
    templates = "${config.xdg.userDirs.documents}/Templates";
    extraConfig = {
      XDG_SCREENSHOTS_DIR = "${config.xdg.userDirs.pictures}/Screenshots";
    };
  };

  home.sessionVariables = {
    _JAVA_OPTIONS = concatStringsSep " " [
      "-Djava.util.prefs.userRoot='${xdg.configHome}'/java"
      "-Djavafx.cachedir='${xdg.cacheHome}/openjfx'"
    ];
    LESSKEY = "${xdg.cacheHome}/less/key";
    LESSHISTFILE = "${xdg.cacheHome}/less/history";
    PYLINTHOME = "${xdg.cacheHome}/pylint";
    CARGO_HOME = "${xdg.cacheHome}/cargo";
    RUSTUP_HOME = "${xdg.dataHome}/rustup";
    XCOMPOSECACHE = "${xdg.cacheHome}/X11/xcompose";
    XCOMPOSEFILE = "${xdg.configHome}/X11/xcompose";
    MAILCAPS = "${xdg.configHome}/mailcap";
    IPYTHONDIR = "${xdg.dataHome}/ipython";
    JUPYTER_CONFIG_DIR = "${xdg.dataHome}/ipython";
    HISTFILE = "${xdg.dataHome}/histfile";
    RLWRAP_HOME = "${xdg.dataHome}/rlwrap"; # stumpish and perhaps others
    CUDA_CACHE_PATH = "${xdg.dataHome}/cuda";

    # See, this is exactly why things should follow the spec. I have
    # no intention of using gradle ever, but occasionally I need to
    # build software that uses it.
    #
    # Now I need to deal with gradle puking directories all over my
    # file system, or have a permanent configuration option here for
    # software I don't even use.
    #
    # Grmbl.
    GRADLE_USER_HOME = "${xdg.cacheHome}/gradle";

    NPM_CONFIG_USERCONFIG = writeText "npmrc" ''
      prefix=${xdg.cacheHome}/npm
      cache=${xdg.cacheHome}/npm
      tmp=$XDG_RUNTIME_DIR/npm
      init-module=${xdg.configHome}/npm/config/npm-init.js
    '';

    PYTHONSTARTUP = "${config._dotfiles}/bin/history.py";
  };
}
