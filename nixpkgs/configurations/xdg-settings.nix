{ config, lib, ... }:

let xdg = config.xdg;
in {
  home.activation = {
    xdg-dir-prep = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
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
  };

  home.sessionVariables = {
    _JAVA_OPTIONS = "-Djava.util.prefs.userRoot='${xdg.configHome}'/java";
    LESSKEY = "${xdg.cacheHome}/less/key";
    LESSHISTFILE = "${xdg.cacheHome}/less/history";
    PYLINTHOME = "${xdg.cacheHome}/pylint";
    CARGO_HOME = "${xdg.cacheHome}/cargo";
    RUSTUP_HOME = "${xdg.dataHome}/rustup";
    XCOMPOSECACHE = "${xdg.cacheHome}/X11/xcompose";
    XCOMPOSEFILE = "${xdg.configHome}X11/xcompose";
    NPM_CONFIG_USERCONFIG = "${xdg.configHome}/npm/npmrc";
    MAILCAPS = "${xdg.configHome}/mailcap";

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
  };

  # More hacks to avoid stupid package managers puking all over my
  # $HOME, for this one I even need a permanent file in ~/.config!
  xdg.configFile."npm/npmrc".text = ''
    prefix=${xdg.cacheHome}/npm
    cache=${xdg.cacheHome}/npm
    tmp=$XDG_RUNTIME_DIR/npm
  '';
}
