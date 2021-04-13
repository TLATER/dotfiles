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
    RUSTUP_HOME = "${xdg.dataHome}/rustup";
    XCOMPOSECACHE = "${xdg.cacheHome}/X11/xcompose";
  };
}
