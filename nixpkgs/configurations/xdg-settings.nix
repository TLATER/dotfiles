{ config, lib, ... }:

let xdg = config.xdg;
in {
  home.activation = {
    xdg-dir-prep = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD mkdir $VERBOSE_ARG -p '${xdg.cacheHome}/less' '${xdg.cacheHome}/zsh'
    '';
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
