/**
  Module to fix XDG base dir compliance issues in various software I use.

  TODO(tlater): Write something that runs xdg-ninja every once in a while.
*/
{
  hjem.extraModules = [
    (
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        environment.sessionVariables = {
          # Cache
          CARGO_HOME = "${config.xdg.cache.directory}/cargo";
          CUDA_CACHE_PATH = "${config.xdg.cache.directory}/cuda";
          GRADLE_USER_HOME = "${config.xdg.cache.directory}/gradle";
          EM_CACHE = "${config.xdg.cache.directory}/emscripten"; # WASM compiler
          EM_PORTS = "${config.xdg.cache.directory}/emscripten"; # WASM compiler

          # Config
          DOCKER_CONFIG = "${config.xdg.config.directory}/docker";
          EM_CONFIG = "${config.xdg.config.directory}/emscripten"; # WASM compiler

          # State
          BUILDX_CONFIG = "${config.xdg.state.directory}/docker-buildx";
          IPYTHONDIR = "${config.xdg.state.directory}/ipython";
          RLWRAP = "${config.xdg.state.directory}/rlwrap";

          # Disable
          HISTFILE = "/dev/null";

          # Unorthodox
          _JAVA_OPTIONS = lib.concatStringsSep " " [
            "-Djava.util.prefs.userRoot=${config.xdg.config.directory}/java"
            "-Djavafx.cachedir=${config.xdg.cache.directory}/openjfx"
          ];

          NPM_CONFIG_USERCONFIG = pkgs.writeText "npmrc" ''
            prefix=${config.xdg.cache.directory}/npm
            cache=${config.xdg.cache.directory}/npm
            tmp=$XDG_RUNTIME_DIR/npm
            init-module=${config.xdg.config.directory}/npm/config/npm-init.js
          '';

          PYTHONSTARTUP = "${./home-config/dotfiles/bin/history.py}";
        };
      }
    )
  ];
}
