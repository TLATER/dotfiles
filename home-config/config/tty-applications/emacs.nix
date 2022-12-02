{
  config,
  pkgs,
  flake-inputs,
  ...
}: let
  inherit (flake-inputs.self.packages.${pkgs.system}) emacs;
in {
  home.packages = with pkgs; [
    # Spell checks
    (aspellWithDicts (dicts: with dicts; [af de en en-computers nl]))

    # Used for interactive python shells
    python3Packages.ipython

    # Needed for magit-lfs
    git-lfs

    # *.nix files are used to pull in project deps, so we always need these
    alejandra
    nil

    # I just edit shell scripts often enough it makes sense to have
    # this by default
    shellcheck
  ];

  xdg.configFile."emacs" = {
    onChange = ''
      # Recompile init files
      SCANNING_PACKAGES=true ${emacs}/bin/emacs --batch --quick \
        --eval "(byte-recompile-directory user-emacs-directory 0)"
    '';
    recursive = true;
    source = "${config._dotfiles}/emacs.d";
  };

  programs.emacs = {
    enable = true;
    package = emacs;
  };

  services.emacs = {
    enable = true;
    client = {
      enable = true;
      arguments = ["--no-wait" "--create-frame"];
    };
    socketActivation.enable = true;
  };
}
