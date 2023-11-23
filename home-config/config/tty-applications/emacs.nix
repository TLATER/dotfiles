{
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

    # Needed for magit
    git
    git-lfs

    # *.nix files are used to pull in project deps, so we always need these
    alejandra
    nil

    # I just edit shell scripts often enough it makes sense to have
    # this by default
    shellcheck

    # Required for markdown-mode (though could be replaced with a
    # different markdown implementation at some point)
    pandoc

    sqlite.dev
  ];

  xdg.configFile."emacs" = {
    source = emacs.dotfiles;
    recursive = true;
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
