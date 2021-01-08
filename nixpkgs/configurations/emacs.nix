{ pkgs, ... }:

let
  local-pkgs = import ../local-pkgs { inherit pkgs; };

in
{
  home.packages = with pkgs; [
    # Spell checks
    aspell
    aspellDicts.en
    aspellDicts.en-computers

    # Used for interactive python shells
    nodePackages.bash-language-server
    python37Packages.ipython
    shellcheck
    gnuplot

    # Needed for magit-lfs
    git-lfs
  ];

  xdg.configFile."emacs" = {
    onChange = ''
      # Recompile init files
      SCANNING_PACKAGES=true ${local-pkgs.emacs}/bin/emacs --batch --quick \
        --eval "(byte-recompile-directory user-emacs-directory 0)"
    '';
    recursive = true;
    source = ../../dotfiles/emacs.d;
  };

  programs.emacs = {
    enable = true;
    package = local-pkgs.emacs;
  };

  services.emacs = {
    enable = true;
    client = {
      enable = true;
      arguments = ["--no-wait" "--create-frame"];
    };
  };
}
