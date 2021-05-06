{ pkgs, dotroot, ... }:

{
  home.packages = with pkgs; [
    # Spell checks
    aspell
    aspellDicts.en
    aspellDicts.en-computers

    # Used for interactive python shells
    python37Packages.ipython

    # Needed for magit-lfs
    git-lfs
  ];

  xdg.configFile."emacs" = {
    onChange = ''
      # Recompile init files
      SCANNING_PACKAGES=true ${pkgs.local.emacs}/bin/emacs --batch --quick \
        --eval "(byte-recompile-directory user-emacs-directory 0)"
    '';
    recursive = true;
    source = "${dotroot}/dotfiles/emacs.d";
  };

  programs.emacs = {
    enable = true;
    package = pkgs.local.emacs;
  };

  services.emacs = {
    enable = true;
    client = {
      enable = true;
      arguments = [ "--no-wait" "--create-frame" ];
    };
    socketActivation.enable = true;
  };
}
