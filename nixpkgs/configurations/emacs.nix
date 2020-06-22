{ pkgs, ... }:

let
  local-pkgs = import ../local-pkgs { inherit pkgs; };

in
{
  home.packages = with pkgs; [
    local-pkgs.emacs

    # Spell checks
    aspell
    aspellDicts.en
    aspellDicts.en-computers

    # Used for interactive python shells
    nodePackages.bash-language-server
    python37Packages.ipython
    shellcheck
    gnuplot
  ];

  home.file.".emacs.d" = {
    onChange = ''
      # Recompile init files
      SCANNING_PACKAGES=true ${local-pkgs.emacs}/bin/emacs --batch --quick \
        --eval "(byte-recompile-directory user-emacs-directory 0)"

      # Then reload the running emacs config, if any
      ${pkgs.systemd}/bin/systemctl --user reload emacs.service
    '';
    recursive = true;
    source = ../../dotfiles/emacs.d;
  };

  # We need to replace the normal systemd user service with this one
  # so that we can properly reload the service when the files are
  # updated.
  #
  # FIXME: A PR would do nicely.
  systemd.user.services = {
    emacs = {
      Unit = {
        Description = "Emacs: the extensible, self-documenting text editor";
        Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs";
        X-RestartIfChanged = false;
      };
      Service = {
        ExecReload = "${local-pkgs.emacs}/bin/emacsclient --eval \"(load-file user-init-file)\"";
        ExecStart = "${pkgs.runtimeShell} -l -c 'exec ${local-pkgs.emacs}/bin/emacs --fg-daemon'";
        ExecStop = "${local-pkgs.emacs}/bin/emacsclient --eval \"(kill-emacs)\"";
        Restart = "on-failure";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };
}
