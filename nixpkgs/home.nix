{ pkgs, ... }:

{
  home.packages = with pkgs; [
    alacritty
    dunst
    python37Packages.ipython
    screen

    (import ./emacs.nix { inherit pkgs; })
  ];

  home.file = {
    ".emacs.d" = {
      onChange = "${pkgs.systemd}/bin/systemctl --user reload emacs.service";
      recursive = true;
      source = ../dotfiles/emacs.d;
    };
    ".Xresources".source = ../dotfiles/Xresources;
  };

  xdg.configFile = {
    "dunst/dunstrc".source = ../dotfiles/dunst/dunstrc;
  };

  programs = {
    feh.enable = true;
    git = {
      enable = true;
      userName = "Tristan Daniël Maat";
      userEmail = if isWorkProfile then "tristan.maat@codethink.co.uk" else "tm@tlater.net";
      signing = {
        key = if isWorkProfile then "B32554B9C8BA03ED" else "4CFAC122454FB978";
        signByDefault = true;
      };
    };
    # Rofi is configured in .Xresources
    rofi.enable = true;
  };

  services = {
    # Dunst is configured in ~/.config/dunst/dunstrc
    dunst.enable = true;
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      defaultCacheTtl = 28800;
    };
  };

  systemd.user.services = {
    emacs = {
      Unit = {
        Description = "Emacs: the extensible, self-documenting text editor";
        Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs";
        X-RestartIfChanged = false;
      };
      Service = {
        ExecReload = "${(import ./emacs.nix { inherit pkgs; })}/bin/emacsclient --eval \"(load-file user-init-file)\"";
        ExecStart = "${pkgs.runtimeShell} -l -c 'exec ${(import ./emacs.nix { inherit pkgs; })}/bin/emacs --fg-daemon'";
        ExecStop = "${(import ./emacs.nix { inherit pkgs; })}/bin/emacsclient --eval \"(kill-emacs)\"";
        Restart = "on-failure";
      };
      Install = {
        WantedBy = ["default.target"];
      };
    };
  };

  programs.home-manager.enable = true;
  home.stateVersion = "19.09";
}
