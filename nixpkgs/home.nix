{ pkgs, ... }:

{
  home.packages = with pkgs; [
    alacritty
    dunst
    screen

    (import ./emacs.nix { inherit pkgs; })
  ];

  home.file = {
    ".emacs.d" = {
      onChange = "${pkgs.systemd}/bin/systemctl --user reload emacs.service";
      recursive = true;
      source = ../dotfiles/emacs.d;
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
