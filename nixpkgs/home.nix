{ pkgs, ... }:

{
  home.packages = with pkgs; [
    alacritty
    dunst
    screen

    (import ./emacs.nix { inherit pkgs; })
  ];

  systemd.user.services = {
    emacs = {
      Unit = {
        Description = "Emacs text editor";
        Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs";
      };
      Service = {
        ExecStart = "${(import ./emacs.nix { inherit pkgs; })}/bin/emacs --fg-daemon";
        ExecStop = "${(import ./emacs.nix { inherit pkgs; })}/bin/emacsclient --eval \"(kill-emacs)\"";
        Environment = "SSH_AUTH_SOCK=%t/keyring/ssh";
      };
      Install = {
        WantedBy = ["default.target"];
      };
    };
  };

  programs.home-manager.enable = true;
  home.stateVersion = "19.09";
}
