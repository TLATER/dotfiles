{ pkgs, inputs, ... }:
let
  inherit (inputs) self;
  inherit (inputs.self.packages.${pkgs.stdenv.hostPlatform.system}) emacs;
in
{
  disabledModules = [ "misc/ssh-auth-sock.nix" ];
  imports = [ "${inputs.home-manager-fix-ssh-socket}/modules/misc/ssh-auth-sock.nix" ];

  sshAuthSock = {
    enable = true;
    systemd.socketProviderUnit = "gpg-agent-ssh.socket";
  };

  xdg.configFile."emacs".source = "${self}/home-config/dotfiles/emacs.d/";

  programs.emacs = {
    enable = true;
    package = emacs;
  };

  services.emacs = {
    enable = true;
    client = {
      enable = true;
      arguments = [
        "--no-wait"
        "--create-frame"
      ];
    };
    socketActivation.enable = true;
  };

  systemd.user.services.emacs.Unit = {
    After = [ "set-SSH_AUTH_SOCK.service" ];
    Requires = [ "set-SSH_AUTH_SOCK.service" ];
  };
}
