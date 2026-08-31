/**
  Configuration for system-level software that is called directly,
  rather than just used as a service.
*/
{ pkgs, ... }: {
  imports = [ ./programs/nix.nix ];

  documentation.man.cache.enable = true;

  users.defaultUserShell = pkgs.dash;

  services.udisks2.enable = true;

  programs.ssh.enableAskPassword = false;
}
