{ pkgs, ... }:
{
  home.shell.enableNushellIntegration = true;

  programs = {
    nushell = {
      enable = true;
      configFile.source = ../dotfiles/nushell/config.nu;

      plugins = [ pkgs.nushellPlugins.query ];
    };

    carapace.enable = true;
  };
}
