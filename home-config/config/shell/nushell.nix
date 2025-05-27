{ config, ... }:
{
  home.shell.enableNushellIntegration = true;

  programs.nushell = {
    enable = true;
    configFile.source = "${config._dotfiles}/nushell/config.nu";
  };

  programs.carapace.enable = true;
}
