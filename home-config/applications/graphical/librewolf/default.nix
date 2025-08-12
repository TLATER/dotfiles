{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [ ./lepton.nix ];

  programs.librewolf = {
    enable = true;

    profiles."tlater" = {
      extensions = {
        packages =
          with pkgs.nur.repos.rycee.firefox-addons;
          lib.mkIf config.allowThirdPartyPackages [
            aria2-integration
            canvasblocker
            indie-wiki-buddy
            keepassxc-browser
            libredirect
          ];
      };

    };
  };

  home.file.".librewolf/librewolf.overrides.cfg".source = ./librewolf.overrides.js;
}
