{
  pkgs,
  lib,
  config,
  ...
}:
{
  programs.nushell = {
    enable = true;
    # TODO(tlater): On next home-manager release, the `settings`
    # option will be available, as well as better shellIntegration
    # settings both for nushell and zsh
    configFile.source = "${config._dotfiles}/nushell/config.nu";

    # TODO(tlater): Next home-manager release adds
    # `enableNuShellIntegration` for carapace
    extraConfig = ''
      source ${
        pkgs.runCommand "carapace-nushell-config.nu" { } ''
          ${lib.getExe config.programs.carapace.package} _carapace nushell >> "$out"
        ''
      }
    '';
  };

  programs.carapace.enable = true;
}
