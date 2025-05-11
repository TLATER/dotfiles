{
  pkgs,
  lib,
  config,
  flake-inputs,
  ...
}:
{
  programs.nushell = {
    enable = true;
    # TODO(tlater): On next home-manager release, the `settings`
    # option will be available, as well as better shellIntegration
    # settings both for nushell and zsh
    configFile.source = "${config._dotfiles}/nushell/config.nu";
    envFile.source = "${config._dotfiles}/nushell/env.nu";

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

  # TODO(tlater): Next home-manager release adds a plugin option
  home.file."${config.xdg.configHome}/nushell/plugin.msgpackz".source =
    let
      plugins = [ flake-inputs.self.packages.${pkgs.system}.nushell-dbus ];
      msgpackz = pkgs.runCommand "nushellMsgpackz" { } ''
        mkdir -p $out

        ${lib.getExe config.programs.nushell.package} \
          --plugin-config "$out/plugin.msgpackz" \
          --commands '${
            lib.concatStringsSep "; " (map (plugin: "plugin add ${lib.getExe plugin}") plugins)
          }'
      '';
    in
    "${msgpackz}/plugin.msgpackz";

  programs.carapace.enable = true;
}
