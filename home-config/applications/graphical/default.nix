{ flake-inputs, pkgs, ... }:
{
  imports = [ ./librewolf ];

  home.packages = with pkgs; [
    apvlv
    feh
    yubioath-flutter
  ];

  programs.alacritty = {
    enable = true;
    settings.general.import =
      let
        inherit (flake-inputs.self.packages.${pkgs.stdenv.hostPlatform.system}) catppuccin-themes;
      in
      [ "${catppuccin-themes}/share/alacritty/themes/catppuccin-macchiato.toml" ];
  };
}
