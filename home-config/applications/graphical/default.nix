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
        inherit (flake-inputs.self.packages.${pkgs.system}) catppuccin-alacritty;
      in
      [ "${catppuccin-alacritty}/share/alacritty/themes/catppuccin-macchiato.toml" ];
  };
}
