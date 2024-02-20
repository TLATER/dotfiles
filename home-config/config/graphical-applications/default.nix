{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./firefox.nix
    ./discord.nix
    ./element.nix
    ./keepassxc.nix
    ./whatsapp.nix
  ];

  home.packages = with pkgs; [
    alacritty
    apvlv
    feh
    xsel
    yubioath-flutter
  ];

  xdg.configFile."alacritty/alacritty.yml".source = "${config._dotfiles}/alacritty/alacritty.yml";
}
