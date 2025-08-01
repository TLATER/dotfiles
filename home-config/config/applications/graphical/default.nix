{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./firefox.nix
    ./keepassxc.nix
  ];

  home.packages = with pkgs; [
    apvlv
    feh
    xsel
    yubioath-flutter
  ];

  programs.alacritty =
    let
      inherit (config.lib.stylix.colors.withHashtag)
        base10
        base11
        base01
        base02
        base04
        base06
        ;
    in
    {
      enable = true;
      # We don't want stylix to apply its fonts, we use fontconfig for
      # that
      settings = {
        font = lib.mkForce { };

        colors.indexed_colors = [
          {
            index = 16;
            color = base11;
          }
          {
            index = 17;
            color = base10;
          }
          {
            index = 18;
            color = base01;
          }
          {
            index = 19;
            color = base02;
          }
          {
            index = 20;
            color = base04;
          }
          {
            index = 21;
            color = base06;
          }
        ];
      };
    };
  stylix.targets.alacritty.enable = true;
}
