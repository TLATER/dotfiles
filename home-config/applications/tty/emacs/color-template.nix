{ melpaBuild, colors }:
melpaBuild {
  pname = "color-template";
  version = "0.1.0";
  src = ./color-template.el;

  inherit (colors)
    base00
    base01
    base02
    base03
    base04
    base05
    base06
    base07
    base08
    base09
    base0A
    base0B
    base0C
    base0D
    base0E
    base0F
    ;

  postPatch = ''
    # Make the color variables available to `substituteAll`
    export $(set | egrep '^base[0-9A-F]{2}=' | sed "s/'//g")
    substituteAll $src color-template.el
  '';
}
