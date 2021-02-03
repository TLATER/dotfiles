{ pkgs, use-xdotool ? true }:

with pkgs;

stdenv.mkDerivation {
  pname = "pass-rofi";
  version = "1.0";
  src = ../../dotfiles/bin;
  installPhase = ''
    mkdir -p $out/bin
    install pass-rofi $out/bin
  '';
  postFixup = ''
    [ ${
      toString use-xdotool
    } == "1" ] && sed -i 's|xdotool|${xdotool}/bin/xdotool|' "$out/bin/pass-rofi"
    sed -i 's|rofi|${rofi}/bin/rofi|' "$out/bin/pass-rofi"
    sed -i 's|pass show|${pass}/bin/pass show|' "$out/bin/pass-rofi"
  '';
  propagatedBuildInputs = [ rofi pass ]
    ++ stdenv.lib.optional (use-xdotool) xdotool;
}
