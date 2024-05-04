{ sources, stdenv }:
stdenv.mkDerivation {
  inherit (sources.tridactyl-emacs) pname version src;
  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/tridactyl/
    cp emacs_bindings $out/share/tridactyl/
  '';
}
