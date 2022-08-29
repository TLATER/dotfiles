{
  sources,
  lib,
  stdenv,
  jdk17,
  makeWrapper,
  wrapGAppsHook,
  glib,
}:
stdenv.mkDerivation rec {
  inherit (sources.gcs) pname version src;

  nativeBuildInputs = [jdk17 makeWrapper wrapGAppsHook];
  buildInputs = [glib];

  dontConfigure = true;
  dontInstall = true;

  buildPhase = ''
    patchShebangs bundle.sh
    substituteInPlace bundle.sh --replace /bin/rm rm
    ./bundle.sh --unpackaged
  '';

  preFixup = let
    v = lib.removePrefix "v" version;
  in ''
    mkdir -p $out/{bin,share}
    cp -r out/dist/modules $out/share/java

    makeWrapper ${jdk17}/bin/java $out/bin/gcs \
        ''${gappsWrapperArgs[@]} \
          --add-flags "-cp $out/share/java/com.lowagie.text-2.1.7.jar -jar $out/share/java/com.trollworks.gcs-${v}.jar"
  '';

  dontWrapGApps = true;
}
