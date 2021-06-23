{ sources, lib, stdenv, adoptopenjdk-hotspot-bin-15, jre, makeWrapper
, wrapGAppsHook }:

stdenv.mkDerivation rec {
  inherit (sources.gcs) pname version src;

  nativeBuildInputs =
    [ adoptopenjdk-hotspot-bin-15 jre.gtk3 makeWrapper wrapGAppsHook ];

  dontConfigure = true;
  dontInstall = true;

  buildPhase = ''
    patchShebangs bundle.sh
    substituteInPlace bundle.sh --replace /bin/rm rm
    ./bundle.sh --unpackaged
  '';

  preFixup = let v = lib.removePrefix "v" version;
  in ''
    mkdir -p $out/{bin,share}
    cp -r out/dist/modules $out/share/java

    makeWrapperArgs+=("''${gappsWrapperArgs[@]}")
    makeWrapper ${jre}/bin/java $out/bin/gcs \
        ''${makeWrapperArgs[@]} \
          --add-flags "-cp $out/share/java/com.lowagie.text-2.1.7.jar -jar $out/share/java/com.trollworks.gcs-${v}.jar"
  '';

  dontWrapGApps = true;
}
