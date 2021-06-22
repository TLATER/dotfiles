{ sources, stdenv, adoptopenjdk-hotspot-bin-15, jre, makeWrapper, wrapGAppsHook
}:

stdenv.mkDerivation rec {
  inherit (sources.gcs) pname version src;

  nativeBuildInputs =
    [ adoptopenjdk-hotspot-bin-15 jre.gtk3 makeWrapper wrapGAppsHook ];

  dontConfigure = true;
  dontInstall = true;

  buildPhase = ''
    mkdir -p out/bootstrap
    javac -d out/bootstrap -encoding UTF8 ./bundler/bundler/Bundler.java
    java -cp out/bootstrap bundler.Bundler --unpackaged
  '';

  preFixup = ''
    mkdir -p $out/{bin,share}
    cp -r out/dist/modules $out/share/java

    makeWrapperArgs+=("''${gappsWrapperArgs[@]}")
    makeWrapper ${jre}/bin/java $out/bin/gcs \
        ''${makeWrapperArgs[@]} \
          --add-flags "-cp $out/share/java/com.lowagie.text-2.1.7.jar:$out/share/java/com.trollworks.gcs-${version}.jar com.trollworks.gcs.GCS"
  '';

  dontWrapGApps = true;
}
