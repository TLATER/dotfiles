{
  stdenv,
  sources,

  premake5,

  luacxx,
}:
stdenv.mkDerivation {
  inherit (sources.ocgcore) pname version src;

  nativeBuildInputs = [ premake5 ];

  buildInputs = [ luacxx ];

  buildFlags = [ "verbose=true config=release ocgcoreshared" ];

  # We don't want to link against the vendored lua
  prePatch = ''
    sed 's|include "./lua/"||' -i premake5.lua
  '';

  preBuild = ''
    premake5 gmake2
    cd build
  '';

  installPhase = ''
    install -Dt $out/lib/ ../bin/release/libocgcore.so
  '';
}
