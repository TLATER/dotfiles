{
  edopro-unwrapped,

  stdenv,
  premake5,

  luacxx,
}:
stdenv.mkDerivation (drv: {
  inherit (edopro-unwrapped) src;
  sourceRoot = "${drv.src.name}/ocgcore";

  pname = "ocgcore";
  version = drv.src.rev;

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
})
