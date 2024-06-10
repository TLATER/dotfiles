{
  stdenv,
  sources,
  premake5,
}:
stdenv.mkDerivation {
  inherit (sources) pname src;
  TARGET_OS = "linux";

  nativeBuildInputs = [ premake5 ];

  buildPhase = ''
    ./travis/build.sh
  '';
}
