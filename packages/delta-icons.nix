{
  stdenvNoCC,
  fetchFromGitHub,
  self',
}:
stdenvNoCC.mkDerivation (drv: {
  pname = "delta-icons";
  version = "2.15.0";

  src = fetchFromGitHub {
    owner = "Delta-Icons";
    repo = "android";
    rev = "v${drv.version}";
    sha256 = "sha256-LasVhKUnlu7e7ZthDRVFX1Hmn75U+i6w6wNGGpcJO6U=";
  };

  installPhase = ''
    install -D resources/vectors/figma/yu_gi_oh.svg $out/share/icons/delta-icons/scalable/apps/EDOPro.svg
  '';

  passthru.updateScript = self'.builders.nixUpdateScript { packageToUpdate = "delta-icons"; };
})
