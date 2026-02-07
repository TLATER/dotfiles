{
  stdenvNoCC,
  fetchFromGitHub,
  localLib,
}:
stdenvNoCC.mkDerivation (drv: {
  pname = "delta-icons";
  version = "2.12.0";

  src = fetchFromGitHub {
    owner = "Delta-Icons";
    repo = "android";
    rev = "v${drv.version}";
    sha256 = "sha256-rmEeDfR15hxcmbIx2cumdvG/EP8qpkzw1H2Bm4eawms=";
  };

  installPhase = ''
    install -D resources/vectors/figma/yu_gi_oh.svg $out/share/icons/delta-icons/scalable/apps/EDOPro.svg
  '';

  passthru.updateScript = localLib.nixUpdateScript { packageToUpdate = "delta-icons"; };
})
