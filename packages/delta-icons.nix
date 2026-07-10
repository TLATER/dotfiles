{
  stdenvNoCC,
  fetchFromGitHub,
  self',
}:
stdenvNoCC.mkDerivation (drv: {
  pname = "delta-icons";
  version = "2.17.0";

  src = fetchFromGitHub {
    owner = "Delta-Icons";
    repo = "android";
    rev = "v${drv.version}";
    sha256 = "sha256-oq3C1JFuPVDa9g5fNgwp3s9n5sFz5VOuiNyMkCPAHR8=";
  };

  installPhase = ''
    install -D resources/vectors/figma/yu_gi_oh.svg $out/share/icons/delta-icons/scalable/apps/EDOPro.svg
  '';

  passthru.updateScript = self'.builders.nixUpdateScript { packageToUpdate = "delta-icons"; };
})
