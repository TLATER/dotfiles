{
  stdenvNoCC,
  fetchFromGitHub,
  self',
}:
stdenvNoCC.mkDerivation (drv: {
  pname = "delta-icons";
  version = "2.18.0";

  src = fetchFromGitHub {
    owner = "Delta-Icons";
    repo = "android";
    rev = "v${drv.version}";
    sha256 = "sha256-baylMkuAMQr7A+shF/HvLz5NTzVVpPaOR/8VYKBnZUA=";
  };

  installPhase = ''
    install -D resources/vectors/figma/yu_gi_oh.svg $out/share/icons/delta-icons/scalable/apps/EDOPro.svg
  '';

  passthru.updateScript = self'.builders.nixUpdateScript { packageToUpdate = "delta-icons"; };
})
