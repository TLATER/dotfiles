{
  stdenvNoCC,
  fetchFromGitHub,

  localLib,
}:
stdenvNoCC.mkDerivation (drv: {
  pname = "firefox-ui-fix";
  version = "8.7.3";

  src = fetchFromGitHub {
    owner = "black7375";
    repo = "Firefox-UI-Fix";
    rev = "v${drv.version}";
    hash = "sha256-2AIUzfWp7RhhW5Ku1qYTxr0y+1qpfCIHPVv3wdI2VyU=";
  };

  installPhase = ''
    mkdir -p $out/
    cp -r user.js icons/ css/ $out/
  '';

  passthru.updateScript = localLib.nixUpdateScript { packageToUpdate = "firefox-ui-fix"; };
})
