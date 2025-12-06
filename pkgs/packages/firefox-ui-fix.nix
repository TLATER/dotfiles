{
  stdenvNoCC,
  fetchFromGitHub,

  localLib,
}:
stdenvNoCC.mkDerivation (drv: {
  pname = "firefox-ui-fix";
  version = "8.7.4";

  src = fetchFromGitHub {
    owner = "black7375";
    repo = "Firefox-UI-Fix";
    rev = "v${drv.version}";
    hash = "sha256-YG8C1FgXZHdG4K7xs44paOWuOr256S8Z2dCPA1MhxUo=";
  };

  installPhase = ''
    mkdir -p $out/
    cp -r user.js icons/ css/ $out/
  '';

  passthru.updateScript = localLib.nixUpdateScript { packageToUpdate = "firefox-ui-fix"; };
})
