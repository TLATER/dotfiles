{
  stdenvNoCC,
  fetchFromGitHub,

  localLib,
}:
stdenvNoCC.mkDerivation (drv: {
  pname = "firefox-ui-fix";
  version = "8.7.5";

  src = fetchFromGitHub {
    owner = "black7375";
    repo = "Firefox-UI-Fix";
    rev = "v${drv.version}";
    hash = "sha256-IfR5pI+tpP5RfoTqO6Vgnbc5nADqSA4gg+9csz/+pO0=";
  };

  installPhase = ''
    mkdir -p $out/
    cp -r user.js icons/ css/ $out/
  '';

  passthru.updateScript = localLib.nixUpdateScript { packageToUpdate = "firefox-ui-fix"; };
})
