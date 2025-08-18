{
  stdenvNoCC,
  fetchFromGitHub,
  localLib,
}:
stdenvNoCC.mkDerivation (drv: {
  pname = "phosphor-icons";
  version = "2.1.2";

  src = fetchFromGitHub {
    owner = "phosphor-icons";
    repo = "web";
    rev = "v${drv.version}";
    hash = "sha256-96ivFjm0cBhqDKNB50klM7D3fevt8X9Zzm82KkJKMtU=";
  };

  installPhase = ''
    mkdir -p $out/share/fonts/ttf

    for variant in src/*; do
        if [ -d "$variant" ]; then
            install -Dm 444 $variant/*.ttf $out/share/fonts/ttf/
        fi
    done
  '';

  passthru.updateScript = localLib.nixUpdateScript { packageToUpdate = "phosphor-icons"; };
})
