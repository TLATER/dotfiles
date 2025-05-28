{
  sources,
  stdenv,
  lib,
  autoPatchelfHook,
  dmidecode,
  glib,
  glib-networking,
  libsoup_2_4,
  rpmextract,
  wrapGAppsHook,
}:
stdenv.mkDerivation (finalAttrs: {
  inherit (sources.drivestrike) pname version src;

  nativeBuildInputs = [
    autoPatchelfHook
    wrapGAppsHook
    glib
    glib-networking
    rpmextract
  ];
  buildInputs = [ libsoup_2_4 ];

  unpackCmd = ''
    mkdir ${finalAttrs.pname}-${finalAttrs.version} && pushd ${finalAttrs.pname}-${finalAttrs.version}
    rpmextract $curSrc
    popd
  '';

  postPatch = ''
    substituteInPlace lib/systemd/system/drivestrike.service \
      --replace "/usr/bin/drivestrike" "$out/bin/drivestrike"
  '';

  preFixup = ''
    gappsWrapperArgs+=(
      --prefix PATH : ${lib.makeBinPath [ dmidecode ]}
    )
  '';

  installPhase = ''
    install -D usr/bin/drivestrike $out/bin/drivestrike
    install -D lib/systemd/system/drivestrike.service $out/lib/systemd/drivestrike.service
  '';

  # To register, use:
  #
  # ```console
  # # drivestrike register <registration code> "" https://app.drivestrike.com/svc/
  # ```
})
