{
  unzip,
  fetchurl,
  sources,
  buildGoModule,
  buildNpmPackage,
  pkg-config,
  libGL,
  fontconfig,
  freetype,
  mupdf,
  xorg,
}:
let
  frontend = buildNpmPackage {
    inherit (sources.gcs) pname version src;
    sourceRoot = "source/server/frontend";
    npmDepsHash = "sha256-6hKrp6hbDqPpKXlzS7nvosDBiadPWKl6WSQ2J8+8WPY=";
    postInstall = ''
      cp -r dist $out
    '';
  };
  pdfjs =
    let
      version = "4.3.136";
    in
    fetchurl {
      url = "https://github.com/mozilla/pdf.js/releases/download/v${version}/pdfjs-${version}-dist.zip";
      hash = "sha256-L0VFmQGUGKmj691KZIPc9LdrpsZwVxqias+wyrVLOXs=";
    };
in
buildGoModule {
  inherit (sources.gcs) pname version src;

  overrideModAttrs = _: {
    postBuild = ''
      sed 's/lmupdf_linux_amd64/lmupdf/' -i ./vendor/github.com/richardwilkes/pdf/pdf.go
    '';
  };

  RELEASE = sources.gcs.version;
  DIST = 1;

  ldflags = [
    "-X"
    "github.com/richardwilkes/toolbox/cmdline.AppVersion=${sources.gcs.version}"
    "-s"
    "-w"
  ];

  nativeBuildInputs = [
    pkg-config
    unzip
  ];

  buildInputs = [
    libGL
    fontconfig
    freetype
    mupdf

    xorg.libX11
    xorg.libXcursor
    xorg.libXrandr
    xorg.libXinerama
    xorg.libXi
    xorg.libXxf86vm
  ];

  preBuild = ''
    cp -r ${frontend}/dist server/frontend/dist
    # Use a pdfjs zip downloaded by nix instead
    sed 's|/bin/rm|rm|' -i server/pdf/refresh-pdf.js.sh
    sed '/curl/c\cp ${pdfjs} ''${ARCHIVE}' -i server/pdf/refresh-pdf.js.sh
    (cd server/pdf && bash refresh-pdf.js.sh)
  '';

  postInstall =
    let
      # TODO(tlater): Add mimetypes
      desktop = ''
        [Desktop Entry]
        Name=GCS
        Comment=GCS (GURPS Character Sheet) is a stand-alone, interactive, character sheet editor that allows you to build characters for the GURPS 4th Edition roleplaying game system.
        Exec=gcs
        Icon=gcs
        Terminal=false
        Type=Application
        Catgories=Game;Utility;RolePlaying
      '';
    in
    ''
      mkdir -p $out/share/{applications,icons/hicolor/1024x1024/apps}
      echo -n '${desktop}' > $out/share/applications/com.trollworks.gcs.desktop
      cp packaging/internal/embedded/app-1024.png $out/share/icons/hicolor/1024x1024/apps/gcs.png

      rm $out/bin/{gen,packaging,scr}
    '';

  vendorHash = "sha256-1K8m2NCsQbkFHC9Lx+lNS8YP3PFKwNryO5qz0N/m0Ck=";
  meta.mainProgram = "${sources.gcs.pname}";
}
