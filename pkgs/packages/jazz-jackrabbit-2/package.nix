{
  location ? "~/.local/share/wine-prefixes/jjr2",
  protonPath ? "${proton-ge-bin.steamcompattool}/",
  umu-launcher ? flake-inputs.nixpkgs-unstable.legacyPackages.${system}.umu-launcher,

  flake-inputs,
  system,
  lib,

  proton-ge-bin,
  innoextract,
  icoutils,

  requireFile,
  writers,
  makeDesktopItem,
  symlinkJoin,
  runCommand,
}:
let
  secret_files_installer = requireFile {
    name = "setup_jazz_jackrabbit_2.exe";
    url = "https://www.gog.com/downloads/jazz_jackrabbit_2_secret_files/95627";
    sha256 = "1w67046xg1xnvh9hj11mnkmpzjdma7dcr9qpqih4fd2jwgl6h8w2";
  };

  christmas_chronicles =
    let
      src = requireFile {
        name = "setup_jazz_jackrabbit_2_cc.exe";
        url = "https://www.gog.com/downloads/jazz_jackrabbit_2_christmas_chronicles/en1installer0";
        sha256 = "10q3mv2h9ad0503aq19ia4bqw1jmprisgd7vz3m40bbc6v9myfw3";
      };
    in
    runCommand "christmas-chronicles" { nativeBuildInputs = [ innoextract ]; } ''
      innoextract --silent --exclude-temp --gog --output-dir cc ${src}
      install -D --target-directory $out/share/jj2/ cc/*.j2{l,t,b,e}
    '';

  icons =
    runCommand "jj2-icons"
      {
        nativeBuildInputs = [
          innoextract
          icoutils
        ];
      }
      ''
        innoextract --exclude-temp --gog --output-dir sf ${secret_files_installer}

        icotool -x sf/app/goggame-1351891846.ico

        for icon in *.png; do
            size="''${icon##*_}"
            size="''${size%x*}"

            install -D $icon "$out/share/icons/hicolor/$size/apps/Jazz2.png"
        done
      '';

  script =
    writers.writeNuBin "jazz-jackrabbit-2"
      {
        makeWrapperArgs = [
          "--prefix"
          "PATH"
          ":"
          "${lib.makeBinPath [
            umu-launcher
          ]}"
        ];
      }
      ''
        $env.WINEPREFIX = '${location}' | path expand
        $env.WINEARCH = 'win32'
        $env.PROTONPATH = '${protonPath}'
        $env.PROTON_VERBS = "waitforexitandrun"

        # umu-laucher vars
        # TODO(tlater): Figure out what to do with a numeric non-steam ID
        $env.GAMEID = "umu-1351891846"
        $env.STORE = "gog"

        let jj2_path = $env.WINEPREFIX | path join 'drive_c/GOG Games/Jazz Jackrabbit 2/'

        if not ($jj2_path | path exists) {
          umu-run '${secret_files_installer}'

          let installed_files = ls -s $jj2_path

          let cc_exclusives = (ls '${christmas_chronicles}/share/jj2/' |
            where {|f| ($f.name | path parse).extension in [j2l, j2t, j2b, j2e]} |
            where {|f| ($f.name | path basename) not-in ($installed_files | $in.name)})

          $cc_exclusives | each {|f| cp $f.name $jj2_path}
        }

        def main [exe?: string] {
          exec umu-run ($jj2_path | path join ($exe | default 'Jazz2.exe'))
        }
      '';

  jazz = makeDesktopItem {
    name = "jazz-jackrabbit-2";
    desktopName = "Jazz Jackrabbit 2";
    exec = "${script}/bin/jazz-jackrabbit-2 Jazz2.exe";
    icon = "Jazz2";
    categories = [
      "Game"
      "ArcadeGame"
    ];
  };

  jcs = makeDesktopItem {
    name = "jcs";
    desktopName = "Jazz Creation Station";
    exec = "${script}/bin/jazz-jackrabbit-2 Jcs.exe";
    icon = "Jazz2";
    categories = [
      "Game"
      "ArcadeGame"
    ];
  };
in
symlinkJoin {
  name = "jazz-jackrabbit-2";

  paths = [
    icons
    jazz
    jcs
    script
  ];
}
