{ flake-inputs }:
let
  inherit (flake-inputs.nixpkgs) lib;
  pkgs = flake-inputs.nixpkgs.legacyPackages.x86_64-linux;
  checkLib = pkgs.callPackage ./lib.nix { };
in
{
  nix = checkLib.mkLint {
    name = "nix-lints";
    fileset = lib.fileset.fileFilter (file: file.hasExt "nix") ../.;

    checkInputs = lib.attrValues {
      inherit (pkgs) deadnix nixfmt-rfc-style;

      statix = pkgs.statix.overrideAttrs (old: {
        patches = old.patches ++ [
          (pkgs.fetchpatch {
            url = "https://github.com/oppiliappan/statix/commit/925dec39bb705acbbe77178b4d658fe1b752abbb.patch";
            hash = "sha256-0wacO6wuYJ4ufN9PGucRVJucFdFFNF+NoHYIrLXsCWs=";
          })
        ];
      });
    };

    script = ''
      statix check **/*.nix
      deadnix --fail **/*.nix
      nixfmt --check --strict **/*.nix
    '';
  };

  lockfile = checkLib.mkLint {
    name = "nix-lockfile";
    fileset = ../flake.lock;

    script = ''
      nu ${./optimal-flake-lock.nu}
    '';
  };

  shell = checkLib.mkLint {
    name = "shell-lints";
    fileset = lib.fileset.unions [
      ../home-config/dotfiles/dashrc
      (lib.fileset.fileFilter (file: file.hasExt "sh") ../.)
    ];

    checkInputs = lib.attrValues { inherit (pkgs) shellcheck; };

    script = ''
      shellcheck ...(glob --no-dir **/*)
    '';
  };
}
