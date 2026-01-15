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
      inherit (flake-inputs.nix-ast-lint.packages.x86_64-linux) nix-ast-lint;
    };

    script = ''
      nix-ast-lint --error --off=empty-let-in
      deadnix --fail **/*.nix
      nixfmt --check --strict **/*.nix
    '';
  };

  lockfile = checkLib.mkLint {
    name = "nix-lockfile";
    fileset = ../flake.lock;
    checkInputs = lib.attrValues { inherit (flake-inputs.flint.packages.x86_64-linux) flint; };

    script = ''
      flint --fail-if-multiple-versions
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
