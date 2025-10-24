{ pkgs, lib, ... }:
{
  mkLint =
    {
      name,
      fileset,
      checkInputs ? [ ],
      script,
    }:
    pkgs.stdenvNoCC.mkDerivation {
      inherit name;

      src = lib.fileset.toSource {
        root = ../.;
        fileset = lib.fileset.difference fileset (
          lib.fileset.unions [
            ../home-config/dotfiles/emacs.d/share/templates
            (lib.fileset.fileFilter (
              file: file.type != "regular" || file.name == "hardware-configuration.nix"
            ) ../.)
          ]
        );
      };

      checkInputs = [ pkgs.nushell ] ++ checkInputs;

      checkPhase = ''
        nu -c '${script}' | tee $out
      '';

      dontPatch = true;
      dontConfigure = true;
      dontBuild = true;
      dontInstall = true;
      dontFixup = true;
      doCheck = true;
    };
}
