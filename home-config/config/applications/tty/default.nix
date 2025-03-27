{
  pkgs,
  lib,
  flake-inputs,
  ...
}:
{
  imports = [ ./emacs.nix ];

  programs.bottom = {
    enable = true;
    settings.flags.group_processes = true;
  };

  programs.git.extraConfig =
    let
      # Using unstable because version 0.6.0 introduced an
      # important fix to the gitattributes format
      inherit (flake-inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}) mergiraf;
      mergiraf-attributes =
        pkgs.runCommandLocal "gitattributes"
          {
            nativeBuildInputs = [
              mergiraf
            ];
          }
          ''
            mergiraf languages --gitattributes >> $out
          '';
    in
    {
      core.attributesfile = mergiraf-attributes.outPath;

      merge.mergiraf = {
        name = "mergiraf";
        driver = "${lib.getExe mergiraf} merge --git %O %A %B -s %S -x %X -y %Y -p %P -l %L";
      };
    };
}
