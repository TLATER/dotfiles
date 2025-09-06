{
  flake-inputs,
  pkgs,
  lib,
  ...
}:
{
  imports = [ ./emacs ];

  home.packages = [ flake-inputs.self.packages.${pkgs.system}.topiary ];

  programs.bottom = {
    enable = true;
    settings.flags.group_processes = true;
  };

  programs.git.extraConfig =
    let
      mergiraf-attributes =
        pkgs.runCommandLocal "gitattributes" { nativeBuildInputs = [ pkgs.mergiraf ]; }
          ''
            mergiraf languages --gitattributes >> $out
          '';
    in
    {
      core.attributesfile = mergiraf-attributes.outPath;

      merge.mergiraf = {
        name = "mergiraf";
        driver = "${lib.getExe pkgs.mergiraf} merge --git %O %A %B -s %S -x %X -y %Y -p %P -l %L";
      };
    };
}
