{
  flake-inputs,
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (flake-inputs.self.packages.${pkgs.system}) gauth;
in {
  config = lib.mkIf config.custom.is-work {
    home.packages = [gauth];
    programs.git.userEmail = lib.mkOverride 99 "tristan.maat@codethink.co.uk";

    programs.ssh = {
      matchBlocks = {
        "shell.codethink.co.uk" = lib.hm.dag.entryAfter ["*"] {
          user = "tristanmaat";
          extraOptions = {"VerifyHostKeyDNS" = "yes";};
        };
        "gitlab.codethink.co.uk" = lib.hm.dag.entryAfter ["*"] {
          user = "git";
          extraOptions = {"VerifyHostKeyDNS" = "yes";};
        };
        "git.codethink.co.uk" = lib.hm.dag.entryAfter ["*"] {
          user = "git";
          extraOptions = {"VerifyHostKeyDNS" = "yes";};
        };
      };
    };
  };
}
