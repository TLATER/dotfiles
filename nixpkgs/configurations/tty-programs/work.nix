{
  pkgs,
  lib,
  ...
}: {
  home.packages = with pkgs; [local.gauth];
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
}
