{
  config,
  lib,
  ...
}: {
  programs.git = {
    enable = true;
    userName = "Tristan Daniël Maat";
    userEmail = "tm@tlater.net";

    signing = {
      key = "0x49670FD774E43268";
      signByDefault = config.custom.has-yubikey;
    };

    ignores = [".envrc" ".direnv/"];
    extraConfig = {
      branch.autoSetupRebase = "always";
      checkout.defaultRemote = "origin";

      pull.rebase = true;
      pull.ff = "only";
      push.default = "current";

      init.defaultBranch = "main";
      submodule.recurse = "true";

      # Magit-forge configuration
      github.user = "tlater";
      gitlab.user = "tlater";
      # gitlab.gitlab is intentional; tells magit-forge to use the
      # gitlab API and *then* specifies the domain
      "gitlab.gitlab.codethink.co.uk/api/v4".user = "tristanmaat";
      url."ssh://git@".pushInsteadOf = lib.mkIf config.custom.has-yubikey "https://";
    };
  };
}
