{
  config,
  lib,
  ...
}: {
  config = lib.mkIf (config.custom.desktop-environment && config.custom.has-yubikey) {
    programs.password-store = {
      enable = true;
      settings = {
        PASSWORD_STORE_DIR = "${config.xdg.dataHome}/password-store";
        PASSWORD_STORE_KEY = "0x9FAF1AA48509A7F1";
        PASSWORD_STORE_GENERATED_LENGTH = "16";
      };
    };
  };
}
