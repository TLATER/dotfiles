{
  flake-inputs,
  pkgs,
  lib,
  config,
  ...
}:
let
  inherit (flake-inputs.self.packages.${pkgs.stdenv.hostPlatform.system}) firefox-ui-fix;
  cfg = config.programs.librewolf;
in
{
  options.programs.librewolf.enableLepton = lib.mkEnableOption "enable the lepton theme";

  config = lib.mkIf cfg.enableLepton {
    home.file = lib.mkMerge (
      lib.mapAttrsToList (_: profile: {
        "${cfg.profilesPath}/${profile.path}/chrome/userChrome.css".source =
          "${firefox-ui-fix}/css/leptonChrome.css";
        "${cfg.profilesPath}/${profile.path}/chrome/userContent.css".source =
          "${firefox-ui-fix}/css/leptonContent.css";
        "${cfg.profilesPath}/${profile.path}/user.js".source = "${firefox-ui-fix}/user.js";
        "${cfg.profilesPath}/${profile.path}/icons".source = "${firefox-ui-fix}/icons";
        "${cfg.profilesPath}/${profile.path}/css".source = "${firefox-ui-fix}/css";
      }) cfg.profiles
    );
  };
}
