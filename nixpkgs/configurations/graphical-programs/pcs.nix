{ pkgs, ... }:

{
  imports = [ ./barrier-client.nix ./barrier-server.nix ./stumpwm.nix ];
  home.packages = with pkgs; [ local.pass-rofi ];
  services.caffeine.enable = true;
}
