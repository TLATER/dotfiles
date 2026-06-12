/**
  Configuration for sound.
*/
{ pkgs, ... }: {
  environment.systemPackages = [ pkgs.pwvucontrol ];

  services.pipewire = {
    enable = true;

    alsa.enable = true;
    pulse.enable = true;
  };
}
