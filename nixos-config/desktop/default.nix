{
  imports = [
    ./greeter
    ./sway.nix
  ];

  programs.uwsm = {
    enable = true;
    waylandCompositors.dummy = {
      prettyName = "dummy";
      comment = "Dummy service since the uwsm module won't work unless this is defined";
      binPath = "/";
    };
  };
}
