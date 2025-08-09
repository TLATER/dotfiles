{ sources, buildEnv }:
buildEnv {
  name = sources.catppuccin-i3.pname;
  paths = [ sources.catppuccin-i3.src ];
  pathsToLink = [ "/themes" ];
  extraPrefix = "/share/i3";
}
