{ sources, buildEnv }:
buildEnv {
  name = "catppuccin-fuzzel";
  paths = [ sources.catppuccin-fuzzel.src ];
  pathsToLink = [ "/themes" ];
  extraPrefix = "/share/fuzzel";
}
