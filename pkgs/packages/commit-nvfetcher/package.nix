{
  writeShellApplication,
  git,
  mktemp,
  nvfetcher,
}:
writeShellApplication {
  name = "commit-nvfetcher";

  runtimeInputs = [
    nvfetcher
    git
    mktemp
  ];

  text = builtins.readFile ./commit-nvfetcher.sh;
}
