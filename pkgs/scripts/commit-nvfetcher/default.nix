{
  system,
  writeShellApplication,
  git,
  mktemp,
  flake-inputs,
}:
writeShellApplication {
  name = "commit-nvfetcher";

  runtimeInputs = [
    flake-inputs.nvfetcher.packages.${system}.default
    git
    mktemp
  ];

  text = builtins.readFile ./commit-nvfetcher;
}
