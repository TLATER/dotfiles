{ inputs, ... }: {
  imports = [ inputs.treefmt.flakeModule ];

  perSystem.treefmt = {
    # `prek` is in charge of running these kinds of checks, we don't
    # want to run formatters with `nix flake check`.
    flakeCheck = false;

    settings = {
      allowMissingFormatter = false;
      walk = "git";

      formatter.nixfmt.options = [ "--strict" ];
    };

    programs = {
      nixfmt.enable = true;
      prettier.enable = true;
      shfmt.enable = true;
    };

    # settings.formatter.prettier.excludes = [ "*.md" ];
  };
}
