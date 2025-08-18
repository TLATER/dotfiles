{ pkgs }:
let
  inherit (pkgs) lib;
  inherit (pkgs.writers) makeScriptWriter;

  writeNuBin =
    name: plugins: args:
    makeScriptWriter (
      args
      // {
        interpreter = lib.concatStringsSep " " (
          [
            (lib.getExe pkgs.nushell)
            "--no-config-file"
          ]
          ++
            lib.optional (plugins != [ ])
              "--plugins [${lib.concatStringsSep " " (map (p: lib.getExe p) plugins)}]"
        );
      }
    ) "/bin/${name}";
in
{
  nixUpdateScript =
    { packageToUpdate }:
    writeNuBin "update-${packageToUpdate}" [ ]
      {
        makeWrapperArgs = [
          "--prefix"
          "PATH"
          ":"
          "${lib.makeBinPath [
            pkgs.nix-update
            pkgs.nixfmt-rfc-style
          ]}"
        ];
      }
      ''
        (nix-update
          --flake
          --format
          ${packageToUpdate})
      '';

  writeUpdateScript =
    {
      script,
      packageToUpdate,
      utils ? [ ],
      nushellPlugins ? [ ],
    }:
    # TODO(tlater): Upstream's `writeNuBin` doesn't support nushell
    # plugins currently
    writeNuBin "update-${packageToUpdate}" nushellPlugins {
      makeWrapperArgs = lib.optionals (utils != [ ]) [
        "--prefix"
        "PATH"
        ":"
        "${lib.makeBinPath utils}"
      ];

    } script;
}
