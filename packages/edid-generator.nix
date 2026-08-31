{
  fetchFromGitHub,
  runCommand,
  writers,
}:
let
  edid-core =
    runCommand "edid-core"
      {
        src = fetchFromGitHub {
          owner = "goncalossilva";
          repo = "edid-generator";
          rev = "bc311cb7d5d4900a1bdad8fa967125c8e50c0174";
          hash = "sha256-90yzP0wxVms/6+sLa8zOkZbuBkqnBMQS0WyEn2poWUM=";
        };
      }
      /* sh */ ''
        mkdir -p "$out/lib/node_modules/edid-core/"
        cp "$src/assets/js/edid-core.js" "$out/lib/node_modules/edid-core/index.js"
      '';
in
writers.writeJSBin "edid-generator" { libraries = [ edid-core ]; } /* js */ ''
  const { readFile } = require("node:fs/promises");
  const { resolve } = require("node:path");
  const { stdout, stderr } = require("node:process");

  const { generateEdid } = require("edid-core");

  async function main() {
      const settings = JSON.parse(await readFile(resolve(process.argv[2])));
      const edid = generateEdid(settings);

      if (edid.warnings) {
          console.warn("EDID wasn't generated cleanly:");

          for (warning of edid.warnings) {
              console.warn("- " + warning);
          }
      }

      stdout.write(edid.bytes);
  }

  main();
''
