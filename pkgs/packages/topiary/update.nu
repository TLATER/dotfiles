let self = "pkgs/packages/topiary"

nix-update --flake --format --version=branch topiary.languages.topiary-nushell

# Get the current version of topiary-nushell
let src = nix eval --raw .#packages.x86_64-linux.topiary.languages.topiary-nushell.src

# Parse the commit of tree-sitter-nu topiary-nushell is compatible
# with from their `flake.nix`
let rev = (
  open --raw $'($src)/flake.nix'
  | ast-grep run --json --stdin --lang nix --pattern 'fetchGit { url = "$URL"; rev = "$REV"; }'
  | from json
  | $in.metaVariables.single.REV.text.0
)

let hash = (
  ^nix-prefetch-github --rev $rev nushell tree-sitter-nu
  | from json
  | $in.hash
)

(
  ast-grep run
  --pattern $'{rev = $REV;}'
  --selector binding
  --rewrite $'rev = "($rev)";'
  --update-all
  $"($self)/tree-sitter-nu.nix"
)

(
  ast-grep run
  --pattern $'{hash = $HASH;}'
  --selector binding
  --rewrite $'hash = "($hash)";'
  --update-all
  $"($self)/tree-sitter-nu.nix"
)
