let dir = 'pkgs/packages/edopro/' | path expand

def parse_edopro_version []: nothing -> string {
  print $dir

  (ast-grep run
    --pattern '{ version = "$VERSION"; }'
    --selector binding
    ($dir | path join edopro-unwrapped.nix)
    | parse '{key} = "{version}";' | $in.version.0)
}

let old_version = parse_edopro_version

# EDOPro and irrlicht are very regular packages
nix-update --flake --format edopro.scope.edopro-unwrapped
nix-update --flake --format --version=skip edopro.scope.irrlicht-edopro

let edopro_version = parse_edopro_version

if ($old_version == $edopro_version) {
  exit
}

# Prefetch the version of the EDOPro assets
let edopro_assets_hash = (nix-prefetch-url --unpack
  https://github.com/ProjectIgnis/edopro-assets/releases/download/($edopro_version)/ProjectIgnis-EDOPro-($edopro_version)-linux.tar.gz
  | nix hash to-sri --type sha256 $in)

(ast-grep run
  --pattern '{ hash = "$HASH"; }'
  --selector binding
  --rewrite $'hash = "($edopro_assets_hash)";'
  --update-all
  ($dir | path join package.nix))
