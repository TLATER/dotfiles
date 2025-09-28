use std/log

let packages_with_updatescript = (
  nix flake show --json
  | from json
  | $in.packages.x86_64-linux
  | columns
  | filter {|p| nix eval $'.#($p)' --apply 'builtins.hasAttr "updateScript"' | $in == 'true' }
)

for $package in $packages_with_updatescript {
  log info $'Updating ($package)'
  nix run $'.#($package).updateScript'
}

log info 'Committing changes'

try {
  git add pkgs/packages/
  git add home-config/dotfiles/eww/desktop-logic/Cargo.lock
  git commit -m 'update(pkgs): Update sources of all downstream packages'
} catch {
  log warning 'No changes to commit'
}
