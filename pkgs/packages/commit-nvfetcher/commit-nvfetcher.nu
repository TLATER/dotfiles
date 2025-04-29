#!/usr/bin/env nu

let scratch_dir = mktemp --directory nvfetcher-XXXXX
let changelog = $scratch_dir + '/changelog'

cd ((git rev-parse --show-toplevel) + '/pkgs')

nvfetcher -l $changelog

if ($scratch_dir + '/changelog' | path exists) {
  git add _sources/
  git commit --message ("sources.nix: Update\n\n" + (open $changelog))
} else {
  # Clean up any debris nvfetcher may leave in its database.
  #
  # This should be "safe", since nvfetcher will override whatever
  # the user had previously anyway, so at least it won't break
  # anything further than running nvfetcher already did.
  git restore _sources/
}

rm -r $scratch_dir
