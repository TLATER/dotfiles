let dir = 'pkgs/packages/emacs'

for file in [$'($dir)/eglot-x.nix' $'($dir)/kdl-mode.nix'] {
  let owner = (ast-grep run
    --pattern '{ owner = $OWNER; }'
    --selector binding
    $file) | parse '{key} = "{owner}";' | $in.owner.0

  let repo = (ast-grep run
    --pattern '{ repo = $REPO; }'
    --selector binding
    $file) | parse '{key} = "{repo}";' | $in.repo.0

  let prefetch = nix-prefetch-github $owner $repo --meta | from json

  # The melpa builder demands a melpa-style version string; these are
  # the commit date + . + the hour/minute, with only the minute being
  # 0-padded.
  let date = $prefetch.meta.commitDate + ' ' + $prefetch.meta.commitTimeOfDay | date from-human
  mut version = $date | format date '%Y%m%d.'
  $version += $date | format date '%k%M' | str trim --left

  (ast-grep run
    --pattern '{ version = $VERSION; }'
    --selector binding
    --rewrite $'version = "($version)";'
    --update-all
    $file)

  (ast-grep run
    --pattern '{ rev = $REV; }'
    --selector binding
    --rewrite $'rev = "($prefetch.src.rev)";'
    --update-all
    $file)

  (ast-grep run
    --pattern '{ hash = $HASH; }'
    --selector binding
    --rewrite $'hash = "($prefetch.src.hash)";'
    --update-all
    $file)
}
