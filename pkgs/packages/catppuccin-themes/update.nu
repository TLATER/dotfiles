let self = "pkgs/packages/catppuccin-themes/package.nix"

def update [prefetch] {
  mut new_attrset = $"{\n  name = \"catppuccin-($prefetch.repo)\";\n  "
  $new_attrset += ($prefetch |
    transpose key value |
    each { $"($in.key) = \"($in.value)\";" } |
    str join "\n  "
  )
  $new_attrset += "\n}"

  (ast-grep run
    --lang nix
    --pattern $'{ name = "catppuccin-($prefetch.repo)"; $$$ }'
    --rewrite $new_attrset
    --update-all
    $self)
}

for $name in [ "alacritty" "fuzzel" ] {
  update (nix-prefetch-github catppuccin $name | from json)
}

update (nix-prefetch-github-latest-release catppuccin i3 | from json)
