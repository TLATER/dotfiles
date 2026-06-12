let self = "pkgs/packages/nvidia/package.nix"

mut updates = {}

$updates.version = (
  http get https://www.nvidia.com/en-us/drivers/unix/ |
  # There is no better selector, sadly; the Linux x86
  # production driver *should* be the first link in the first
  # paragraph, though
  query web --query '#rightContent p:first-child a:first-of-type' | get 0 | get 0 | str trim
)

$updates.sha256_64bit = (
  ^nix-prefetch-url https://us.download.nvidia.com/XFree86/Linux-x86_64/($updates.version)/NVIDIA-Linux-x86_64-($updates.version).run
  | nix hash to-sri --type sha256 $in
)
$updates.openSha256 = (
  ^nix-prefetch-github --rev $updates.version NVIDIA open-gpu-kernel-modules
  | from json
  | $in.hash
)

for $update in ($updates | transpose key value) {
  (
    ast-grep run
    --pattern $'{($update.key) = $VALUE;}'
    --selector binding
    --rewrite $'($update.key) = "($update.value)";'
    --update-all
    $self
  )
}
