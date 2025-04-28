#!/usr/bin/env bash

set -euo pipefail

# shellcheck disable=2154
mkdir -p "$out"

fd -e sh -e zsh -X shellcheck | tee "$out/shellcheck.log"

# Statix doesn't support checking multiple files
statix check \
    --ignore "hardware-configuration.nix" "home-config/dotfiles/emacs.d/share/templates/*" |
    tee "$out/statix.log"

for_nix_files() {
    fd -e nix \
        -E 'hardware-configuration.nix' \
        -E 'home-config/dotfiles/emacs.d/share/templates/*' \
        -E 'pkgs/_sources/*' \
        --type file \
        --exec-batch "$@"
}

for_nix_files nixfmt --check --strict | tee "$out/nixfmt.log"
for_nix_files deadnix --fail | tee "$out/nixfmt.log"
