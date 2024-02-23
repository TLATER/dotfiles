#!/usr/bin/env sh

NVCHECKER_CONFIG="$(mktemp)"
cat > "$NVCHECKER_CONFIG" <<EOF
[drivestrike]
source = "apt"
pkg = "drivestrike"
mirror = "https://app.drivestrike.com/static/apt/"
suite = "stretch"
EOF

nvchecker -c "$NVCHECKER_CONFIG" 2>&1 | cut -d ' ' -f 8
