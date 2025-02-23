#!/bin/sh

if [ -e "$XDG_RUNTIME_DIR/idle-inhibit" ]; then
    echo 'done' > "$XDG_RUNTIME_DIR/idle-inhibit"
    exit
fi

hold_lock="
mkfifo $XDG_RUNTIME_DIR/idle-inhibit; \
exec 3<> $XDG_RUNTIME_DIR/idle-inhibit; \
read -u 3; rm $XDG_RUNTIME_DIR/idle-inhibit; \
eww update -c $(dirname "$0") inhibit=false"

eww update -c "$(dirname "$0")" inhibit=true
systemd-inhibit \
    --what=idle \
    --who=eww-inhibit-widget \
    --why='User requested' \
    --mode=block \
    /bin/sh -c "$hold_lock"
