#shellcheck shell=bash
function eproject {
    local cmd="(with-current-buffer (window-buffer)
                 (projectile-project-root))"
    local dir
    dir="$($EMACS_PLUGIN_LAUNCHER --eval "$cmd" | tr -d \")"
    if [ -n "$dir" ]; then
        cd "$dir" || exit 1
    else
        echo "can not deduce current buffer filename." >/dev/stderr
        return 1
    fi
}

function ssh-unsafe {
    ssh -o GlobalKnownHostsFile=/dev/null -o UserKnownHostsFile=/dev/null "$@"
}
