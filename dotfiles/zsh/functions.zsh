function eproject {
    local cmd="(with-current-buffer (window-buffer)
                 (projectile-project-root))"
    local dir="$($EMACS_PLUGIN_LAUNCHER --eval $cmd | tr -d \")"
    if [ -n "$dir" ]; then
        cd "$dir"
    else
        echo "can not deduce current buffer filename." >/dev/stderr
        return 1
    fi
}
