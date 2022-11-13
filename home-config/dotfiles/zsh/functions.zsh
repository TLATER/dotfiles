#shellcheck shell=bash
function eproject {
    local cmd="(with-current-buffer (window-buffer)
                 (let ((project (project-current)))
                   (when project
                     (expand-file-name (project-root project)))))"
    local dir
    dir="$(emacsclient --eval "$cmd" | tr -d \")"
    if [ -n "$dir" ] && [ "$dir" != "nil" ]; then
        cd "$dir" || return 1
    else
        echo "can not deduce current buffer filename." >/dev/stderr
        return 1
    fi
}
