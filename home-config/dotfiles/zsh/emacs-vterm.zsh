# A bunch of settings for emacs' vterm integration.
#
# See their README for more (some of these snippets copied directly
# from there).
#
#shellcheck shell=bash

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    # Load the vterm-specific zsh config
    if [[ -n ${EMACS_VTERM_PATH} ]] && \
       [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
        #shellcheck disable=SC1091
        source "${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh"
    fi

    # Custom message-passing functions

    # Make `emacs` call find-file instead
    unalias emacs
    function emacs() {
        vterm_cmd find-file "$(realpath "${@:-.}")"
    }

    function magit() {
        vterm_cmd magit
    }

    function man() {
        vterm_cmd woman
    }
fi
