#shellcheck shell=bash disable=SC1091 disable=SC1090

# Load environment configuration first
[ -f "$HOME/.config/dir_colors" ] && eval "$(dircolors "$HOME/.config/dir_colors")"
[ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ] && source "$HOME/.nix-profile/etc/profile.d/nix.sh"
[ -f "$HOME/.nix-profile/etc/profile.d/nix-daemon.sh" ] && source "$HOME/.nix-profile/etc/profile.d/nix-daemon.sh"
export "NIX_PATH=${NIX_PATH:-$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH}"

# Don't create .zcompdump files in $HOME
autoload -Uz compaudit compinit && compinit -d "${XDG_CACHE_HOME}/zsh/zcompdump-${ZSH_VERSION}"

# Everything else is just for interactive shells
if [[ -o interactive ]]; then
    # Load other extensions
    autoload -U zcalc

    # Allow nix-shell to function
    if (( $+commands[any-nix-shell] )); then
        any-nix-shell zsh | source /dev/stdin
    fi

    # Activate direnv
    eval "$(direnv hook zsh)"

    # Load more granular configuration files
    source "$ZDOTDIR/functions.zsh"
    source "$ZDOTDIR/aliases.zsh"
    source "$ZDOTDIR/completion.zsh"
    source "$ZDOTDIR/keybindings.zsh"
    source "$ZDOTDIR/theme.zsh"
    source "$ZDOTDIR/oh-my-zsh-expat/oh-my-zsh-expat.zsh"

    # Load plugins
    for plugin in "$ZDOTDIR/plugins/"*; do
        for file in "$plugin/"*.zsh; do
            source "$file"
        done
    done

    # Start screen if...
    if \
        # The command exists
        (( $+commands[screen] )) && \
            # The configuration file exists
            [ -f "$XDG_CONFIG_HOME/screen/config" ] && \
            # Screen is not already running
            [ -z "$STY" ] && \
            # And our terminal is acceptable
            [ "$TERM" != "dumb" ] && \
            [ "$TERM" != "linux" ]; then
        exec screen -AxRR -c "$XDG_CONFIG_HOME/screen/config"
    fi
fi
