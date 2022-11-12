#####################################################################
# A number of configurations helpfully provided by (stolen from)    #
# oh-my-zsh. I used oh-my-zsh for a bit, but found it to be too     #
# slow. This file takes on some of their configuration because it's #
# nice, but leaves most of it behind because it's huge.             #
#####################################################################

# Some plugins need this variable
ZSH="$ZDOTDIR/oh-my-zsh-expat"

# init
# Load the chosen few plugins
for plugin in "$ZSH/plugins/"*; do
    fpath=($plugin $fpath)
done

for plugin in "$ZSH/plugins/"*; do
    source "$plugin/$(basename $plugin).plugin.zsh"
done

# completion
unsetopt menu_complete
unsetopt flowcontrol

setopt auto_menu
setopt complete_in_word
setopt always_to_end

zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*' matcher-list 'r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' list-colors ''

zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path "$XDG_CACHE_HOME/zsh"

expand-or-complete-with-dots() {
    # toggle line-wrapping off and back on again
    [[ -n "$terminfo[rmam]" && -n "$terminfo[smam]" ]] && echoti rmam
    print -Pn "%{%F{red}......%f%}"
    [[ -n "$terminfo[rmam]" && -n "$terminfo[smam]" ]] && echoti smam

    zle expand-or-complete
    zle redisplay
}
zle -N expand-or-complete-with-dots
bindkey "^I" expand-or-complete-with-dots

# correction
alias cp='nocorrect cp'
alias man='nocorrect man'
alias mkdir='nocorrect mkdir'
alias mv='nocorrect mv'
alias sudo='nocorrect sudo'

setopt correct_all

# directories
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus

# grep
# is x grep argument available?
grep-flag-available() {
    echo | grep $1 "" >/dev/null 2>&1
}

GREP_OPTIONS=""

# color grep results
if grep-flag-available --color=auto; then
    GREP_OPTIONS+=" --color=auto"
fi

# ignore VCS folders (if the necessary grep flags are available)
VCS_FOLDERS="{.bzr,CVS,.git,.hg,.svn}"

if grep-flag-available --exclude-dir=.cvs; then
    GREP_OPTIONS+=" --exclude-dir=$VCS_FOLDERS"
elif grep-flag-available --exclude=.cvs; then
    GREP_OPTIONS+=" --exclude=$VCS_FOLDERS"
fi

# export grep settings
alias grep="grep $GREP_OPTIONS"

# clean up
unset GREP_OPTIONS
unset VCS_FOLDERS
unfunction grep-flag-available

# history
HISTSIZE=50000
SAVEHIST=10000

## History command configuration
setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
setopt inc_append_history     # add commands to HISTFILE in order of execution
setopt share_history          # share command history data

# key-bindings
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

# termsupport
# Set terminal window and tab/icon title
#
# usage: title short_tab_title [long_window_title]
#
# See: http://www.faqs.org/docs/Linux-mini/Xterm-Title.html#ss3.1
# Fully supports screen, iterm, and probably most modern xterm and rxvt
# (In screen, only short_tab_title is used)
# Limited support for Apple Terminal (Terminal can't set window and tab separately)
function title {
    emulate -L zsh
    setopt prompt_subst

    [[ "$EMACS" == *term* ]] && return

    # if $2 is unset use $1 as default
    # if it is set and empty, leave it as is
    : ${2=$1}

    case "$TERM" in
        cygwin|xterm*|putty*|rxvt*|ansi)
            print -Pn "\e]2;$2:q\a" # set window name
            print -Pn "\e]1;$1:q\a" # set tab name
            ;;
        screen*)
            print -Pn "\ek$1:q\e\\" # set screen hardstatus
            ;;
        *)
            if [[ "$TERM_PROGRAM" == "iTerm.app" ]]; then
                print -Pn "\e]2;$2:q\a" # set window name
                print -Pn "\e]1;$1:q\a" # set tab name
            else
                # Try to use terminfo to set the title
                # If the feature is available set title
                if [[ -n "$terminfo[fsl]" ]] && [[ -n "$terminfo[tsl]" ]]; then
                    echoti tsl
                    print -Pn "$1"
                    echoti fsl
                fi
            fi
            ;;
    esac
}

ZSH_THEME_TERM_TAB_TITLE_IDLE="%15<..<%~%<<" #15 char left truncated PWD
ZSH_THEME_TERM_TITLE_IDLE="%n@%m: %~"
# Avoid duplication of directory in terminals with independent dir display
if [[ "$TERM_PROGRAM" == Apple_Terminal ]]; then
    ZSH_THEME_TERM_TITLE_IDLE="%n@%m"
fi

# Runs before showing the prompt
function omz_termsupport_precmd {
    emulate -L zsh

    if [[ "$DISABLE_AUTO_TITLE" == true ]]; then
        return
    fi

    title $ZSH_THEME_TERM_TAB_TITLE_IDLE $ZSH_THEME_TERM_TITLE_IDLE
}

# Runs before executing the command
function omz_termsupport_preexec {
    emulate -L zsh
    setopt extended_glob

    if [[ "$DISABLE_AUTO_TITLE" == true ]]; then
        return
    fi

    # cmd name only, or if this is sudo or ssh, the next cmd
    local CMD=${1[(wr)^(*=*|sudo|ssh|mosh|rake|-*)]:gs/%/%%}
    local LINE="${2:gs/%/%%}"

    title '$CMD' '%100>...>$LINE%<<'
}

precmd_functions+=(omz_termsupport_precmd)
preexec_functions+=(omz_termsupport_preexec)

# theme-and-appearance
# ls colors
autoload -U colors && colors

setopt auto_cd
setopt multios
setopt prompt_subst
