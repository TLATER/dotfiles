#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# enable programmable completion features
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

# if [ $TERM = "linux" ]; then
#     echo -en "\e]P0073642" # Black
#     echo -en "\e]P1dc322f" # Red
#     echo -en "\e]P2859900" # Green
#     echo -en "\e]P3b58900" # Yellow
#     echo -en "\e]P4268bd2" # Blue
#     echo -en "\e]P5707880" # Magenta
#     echo -en "\e]P600ffff" # Cyan
#     echo -en "\e]P7eee8d5" # White
#     echo -en "\e]P8002b36" # Bright Black
#     echo -en "\e]P9cb4b16" # Bright Red
#     echo -en "\e]PA586e75" # Bright Green
#     echo -en "\e]PB70bcd8" # Bright Yellow
#     echo -en "\e]PC8cf3ff" # Bright Blue
#     echo -en "\e]PD6c71c4" # Bright Magenta
#     echo -en "\e]PE00ffff" # Bright Cyan
#     echo -en "\e]PFfdf6e3" # Bright White

#     setterm -background black
# fi

# Aliases
alias ls='ls --color=auto'
alias emacs='emacsclient -na ""'
alias pbcopy='xsel --clipboard --input'
alias pbpaste='xsel --clipboard'
alias winetricks='winetricks -q'
alias cleandocker='sudo docker rmi -f $(sudo docker images | grep "<none>" | awk "{print \$3}")'
alias yaourt='yaourt --noconfirm'

# Environment variables
export RTV_EDITOR='emacsclient'
export VISUAL='emacsclient'
export EDITOR='emacsclient'
export DIANA_SECRET_TOKEN='nYtCKPA4h37CZrQnTUqbnMKEdrftKmWj'
export ALTERNATE_EDITOR='emacs'
export DIANA_SECRET_TOKEN='nYtCKPA4h37CZrQnTUqbnMKEdrftKmWj'
export LD_LIBRARY_PATH="$HOME/.local/lib:$LD_LIBRARY_PATH"
export CPLUS_INCLUDE_PATH="$HOME/.local/include:CPLUS_INCLUDE_PATH"

# Prevent the annoying newline annoyances after a resize
shopt -s checkwinsize

# Set dir colors
if [ -f ~/.config/dir_colors ]; then
    eval `dircolors ~/.config/dir_colors`
fi

PS1='\[\033[1;36m\]\u \[\033[0;36m\]\w \$ \[\033[0m'

# Set dir colors
if [ -f ~/.config/dir_colors ]; then
    eval `dircolors ~/.config/dir_colors`
fi

# Start screen
if [ -z "$STY" ] && [ "$TERM" != "linux" ]; then
    exec screen -AxRR
fi
