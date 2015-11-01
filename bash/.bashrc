#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

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

# Autocompletion
complete -W "mbax4tm2@kilburn.cs.manchester.ac.uk www.tlater.net" ssh

# Aliases
alias ls='ls --color=auto'
#alias skype='apulse32 skype'
alias emacs='emacsclient'
#alias pbcopy='xclip -selection clipboard'
#alias pbpaste='xclip -selection clipboard -o'
#alias open='/usr/bin/vendor_perl/mimeopen'
#alias ncmpcpp='ncmpcpp -c /home/tlater/.config/ncmpcpp'
alias tmux='tmux -f ~/.config/tmux/tmux.conf'
alias pbcopy='xsel --clipboard --input'
alias pbpaste='xsel --clipboard'

# Environment variables
#export ANDROID_HOME='/opt/android-sdk'
export RTV_EDITOR='emacsclient'
export EDITOR='emacsclient'

# Prevent the annoying newline annoyances after a resize
shopt -s checkwinsize

PS1='\[\033[1;36m\]\u \[\033[0;36m\]\w \$ \[\033[0m'
PATH=$PATH:/home/tlater/bin
