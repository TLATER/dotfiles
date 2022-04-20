#shellcheck shell=bash disable=SC2154
bindkey "${terminfo[kcbt]}" reverse-menu-complete
bindkey "^[[3~" delete-char
bindkey "^[3;5~" delete-char
