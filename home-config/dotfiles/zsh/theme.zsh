#shellcheck shell=bash enable=require-double-braces
autoload -U promptinit && promptinit

PROMPT="%B%F{cyan}%n%b %(5~|../|)%4~ %(!|#|$)%f "

if [[ "${IN_NIX_SHELL}" == "pure" ]]; then
    PROMPT="%F{green}[nix-shell]%f ${PROMPT}"
elif
    [[ "${IN_NIX_SHELL}" == "impure" ]]; then
    PROMPT="%F{yellow}[nix-shell]%f ${PROMPT}"
fi
