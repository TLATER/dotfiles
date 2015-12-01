#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

export MATLABPATH='/opt/info/courses/COMP24111/ex3'
export LIBRARY_PATH="$LIBRARY_PATH:$HOME/.local/lib"
export C_INCLUDE_PATH="$C_INCLUDE_PATH:$HOME/.local/include"

. ~/bin/blm
