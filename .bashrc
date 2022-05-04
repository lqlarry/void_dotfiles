# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source ~/.aliases

PATH="$HOME/.cabal/bin:$HOME/.local/bin:$PATH"

PS1='[\u@\h \W]\$ '
