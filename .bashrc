#!/bin/bash
# export PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \W \$\[\033[00m\] '
alias gpush="git push"
alias gpull="git pull"
alias gco="git checkout"
alias gcm="git commit -a -m \"auto\""
alias gst="git status"

which dircolors >> /dev/null && alias ls="ls --color" || alias ls="ls -G"
