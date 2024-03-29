export HISTSIZE=512
export SAVEHIST=1000
export HISTFILE=~/.zhistory
export WORDCHARS='*?_-.[]~&;!#$%^(){}<>'
export ARCH=`uname -m`"-"`uname -s | tr '[:upper:]' '[:lower:]'`
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd'
export PATH="/opt/bin:/opt/usr/bin:/opt/usr/sbin:$PATH:/sbin:/usr/sbin/"

export EIX_LIMIT=0

if [ -d /opt/usr ]; then
    export PATH="/opt/usr/bin:/opt/usr/sbin:$PATH"
fi

## Adding Android Sdk Path (ASP).
if [ -d /opt/Android ]; then
    export ASP="/opt/Android/sdk"
    if [ ! -d $ASP ]; then
        expr `uname` : Darwin > /dev/null 2>&1
        if [ $? -eq 0 ]; then
            export ASP="/opt/Android/android-sdk-macosx"
        fi
    fi
elif [ -d /opt/adt/sdk ]; then
    export ASP="/opt/adt/sdk"
elif [ -d /opt/android-sdk-update-manager ]; then
    export ASP="/opt/android-sdk-update-manager"
fi

if [ ! -z $ASP ]; then
    export PATH="$PATH:$ASP/tools:$ASP/platform-tools"
fi

if [ -d /opt/android-ndk-r9 ]; then
    export PATH="$PATH:/opt/android-ndk-r9"
fi

## Adding texlive path.
if [ -d /opt/texlive/bin/$ARCH ]; then
    export PATH="$PATH:/opt/texlive/bin/$ARCH"
fi


if [ -d $HOME/tools ]; then
    export PATH="$HOME/tools/bin:$PATH"
    export LD_LIBRARY_PATH="$HOME/tools/lib:$HOME/tools/lib64:$LD_LIBRARY_PATH"
fi

# Utilities.
alias ll="ls -lah"
alias rm="rm -i"
alias cp="cp -i"
alias l="ls -ail"
alias cl="clear"

which dircolors >> /dev/null && alias ls="ls --color" || alias ls="ls -G"
alias gst="git status"
alias gpush="git push"
alias gpull="git pull"
alias gco="git checkout"
alias gcm="git commit -a -m \"auto\""
alias gcs="git commit -a -m \"updated submodules\""
alias gcus="git commit -a -m \"Updated submodules \""

alias kxcode="killall -9 Xcode"
alias reboot="shutdown -r now"

# do a du -hs on each dir on current path
alias lsdir="for dir in *;do;if [ -d \$dir ];then;du -hsL \$dir;fi;done"
ulimit -c unlimited

alias rcp="rsync -P "
which dcfldd > /dev/null 2>&1 && alias dd="dcfldd"

alias ee=emacs_edit
alias eet=emacs_edit_terminal
alias edit=emacs_eidt
alias tmux="tmux attach || tmux"
alias ttop="top -u $UID"
which xdg-open > /dev/null 2>&1 && alias open=xdg-open

alias time="/usr/bin/time -p"
