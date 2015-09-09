## Following settings should be shared by both bash and zsh.
#environement variables

export HISTSIZE=512
export SAVEHIST=1000
export HISTFILE=~/.zhistory
export WORDCHARS='*?_-.[]~&;!#$%^(){}<>'
export ARCH=`uname -m`"-"`uname -s | tr '[:upper:]' '[:lower:]'`
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd'
export PATH="/opt/bin:$PATH:/sbin:/usr/sbin/"

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

# Utilities.
alias ll="ls -lah"
alias rm="rm -i"
alias cp="cp -i"
alias l="ls -ail"
alias cl="clear"

which dircolors >> /dev/null && alias ls="ls --color" || alias ls="ls -G"
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

function gtop ()
{
    dir=`git rev-parse --show-toplevel`
    cd $dir
}


function git_submodule_update ()
{
    tmpfile=$(mktemp)
    git submodule update --init | tee $tmpfile
    TOP=$PWD
    for dir in `cat $tmpfile | grep "\.emacs.d/site-lisp"`; do
        dirn=`echo $dir | awk -F "'" '{print $2}'`
        if [ -d $HOME/$dirn ]; then
            printf "\ncd to $HOME/$dirn\n"
            cd $HOME/$dirn
            if [ -e Makefile ]; then
                make
            else
                printf "Not support for now.\n"
            fi
        fi
    done
}

function emacs_eidt ()
{
    #Every FILE can be either just a FILENAME or [+LINE[:COLUMN]] FILENAME.
    fn=`expr "$1" : '\([^:]*\):.*' '|' $1`
    ln=`expr "$1" :  '[^:]*:\(.*\)'` # line_info

    if [ -z $ln ]; then
        emacsclient -n $fn
    else
        emacsclient -n "+$ln" $fn
    fi
}

alias ee=emacs_eidt
alias edit=emacs_eidt
alias tmux="tmux attach || tmux"
alias ttop="top -u $UID"
which xdg-open > /dev/null 2>&1 && alias open=xdg-open


# find and open with vim.
function fvim ()
{
    find . -name $1 -exec vim {} \;
}

function fee ()
{
    find . -name $1 -exec emacsclient -n {} \;
}

# Functions used by arc of phabricator.
function arcd ()
{
    if [ $# -ne 0 ]; then
        cl=$1
    else
        cl="HEAD"
    fi

    arc diff --head $cl "$cl~"
}

