#!/bin/bash
[ -e /etc/profile.d/autojump.bash ] && . /etc/profile.d/autojump.bash

function try_load ()
{
    for fn in $*; do
        [ -e $fn ] && . $fn
    done

}

# Import some shared setting.
try_load ~/.zsh/rc/01_.zsh  ~/.zsh/rc/02_shared.zsh


if [  `uname -s` == "Darwin"  ]; then
    GENTOO="/opt"
    [ -e $GENTOO/etc/profile.d/autojump.bash ] && . $GENTOO/etc/profile.d/autojump.bash
    PATH="$GENTOO/bin":"$GENTOO/sbin":"$GENTOO/usr/bin":"$GENTOO/usr/sbin":${PATH}
    PATH="/Library/Frameworks/Python.framework/Versions/Current/bin:${PATH}"
fi

export PATH


