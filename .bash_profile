#!/bin/bash
[ -e /etc/profile.d/autojump.bash ] && . /etc/profile.d/autojump.bash

# Import some shared setting.
[ -e ~/.zsh/rc/01_shared.zsh ] && . ~/.zsh/rc/01_shared.zsh


if [  `uname -s` == "Darwin"  ]; then
    GENTOO="/opt"
    [ -e $GENTOO/etc/profile.d/autojump.bash ] && . $GENTOO/etc/profile.d/autojump.bash
    PATH="$GENTOO/bin":"$GENTOO/sbin":"$GENTOO/usr/bin":"$GENTOO/usr/sbin":${PATH}
    PATH="/Library/Frameworks/Python.framework/Versions/Current/bin:${PATH}"
fi

export PATH


