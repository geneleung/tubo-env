#!/bin/bash
#
# Author: Yang, Ying-chao@gmail.com, 2015-01-12
#

for fn in /usr/share/emacs/24.4/lisp/cedet/ede/*.el; do
    echo $fn
    bfn=`basename $fn`
    if [ -e $bfn ]; then
        rm -rf $bfn
    fi
done
