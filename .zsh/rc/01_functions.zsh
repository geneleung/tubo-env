## Following settings should be shared by both bash and zsh.
#environement variables

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

_is_emacs_daemon_started () {
    if [ $# -ne 0 ]; then
        netstat -nl 2>/dev/null | awk '{print $NF}' | grep "emacs" | grep -q "$*"
    else
        netstat -nl 2>/dev/null | awk '{print $NF}' | grep -q "emacs"
    fi
}

function emacs_edit ()
{
    #Every FILE can be either just a FILENAME or [+LINE[:COLUMN]] FILENAME.
    fn=`expr "$1" : '\([^:]*\):.*' '|' $1`
    ln=`expr "$1" :  '[^:]*:\(.*\)'` # line_info

    if [ -z $fn ]; then
        fn="."
    fi

    if [ -z $ln ]; then
        run-emacs $fn || emacsclient -n $fn
    else
        run-emacs "+$ln" $fn || emacsclient -n "+$ln" $fn
    fi
}

function emacs_edit_terminal ()
{
    #Every FILE can be either just a FILENAME or [+LINE[:COLUMN]] FILENAME.
    fn=`expr "$1" : '\([^:]*\):.*' '|' $1`
    ln=`expr "$1" :  '[^:]*:\(.*\)'` # line_info

    if [ -z $fn ]; then
        fn="."
    fi

    if [ -z $ln ]; then
        emacsclient -nw $fn
    else
        emacsclient -nw "+$ln" $fn
    fi
}


# function to call valgrind and show output...
function tval ()
{
    if [ $# -lt 1 ]; then
        cat <<EOF
Usage: tval EXECUTABLE [args]
EOF
        return 0
    fi

    app=`basename $1`
    tmpfile=$(mktemp --suffix=".log" valgrind_"$app"_XXXXXXXX)
    echo "Will write to file: $tmpfile"

    valgrind  --leak-check=full --show-leak-kinds=all --undef-value-errors=no \
             --log-fd=1 --log-file=$tmpfile "$@" &

    tail -f $tmpfile
}

function tperf-record()
{
    if [ -e perf.data ]; then
        sudo mv perf.data "perf_`date +'%m_%d_%H:%M:%S'`.data"
    fi

    sudo perf record \
         -e cycles,instructions,branch-misses,cache-misses \
         $*
}


#mkdir and cd
function mcd ()
{
    mkdir $1 && cd $1
}


function svnedit ()
{
    if [ $# -lt 2 ]; then
        echo "Usage: svnedit revision URL"
        return
    fi

    svn propedit -r $1 --revprop svn:log $2
}

function o_dump_addr ()
{
    if [ $# -lt 2 ]; then
        printf "Usage: o_dump_addr addr[addr...] binary\n"
        return
    fi

    exe="${@: -1}"
    if [ ! -f $exe ]; then
        printf "file $exe does not exist\n"
        return
    fi

    for addr in ${@:1:$(($# - 1))}; do
        if [ $(expr $addr : "0x[0-9a-fA-F]\+$") -eq 0 ]; then
            printf "Warning: %s is not valid address, skipped...\n" $addr
        else
            new_addr=$(($addr - 100)) # show last 100 lines as context
            if [ $new_addr -gt 0 ]; then
                addr=$new_addr
            fi
            objdump -d -S --start-address=$addr $exe | awk '{print $0} $3~/retq?/{exit}'
        fi
    done
}

function g_dump_symbol ()
{
    if [ $# -lt 2 ]; then
        printf "Usage: g_dump_symbol symbol[symbol...] binary\n"
        return
    fi

    gdb --version > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        printf "g_dump_symbol requires gdb which is not available...\n"
        return
    fi

    exe="${@: -1}"
    if [ ! -f $exe ]; then
        printf "file $exe does not exist\n"
        return
    fi

    for symbol in ${@:1:$(($# - 1))}; do
        gdb -batch -ex "file $exe" -ex "disassemble $symbol"
    done
}
