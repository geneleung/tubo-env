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
    netstat -nl 2>/dev/null | awk '{print $NF}' | grep -q "emacs"
}

function emacs_eidt ()
{
    #Every FILE can be either just a FILENAME or [+LINE[:COLUMN]] FILENAME.
    fn=`expr "$1" : '\([^:]*\):.*' '|' $1`
    ln=`expr "$1" :  '[^:]*:\(.*\)'` # line_info

    if [ -z $ln ]; then
        run-emacs $fn || emacsclient -n $fn
    else
        run-emacs "+$ln" $fn || emacsclient -n "+$ln" $fn
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
    valgrind  --leak-check=full --undef-value-errors=no \
             --log-fd=1 --log-file=$tmpfile "$@" &
    tail -f $tmpfile
}

function tperf-record()
{
    sudo perf record -o "perf_`date +'%m_%d_%H:%M:%S'`.data" $*
}


#mkdir and cd
function mcd ()
{
    mkdir $1 && cd $1
}
