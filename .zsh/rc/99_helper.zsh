function gcms ()
{
    NOW=`date +"%H:%M:%S %m/%d/%Y"`
    git commit -a -m "Synced with Server at $NOW"
}

# find files and dump ..
# need to set OBJDUMP & CPPFILT.
function dump_obj ()
{
    if [ -z $OBJDUMP ]; then
        export OBJDUMP=objdump
    fi
    if [ -z $CPPFILT ]; then
        export CPPFILT=c++filt
    fi

    for fn in `find . -name "$1" -print`; do
        fn1="$fn.s"
        printf "Dumping %s to file:%s\n" $fn $fn1
        $OBJDUMP -D -S -l "$fn" | $CPPFILT > "$fn1"
        printf "Done\n\n"
    done

    printf "All work done \n\n"
}

function _icrash_internal ()
{
    _SEP="------------------------------------------------------------------------------"
    printf "\n%s\nShowing log: %s\n%s\n" $_SEP $2 $_SEP
    cmd=`cat <<EOF
(progn
(require 'iPhone_Crash_Log)
(iphone-crash "$1" "$2"))
EOF`

res=`emacsclient -e "$cmd"`
echo $res
}

# dump crash logs
function icrash ()
{
    if [ $# -lt 2 ]; then
        echo "Usage: icrash App CrashFile"
        return 1
    fi
    app=$1
    shift 1

    for lg in $*; do
        _icrash_internal $app $lg
    done
}


alias tlogcat="adb logcat -c >/dev/null 2>&1 && adb logcat | tee tlog.txt"
