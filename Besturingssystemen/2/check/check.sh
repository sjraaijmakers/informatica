#!/usr/bin/env bash

declare -r F_ALLOC_FREE="cnt mm_alloc/mm_free"
declare -r F_FAILED="cnt failed allocations despite free space"
declare -r F_ACT_DEACT="cnt hw_activate/hw_deactivate"
declare -r F_MAX_BANKS="max number of banks active"
declare -r F_END_BANKS="cnt banks left activated at end"
declare -r F_AVG_OVERHEAD="avg external overhead (bytes)"
declare -r F_MAX_OVERHEAD="max external overhead (bytes)"
declare -r F_TIME="total time (virtual ms)"
declare -r F_ENERGY="total energy usage (virtual mJ)"
declare -r F_POWER="avg power usage (virtual W)"
declare -r F_PERF_TIME="performance (op / s)"
declare -r F_PERF_WATT="performance/watt (op / J)"

declare -r COLORS_SUPPORTED=$(bc <<< "`(tput colors) 2>/dev/null || echo 0` >= 8")

src="mm.c `grep ^ADDITIONAL_SOURCES Makefile | cut -d = -f 2`"
hdr=`grep ^ADDITIONAL_HEADERS Makefile | cut -d = -f 2`

# cd to one dir above check script
#XXX: cd "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/.."

bail() {
    printf "$1\n\n"
    echo "preliminary grade: $grade"
    rm -f _out
    exit 1
}

summary_field() {
    if [ $ret -eq 0 ]
    then grep "$*" _out | sed 's/[^:]\+: //'
    else echo undefined
    fi
}

grade=0
grant() {
    echo -n "grade "
    [ `bc <<< "$1 > 0"` -eq 1 ] && printf +
    echo $1
    grade=`bc <<< "$grade + $1" | sed 's/^\./0./'`
}

header() { printf "%-70s" "$*"; }
color() {
    if [ $COLORS_SUPPORTED -eq 1 ]
    then echo -e "\033[1;${1}m${*:2}\033[0m"
    else echo ${*:2}
    fi
}
ok()   { echo [ `color 32 ok` ]; }
fail() { echo [ `color 31 failed` ]; }

check_fail() {
    local ret=$?
    if [ $ret -ne 0 ]
    then
        fail
        [ "$cmd" ] && echo "command: $cmd"
        [ "$OUT" ] && printf "$OUT\n"
    fi
    return $ret
}
check() {
    check_fail
    ret=$?
    [ $ret -eq 0 ] && ok
    return $ret
}
invert() { [ $? -ne 0 ]; }

save_trace() {
    cmd="./test $*"
    bash -c "$cmd -l _out >/dev/null 2>/dev/null" 2>/dev/null
    ret=$?
    if [ $ret -ne 0 ]
    then
        fail
        echo "command: $cmd"
    fi
    return $ret
}

no_failed_allocs() {
    [ "`summary_field $F_FAILED`" = "0 (0.0%)" ]
    check_fail
}

# +0.5pt for submitted AUTHORS file and non-empty report.pdf
header "checking AUTHORS and report.pdf"

[ -e AUTHORS ]
check_fail || bail "AUTHORS does not exist"
AUTHORS=`python check/parse_authors.py < AUTHORS`
check_fail || bail "AUTHORS format invalid: \n$AUTHORS"

ret=1
if [ -e report.pdf ]; check_fail
then
    file report.pdf | grep -q 'PDF document'
    check || echo "report.pdf is not a valid PDF document"
else
    echo "report.pdf does not exist"
fi

echo -e "$AUTHORS"
[ $ret -eq 0 ] && grant 0.5
echo

# +0.5pt if code compiles without errors and mm.c is modified in any way
header "checking mm.c for modifications"
cmd="diff mm.c mm-emtpy.c"
diff -q mm.c check/mm-empty.c >/dev/null
invert; check || bail "mm.c has not been modified"

header "compiling"
cmd="make"
#XXX: if make_err=`make clean all 2>&1 >/dev/null`; check
if make_err=`make 2>&1 >/dev/null`; check
then grant 0.5
else bail "$make_err\ncompilation failed, quitting"
fi
echo

# check if malloc/free/globvars are used
header "checking if malloc / free / global variables are used"
cmd=
for cfile in $src
do
    ofile=`sed 's/\.c$/.o/' <<< $cfile`
    out=`nm $ofile | perl -ne '
    /U (malloc|free|mmap|strdup|s?brk)\b/ &&
        die "use of $1() is forbidden\n";
    /[a-f0-9]+ [DdCBbGg] (.+)/ &&
        die "use of global variables is forbidden (found global \"$1\")\n";
    ' 2>&1`
    OUT="$cfile:\n$out" check_fail || bail "pre-check failed, quitting"
done
ok
echo

# +1pt if your code supports a simple sequence of calls to mm_alloc without
# errors using only 1 memory module and returning a valid range every time.
# checks if all allocs succeed in check/simple-alloc.t
header "checking simple sequence of calls to mm_alloc"
if save_trace -k check/simple-alloc.t
then no_failed_allocs && { ok; grant 1; }
fi
echo

# +1pt if your overhead is asymptotically constant in long sequences of calls
# to mm_alloc followed by mm_free using only 1 memory module.
header "checking if overhead is asymptotically constant"
if save_trace -k check/long-fifo.t -v 1
then
    ovhpb=$(grep -o '\] [^ ]\+ .\+' _out | cut -d ' ' -f 4 |
            grep '^[[:digit:]]')
    fail # TODO
    echo "THIS CHECK HAS NOT YET BEEN IMPLEMENTED"
fi
echo

# +1pt if your code can optimize for performance: during sequences of mm_alloc
# then mm_free, hw_activate is called less often than there are calls to
# mm_alloc (bank reuse).
header "checking if code reuses banks"
if save_trace -k simple.t
then
    nalloc=$(cut -d / -f 1 <<< "`summary_field $F_ALLOC_FREE`")
    nact=$(cut -d / -f 1 <<< "`summary_field $F_ACT_DEACT`")
    [ $nact -lt $nalloc ]
    check && grant 1
fi
echo

# +1pt if your code has a significantly lower number of "allocations failures
# despite empty space" than the dumb allocator (i.e. your code actively manages
# free space).
# checks if nr of failed allocs despite free space is <10% for smalltrace.t
header "checking allocation failures despite empty space"
if save_trace -k smalltrace.t
then
    failed=$(expr "`summary_field $F_FAILED`" : ".*(\(.*\)%)")
    LIMIT=10
    if [ `bc <<< "$failed < $LIMIT"` -eq 1 ]; check
    then grant 1
    else echo "\"$cmd\" yielded more than $LIMIT% failed allocations"
    fi
fi
echo

# +2pt if your report satifies the requirements
header "assuming 2 points for report (needs manual review)"
ok
grant 2
echo

# -1pt if $(CC) -W -Wall reports warnings
header "checking if compilation with CFLAGS=-W -Wall reports warnings"
if [ "$make_err" ]
then fail; echo "$make_err"; grant -1
else ok
fi
echo

# -1pt if valgrind complains about your code
# FIXME: should we test with more arguments?
header "checking if valgrind reports errors"
out=`valgrind ./test 2>&1`
OUT="$out" check || grant -1
echo

# only continue to advanced checks if at least 5 points have been obtained
if [ `bc <<< "$grade < 5"` -eq 1 ]
then bail "did not get at least 5 points, quitting"
fi

# +0,5pt if your manager can satisfy multiple calls to mm_alloc by sharing a
# common bank (when the allocation size is smaller than a bank).
header "checking if multiple allocations share a common bank"
BANK_SIZE=1024
if save_trace -k check/shared-bank.t -t -s $BANK_SIZE
then
    if no_failed_allocs
    then
        allocs=`grep ^mm_alloc _out | sed 's/.*-> //'`
        a1=`head -n 1 <<< "$allocs"`
        a2=`tail -n 3 <<< "$allocs" | head -n 1`
        a3=`tail -n 2 <<< "$allocs" | head -n 1`
        a4=`tail -n 1 <<< "$allocs"`
        if [ "$a1" -a "$a2" -a "$a3" -a "$a4" ]
        then
            success=1
            for a in $a2 $a3 $a4
            do
                diff=`perl -e "print $a - $a1"`
                if [ $diff -gt $BANK_SIZE ]
                then
                    success=0
                    break
                fi
            done
        fi
        if [ "$success" -eq 1 ]; check
        then grant 0.5
        else echo "allocations where not done on the same bank"
        fi
    fi
fi
echo

# +1pt if your code can optimize for energy usage by deactivating banks
# released by mm_free.
header "checking if code optimizes for energy usage"
if save_trace -k simple-big.t
then
    ndeact=$(cut -d / -f 2 <<< "`summary_field $F_ACT_DEACT`")
    if [ $ndeact -gt 0 ]; check
    then grant 1
    else echo "freed banks are never deactivated"
    fi
fi
echo

# +0,5pt if your code tries to increase locality, i.e. mm_alloc tries to reuse
# the banks most recently deallocated by mm_free.
header "checking for locality"
if save_trace -k check/locality.t -t
then
    if no_failed_allocs
    then
        trace=`grep ^mm_ _out | tail -n 2`
        free=`head -n 1 <<< "$trace" | sed 's/mm_free(st, \([^)]\+\));/\1/'`
        alloc=`tail -n 1 <<< "$trace"  | cut -d ' ' -f 5`
        if [ "$free" -a "$free" = "$alloc" ]; check
        then grant 1
        else echo "allocation did not use most recently deallocated bank"
        fi
    fi
fi
echo

# +0,5pt if your code supports multiple modules and tries to spread allocations
# evenly across modules (minimizes the imbalance factor).
header "checking if allocations are spread evenly across multiple modules"
if save_trace -k check/fifo-rand.t -v 1 -t -s 128 -m 3
then
    if no_failed_allocs
    then
        # get the second visualisation line after the last mm_alloc is logged
        # and check if at least one but less than half of the banks in each
        # module has been activated
        lineno=$(expr `grep -n ^mm_alloc _out | tail -n 1 | cut -d: -f1` + 2)
        vis=$(head -n $lineno _out | tail -n 1 |
              sed 's/\(^[^[]*\[\)\|\(\] .*$\|\[\)//g')
        a1=$(printf `cut -d ] -f 1 <<< "$vis"` | sed s/\\.//g | wc -m)
        a2=$(printf `cut -d ] -f 2 <<< "$vis"` | sed s/\\.//g | wc -m)
        a3=$(printf `cut -d ] -f 3 <<< "$vis"` | sed s/\\.//g | wc -m)
        [ "$a1" -a "$a2" -a "$a3" -a             \
          $a1 -gt 0 -a $a2 -gt 0 -a $a3 -gt 0 -a \
          $a1 -lt 21 -a $a2 -lt 21 -a $a3 -lt 21 ]
        if check
        then grant 0.5
        else echo "allocations not evenly spread before first free"
        fi
    fi
fi
# TODO: check module imbalance factor
echo

# -1pt if any functions contain >30 LoC
header "checking if any functions contain more than 30 lines of code"
success=1
for f in $src $hdr
do
    cmd="check/line_check.sh $f"
    out=`LIMIT=30 bash check/line_check.sh $f 2>&1`
    if ! check_fail
    then
        success=0
        grant -1
        break
    fi
done
[ $success -eq 1 ] && ok
echo

# -1pt if your source files are not neatly indented.
header "assuming source files are neatly indented (needs manual check)"
ok
echo

# report grade
echo "preliminary grade: $grade"

# cleanup
rm -f _out
