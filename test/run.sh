#! /bin/sh

if [ ! -x ${DRIVER:=../src/ldmud} ]
then
    echo "Did not find the driver."
    echo "Please specify via DRIVER environment variable."
    exit 1
fi

mkdir -p log

PARALLEL=8

DRIVER_DEFAULTS="-u-1 -E 0 --no-compat -e -N --cleanup-time -1 --reset-time -1
    --max-array 0 --max-callouts 0 --max-bytes 0 --max-file 0 -s-1
    -sv-1 --hard-malloc-limit unlimited --min-malloc 0 -ru0 -rm0 -rs0
    --no-strict-euids
    --no-wizlist-file --check-refcounts --check-state 2 --access-file none
    --access-log none -f test"

if ${DRIVER} --options | grep -q 'Python supported'; then
    DRIVER_DEFAULTS="$DRIVER_DEFAULTS --python-script startup.py"
fi

while [ $# -gt 0 ]
do
    case "$1" in
        -h | --help)
            echo "Usage: $0 [-h] [-v] [TEST ...]"
            echo ""
            echo "Run test cases for the LDMud driver."
            echo ""
            echo "Optional arguments:"
            echo "  -h, --help      Show this help message and exit."
            echo "  -v, --valgrind  Run the tests with Valgrind memcheck."
            echo ""
            echo "Positional arguments:"
            echo "  TEST            Test case to run, either a directory or a single .c file."
            echo "                  (If none is given all tests are executed.)"
            exit
            ;;

        -v | --valgrind)
            DRIVER="valgrind --tool=memcheck --error-exitcode=1 $VALGRIND_OPTS --suppressions=../src/valgrind/ldmud.supp -- $DRIVER -DVALGRIND"
            shift
            continue
            ;;
    esac

    break
done

export DRIVER DRIVER_DEFAULTS
export GCOV_PREFIX_STRIP=100

run_single_test()
{
    export TESTNAME="$1"
    export PORT="$2"
    export TEST_LOGFILE=./log/result.${TESTNAME}.log
    export TEST_OUTPUTFILE=./log/result.${TESTNAME}.out
    export TEST_ERRFILE=./log/result.${TESTNAME}.err
    export GCOV_PREFIX="$PWD/coverage/${TESTNAME}/"
    export LLVM_PROFILE_FILE="$PWD/coverage/${TESTNAME}.profraw"

    if [ -d "${TESTNAME}" ]
    then
	${DRIVER} ${DRIVER_DEFAULTS} -Mmaster -m"${TESTNAME}" ${PORT} \
              --debug-file ".${TEST_LOGFILE}"  > "${TEST_OUTPUTFILE}" 2> "${TEST_ERRFILE}" \
        || { echo "Test ${TESTNAME} FAILED."; echo "\t${TESTNAME}" >> ./log/fails; }
    else
        case ${TESTNAME} in
        *.c)
            ${DRIVER} ${DRIVER_DEFAULTS} -M"${TESTNAME}" -m. ${PORT} \
                  --debug-file ${TEST_LOGFILE} > "${TEST_OUTPUTFILE}" 2> "${TEST_ERRFILE}" \
            || { echo "Test ${TESTNAME} FAILED."; echo "\t${TESTNAME}" >> ./log/fails; }
        ;;
        *.sh)
            /bin/sh ./${TESTNAME} 2> "${TEST_ERRFILE}"\
            || { echo "Test ${TESTNAME} FAILED."; echo "\t${TESTNAME}" >> ./log/fails; }
        ;;
        esac
    fi

    cat "${TEST_ERRFILE}"
    echo "X" >&3
}

: > ./log/fails

[ -e ./log/semaphore ] || mkfifo ./log/semaphore
exec 3<>./log/semaphore
i=0
while [ $i -le $PARALLEL ]; do
    echo "X" >&3
    i=$(($i+1))
done

port=65432
for testdir in ${@:-t-*}
do
    read dummy <&3
    run_single_test "$testdir" "$port" &
    port=$(($port-1))
done

wait

if [ -s "./log/fails" ]
then
    echo "The following tests FAILED:"
    sort < "./log/fails"
    exit 1
else
    echo "Tests run successfully."
fi
