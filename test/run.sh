#! /bin/sh

if [ ! -x ${DRIVER:=../src/ldmud} ]
then
    echo "Did not find the driver."
    echo "Please specify via DRIVER environment variable."
    exit 1
fi

mkdir -p log

FAILED=""

DRIVER_DEFAULTS="-u-1 -E 0 --no-compat -e -N --cleanup-time -1 --reset-time -1
    --max-array 0 --max-callouts 0 --max-bytes 0 --max-file 0 -s-1
    -sv-1 --hard-malloc-limit unlimited --min-malloc 0 -ru0 -rm0 -rs0
    --no-strict-euids
    --no-wizlist-file --check-refcounts --check-state 2 --access-file none
    --access-log none -f test 65432"

export DRIVER DRIVER_DEFAULTS

for testdir in ${@:-t-*}
do
    if [ -d "${testdir}" ]
    then
	${DRIVER} ${DRIVER_DEFAULTS} -Mmaster -m"${testdir}" \
              --debug-file "../log/result.${testdir}.log"  > /dev/null \
        || { echo "Test ${testdir} FAILED."; FAILED="${FAILED}\n\t${testdir}"; }
    else
        export TEST_LOGFILE=./log/result.${testdir}.log
        case ${testdir} in
        *.c)
            ${DRIVER} ${DRIVER_DEFAULTS} -M"${testdir}" -m. \
                  --debug-file ${TEST_LOGFILE} > /dev/null \
            || { echo "Test ${testdir} FAILED."; FAILED="${FAILED}\n\t${testdir}"; }
        ;;
        *.sh)
            /bin/sh ./${testdir} \
            || { echo "Test ${testdir} FAILED."; FAILED="${FAILED}\n\t${testdir}"; }
        ;;
        esac
    fi
done

if [ -z "${FAILED}" ]
then
    echo "Tests run successfully."
else
    echo "The following tests FAILED:"
    echo -e "${FAILED}"
    exit 1
fi
