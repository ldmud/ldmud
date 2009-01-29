#! /bin/sh

if [ ! -x ${DRIVER:=../src/ldmud} ]
then
    echo "Did not find the driver."
    echo "Please specify via DRIVER environment variable."
    exit 1
fi

mkdir -p log

FAILED=""

for testdir in ${@:-t-*}
do
    if [ -d "$testdir" ]
    then
	${DRIVER} -u-1 -E 0 -Mmaster -m"$testdir" --debug-file "../log/result.$testdir.log" --no-compat -e -N \
              --cleanup-time -1 --reset-time -1 --max-array 0 \
	      --max-callouts 0 --max-bytes 0 --max-file 0 \
	      -s-1 -sv-1 --max-malloc 0 --min-malloc 0 \
	      -ru0 -rm0 -rs0 --no-strict-euids --no-wizlist-file \
	      --check-refcounts --check-state 2 \
	      --access-file none --access-log none \
	      -f test 65432 > /dev/null || { echo "Test $testdir FAILED."; FAILED="${FAILED}\n\t$testdir"; }
    elif [ -r "$testdir" ]
    then
	${DRIVER} -u-1 -E 0 -M"$testdir" -m. --debug-file "./log/result.$testdir.log" --no-compat -e -N \
              --cleanup-time -1 --reset-time -1 --max-array 0 \
	      --max-callouts 0 --max-bytes 0 --max-file 0 \
	      -s-1 -sv-1 --max-malloc 0 --min-malloc 0 \
	      -ru0 -rm0 -rs0 --no-strict-euids --no-wizlist-file \
	      --check-refcounts --check-state 2 \
	      --access-file none --access-log none \
	      -f test 65432 > /dev/null || { echo "Test $testdir FAILED."; FAILED="${FAILED}\n\t$testdir"; }
    fi
done

if [ -z "$FAILED" ]
then
    echo "Tests run successfully."
else
    echo "The following tests FAILED:"
    echo -e "$FAILED"
    exit 1
fi
