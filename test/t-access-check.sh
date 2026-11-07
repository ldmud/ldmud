#! /bin/sh

set -e
ulimit -c 0

# Configure the driver with a --access-file policy that rejects conns from
# 127.0.0.1 and a --access-log we can grep for rejections.
ACCESS_FILE="${PWD}/access-check/ACCESS.ALLOW"
ACCESS_LOG="${PWD}/log/access.${TESTNAME}.log"

rm -f "${ACCESS_LOG}"

${DRIVER} ${DRIVER_DEFAULTS} -maccess-check -Mmaster.c \
    --access-file "${ACCESS_FILE}" --access-log "${ACCESS_LOG}" ${PORT} \
    --debug-file ".${TEST_LOGFILE}" > "${TEST_OUTPUTFILE}" 2>&1

# Based on the ACCESS_FILE, we expect to have logged a rejection from the
# test master.c connecting back to itself on 127.0.0.1
grep -q "127.0.0.1: denied" "${ACCESS_LOG}"
