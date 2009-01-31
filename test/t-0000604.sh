echo
echo "Running test for #0000604:"
echo "--------------------------"
ulimit -c 0
OPTIONS=""
# filter -N option
for option in ${DRIVER_DEFAULTS}; do
    case ${option} in
    -N) ;;
    *)  OPTIONS="${OPTIONS} ${option}" ;;
    esac
done
# the crash is alignment sensitive, so check command lines of varying lengths
for extra in 0 01 012 0123 01234 012345 0123456 01234567; do
    ${DRIVER} --erq "/bin/true ${extra}" ${OPTIONS} -m. \
        -Mgeneric/shutdown --debug-file=/dev/null > ${TEST_LOGFILE} || exit 1
done
echo "Success."
