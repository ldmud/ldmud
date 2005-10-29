rm -f comm1.c comm2.c comm.h frontend.c comm1.o comm2.o frontend.o
#
#
case "$1" in
inet)
	COMM1C=inet-comm1.c
	COMM2C=inet-comm2.c
	COMMH=inet-comm.h
	LN="ln"
	FRONT=fr-fork.c
	;;
inet-poll)
	COMM1C=inet-comm1.c
	COMM2C=inet-comm2.c
	COMMH=inet-comm.h
	LN="ln"
	FRONT=fr-time.c
	;;
inet-poll)
	COMM1C=inet-comm1.c
	COMM2C=inet-comm2.c
	COMMH=inet-comm.h
	LN="ln"
	FRONT=fr-time.c
	;;
msgq)
	COMM1C=msgq-comm1.c
	COMM2C=msgq-comm2.c
	COMMH=msgq-comm.h
	FRONT=fr-fork.c
	LN="ln"
	;;
single)
	COMM1C=single-c1.c
	COMM2C=single-c2.c
	COMMH=single-c.h
	FRONT=fr-single.c
	LN="ln"
	;;
*)
	echo "Unknown type: $1"
	exit 1
	;;
esac

COMM1C="config/$COMM1C"
COMM2C="config/$COMM2C"
COMMH="config/$COMMH"
FRONT="config/$FRONT"

echo "Linking $COMM1C to comm1.c"
$LN $COMM1C comm1.c

echo "Linking $COMM2C to comm2.c"
$LN $COMM2C comm2.c

echo "Linkin $COMMH to comm.h"
$LN $COMMH comm.h

echo "Linking $FRONT to frontend.c"
$LN $FRONT frontend.c

cat >config.status <<APA
lpmud is configured with $1.
APA
