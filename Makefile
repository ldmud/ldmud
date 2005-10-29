#
# Define what random number generator to use.
# If no one is specified, a guaranteed bad one will be used.
#
RAND=RANDOM
YFLAGS=-d
LEX=lex
CFLAGS=-D$(RAND) -g -DLEX -Drcheck -DMSTATS
CC=cc
LIBS=
SRC=lang.y lexical.l main.c lnode.c interpret.c simulate.c object.c backend.c\
    comm1.c ed.c regexp.c wiz_list.c swap.c malloc.c string_space.c vpopen.c

OBJ=lang.o lexical.o main.o lnode.o interpret.o simulate.o object.o backend.o\
    comm1.o ed.o regexp.o wiz_list.o swap.o string_space.o vpopen.o

parse: $(OBJ)
	$(CC) $(CFLAGS) $(OBJ) -o parse $(LIBS)

debug: $(OBJ)
	$(CC) $(CFLAGS) $(OBJ) -o debug $(LIBS)

swap.o: swap.c lnode.h object.h config.h

lang.o: lang.c lnode.h config.h

lang.c y.tab.h: lang.y

lexical.o: lexical.c lnode.h config.h y.tab.h

lexical.c: lexical.l

lnode.o: lnode.h config.h y.tab.h

interpret.o: lnode.h interpret.h config.h y.tab.h

simulate.o: interpret.h object.h config.h sent.h comm.h lnode.h wiz_list.h y.tab.h

vpopen.o: vpopen.c

wiz_list.o: wiz_list.h

main.o: lnode.h config.h object.h interpret.h

clean:
	rm -f *.o y.tab.h lang.c lexical.c comm1.c comm2.c comm.h *.ln mon.out
	rm -f parse core frontend mudlib/core mudlib/debug.log TAGS frontend.c
	find . -name '*.i' -exec rm -f \{} \;
	rm -f config.status lpmud.log mudlib/mon.out
	find . -name '*~' -exec rm -f \{} \;
	find . -name '#*' -exec rm -f \{} \;
	find . -name '*.orig' -exec rm -f \{} \;
	find . -name '*.rej' -exec rm -f \{} \;

object.o: interpret.h object.h sent.h lnode.h config.h

backend.o: object.h lnode.h config.h interpret.h wiz_list.h

comm1.o: comm.h sent.h interpret.h object.h config.h

comm2.o: config.h comm.h

frontend: comm2.o frontend.o
	$(CC) $(CFLAGS) comm2.o frontend.o -o frontend $(LIBS)

frontend.o: frontend.c config.h comm.h

ed.o: regexp.h object.h config.h

regexp.o: regexp.h

TAGS: $(SRC)
	etags $(SRC)

diff:
	make_diffs

count_active: count_active.o
	$(CC) count_active.o -o count_active

all: parse count_active
