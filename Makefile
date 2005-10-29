# This line is needed on some machines.
MAKE=make
#
# Chose one of these mallocs:
MALLOC=smalloc
#
# Gnu malloc, fastest but uses most memory.
#MALLOC=gmalloc
#
# Use the standard malloc on your system:
#MALLOC=sysmalloc
#
# Lars special malloc for LPmud. Uses least memory, but has an inefficient
# recombination scheme that may slow down the game after long uptimes.
#MALLOC=malloc
#
# A malloc that will even reclaim memory that has been lost. Can be used to
# searching for lost memory.
#MALLOC=gc
#
# Set MUD_LIB to the directory which contains the mud data. Was formerly
# defined in config.h !
MUD_LIB = /usr/mud/new/game/lib
#
# Set BINDIR to the directory where you want to install the executables.
BINDIR = /usr/mud/new/game/bin
#
# Define what random number generator to use.
# If no one is specified, a guaranteed bad one will be used.
#
RAND=RANDOM
YFLAGS=-d
#
# If you don't have 'strchr', then add next flag to CFLAGS.
# -Dstrchr=index -Dstrrchr=rindex
#
PROFIL=
#PROFIL=-p -DMARK
#PROFIL=-pg
#Enable warnings from the compiler, if wanted.
#WARN=
WARN= -Wall -Wshadow -Dlint
#
# Enable run time debugging. It will use more time and space.
# When the flag is changed, be sure to recompile everything.
# Simply comment out this line if not wanted.
# If you change anything in the source, you are strongly encouraged to have
# DEBUG defined.
# If you will not change anything, you are still strongly to have it defined,
# as long as the game is not bug free.
DEBUG=-DDEBUG
MPATH=-DMUD_LIB='"$(MUD_LIB)"'
#
# If you have a NeXT, add this to CFLAGS:
# -DNeXT -bsd
#
CFLAGS= -g -D$(RAND) -DMALLOC_$(MALLOC) $(WARN) -DTRACE_CODE $(PROFIL) $(DEBUG) $(MPATH) -DBINDIR='"$(BINDIR)"'
CC=gcc
#
# Add extra libraries here.
#
LIBS= -lm # util/gc/gc.a
MFLAGS = "BINDIR=$(BINDIR)" "MUD_LIB=$(MUD_LIB)"
#
# Add str.c here if you don't have memcpy() or strtol()
#
SRC=lex.c main.c interpret.c simulate.c object.c backend.c array.c\
    comm1.c ed.c regexp.c wiz_list.c swap.c $(MALLOC).c\
    call_out.c otable.c dumpstat.c stralloc.c hash.c indentp.c port.c\
    access_check.c clilib.c parse_old.c parse.c prelang.y postlang.y \
    simul_efun.c
#
# Add str.o here if you don't have memcpy() or strtol().
# Add alloca.o if you don't have alloca() on your machine.
#
OBJ=lang.o lex.o main.o interpret.o simulate.o object.o backend.o array.o\
    comm1.o ed.o regexp.o wiz_list.o swap.o $(MALLOC).o\
    call_out.o otable.o dumpstat.o stralloc.o hash.o indentp.o port.o\
    access_check.o clilib.o parse_old.o parse.o simul_efun.o

driver: $(OBJ)
	-mv driver driver.old
	$(CC) $(CFLAGS) $(OBJ) -o driver $(LIBS)

list_funcs:
	@$(CC) -E $(CFLAGS) func_spec.c

make_func.c: make_func.y
	yacc make_func.y
	mv y.tab.c make_func.c

make_func: make_func.o
	$(CC) make_func.o -o make_func

make_func.o: config.h

install: driver
	install -c $? $(BINDIR)/parse

install.utils:
	(cd util; $(MAKE) $(MFLAGS) install)

utils:	
	(cd util; $(MAKE) $(MFLAGS))

parse: driver
	-mv parse parse.old
	cp driver parse

lint: *.c
	lint *.c

call_out.o: object.h interpret.h

swap.o: swap.c object.h config.h interpret.h exec.h

lang.o: lang.c config.h object.h interpret.h exec.h instrs.h switch.h

lang.c lang.h: lang.y
	yacc -d lang.y
	mv y.tab.c lang.c
	mv y.tab.h lang.h

lexical.o: lexical.c config.h

lexical.c: lexical.l

interpret.o: interpret.h config.h object.h instrs.h exec.h patchlevel.h\
	switch.h lang.h

simulate.o: interpret.h object.h config.h sent.h wiz_list.h exec.h lang.h

wiz_list.o: wiz_list.h interpret.h

main.o: config.h object.h interpret.h lex.h

clean:
	-rm -f *.o lang.h lang.c lexical.c mon.out *.ln tags
	-rm -f parse core frontend mudlib/core mudlib/debug.log TAGS frontend.c
	-rm -f config.status lpmud.log
	(cd util ; echo "Cleaning in util." ; $(MAKE) clean)

frontend: comm2.o frontend.o
	$(CC) $(CFLAGS) comm2.o frontend.o -o frontend $(LIBS)

tags: $(SRC)
	ctags $(SRC)

TAGS: $(SRC)
	etags $(SRC)

count_active: count_active.o
	$(CC) count_active.o -o count_active

object.o: interpret.h object.h sent.h config.h wiz_list.h exec.h

backend.o: object.h config.h interpret.h wiz_list.h exec.h

comm1.o: comm.h sent.h interpret.h object.h config.h mudwho.h patchlevel.h

clilib.o: config.h

comm2.o: config.h comm.h

frontend.o: frontend.c config.h comm.h

ed.o: regexp.h object.h config.h interpret.h exec.h

regexp.o: regexp.h

otable.o: config.h object.h interpret.h

dumpstat.o: object.h interpret.h exec.h

parse_old.o: interpret.h config.h object.h wiz_list.h

parse.o: interpret.h config.h object.h wiz_list.h

stralloc.o: config.h

lex.o: config.h instrs.h interpret.h exec.h efun_defs.c lang.h lex.h

array.o: interpret.h object.h config.h wiz_list.h regexp.h exec.h

access_check.o: config.h

lang.y efun_defs.c: func_spec.c make_func prelang.y postlang.y config.h
	./make_func > efun_defs.c

simul_efun.o: interpret.h object.h exec.h
