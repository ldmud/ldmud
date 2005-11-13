#
# Atari makefile modified (quick and dirty) for OS2
# (there is no reason not to use an OS/2 yacc but
#  I used a unix yacc and continued on OS/2)
#
#					Foslay@tubmud
#

# Generated automatically from Makefile.in by configure.
# These lines are needed on some machines.
MAKE=make
#SHELL=/bin/sh
#
CC=gcc
#
YACC = yacc
YACCTAB = y_tab.
#
RM = rm -f
MV = mv
CP = cp
#
# Chose one of these mallocs:
# Satoria's malloc. Uses little memory. With FAST_FIT, it is also one of the
# fastest. Required for garbage collection.
MALLOC=smalloc
# must not #define SBRK_OK without static linking on __RS6000__
# How can static linking be done?
#
# Use the standard malloc on your system:
#MALLOC=sysmalloc
#
# Set MUD_LIB to the directory which contains the mud data. Was formerly
# defined in config.h !
MUD_LIB = /games/mud/lib
#
# Set BINDIR to the directory where you want to install the executables.
BINDIR = /games/mud/bin
#
#PROFIL= -DOPCPROF -DVERBOSE_OPCPROF
#PROFIL=-p -DMARK
#PROFIL=-pg
PROFIL=
#Enable warnings from the compiler, if wanted.
#defining WARN will disable gcc from finding crt0.o on the atari.
#WARN= # no warning options - will work with all compilers :-)
#WARN= -Wall -Wshadow -Dlint
#WARN= -Wall -Wshadow -Wno-parentheses # gcc settings
#
# Optimization and source level debugging options.
#For TT/Falcon, add '-m68020 -m68881 -DATARI_TT' to use the extra
#CPU and FPU instructions.
OPTIMIZE = -O -g -fstrength-reduce -fomit-frame-pointer # high optimization
#OPTIMIZE= -O -g -fstrength-reduce # for better debugging
#OPTIMIZE= -g # no optimization; for frequent recompilations.
#
# debugging options: [-DDEBUG] [-DYYDEBUG|-DYYDEBUG=1] [-DTRACE_CODE]
# -DDEBUG: Enable run time debugging. It will use more time and space.
# When the flag is changed, be sure to recompile everything.
# Simply comment out this line if not wanted.
# If you change anything in the source, you are strongly encouraged to have
# DEBUG defined.
# If you will not change anything, you are still encouraged to have
# it defined, if you want to locate any game driver bugs.
#DEBUG=-DDEBUG -DTRACE_CODE
DEBUG=
#
MPATH=-DMUD_LIB='"$(MUD_LIB)"' -DBINDIR='"$(BINDIR)"'
#
CFLAGS=  $(OPTIMIZE) $(DEBUG) -DMALLOC_$(MALLOC) $(WARN) $(MPATH) $(PROFIL)
#
#LIBS hides GNULIB on atari.
#LIBS=
#
LDFLAGS=
#
#Note that, if you use smalloc with SBRK_OK(which is the default), and
#don't link statically, it could happen that the system malloc is linked
#along with the one from smalloc; this combination is bound to cause crashes.
MFLAGS = "BINDIR=$(BINDIR)" "MUD_LIB=$(MUD_LIB)"
#
SRC=lex.c main.c interpret.c simulate.c object.c backend.c array.c\
    comm1.c ed.c regexp.c mapping.c wiz_list.c swap.c $(MALLOC).c\
    call_out.c otable.c dumpstat.c stralloc.c hash.c port.c\
    access_check.c parse_old.c parse.c prolang.y\
    simul_efun.c sprintf.c gcollect.c closure.c filestat.c
OBJ=lang.o lex.o main.o interpret.o simulate.o object.o backend.o array.o\
    comm1.o ed.o regexp.o mapping.o wiz_list.o swap.o $(MALLOC).o\
    call_out.o otable.o dumpstat.o stralloc.o hash.o port.o\
    access_check.o parse_old.o parse.o simul_efun.o sprintf.o\
    gcollect.o closure.o filestat.o

driver: $(OBJ)
	$(CC) $(OPTIMIZE) $(LDFLAGS) $(OBJ) -Zmts -o driver.exe

install: driver
	install -c $? $(BINDIR)/parse

install.utils:
	(cd util; $(MAKE) $(MFLAGS) install)

utils: 
	(cd util; $(MAKE) $(MFLAGS))

parse: driver
	-$(MV) parse parse.old
	$(CP) driver parse

#lint: *.c
#	lint *.c

#clean:
#	$(RM) $(YACCTAB)h $(YACCTAB)c make_func.c *.o make_func efun_defs.c lang.y
#	$(RM) lang.h lang.c y.output tags TAGS frontend.c frontend
#	$(RM) parse core mudlib/core mudlib/debug.log lpmud.log
#	(cd util ; echo "Cleaning in util." ; $(MAKE) clean)

#include has problems when the toplevel source file is in a subdirectory.
#frontend: hosts/atari/frontend.c config.h comm.h
#	cp hosts/atari/frontend.c .
#	$(CC) $(CFLAGS) frontend.c -o frontend $(LIBS)
#	rm frontend.c
#	fixstk 16k frontend

#distclean: clean
#	$(RM) driver config.status machine.h Makefile

#tags: $(SRC)
#	ctags $(SRC)

#TAGS: $(SRC)
#	etags $(SRC)

#make_func.c: make_func.y
#	$(YACC) make_func.y
#	$(MV) $(YACCTAB)c make_func.c
	
make_func.o : make_func.c 
	$(CC) $(CFLAGS) -DYACC='"$(YACC)"' -c make_func.c

make_func: make_func.o hash.o
	$(CC) $(OPTIMIZE) $(LDFLAGS) make_func.o hash.o -o make_func
	fixstk 96k make_func

interpret.s : interpret.c
	$(CC) $(CFLAGS) -S interpret.c

interpret.o : interpret.s
	$(CC) $(CFLAGS) -c interpret.s

#lang.y efun_defs.c instrs.h: func_spec make_func prolang.y config.h
#	$(RM) efun_defs.c
#	$(RM) lang.y
#	./make_func > efun_defs.c

lang.c lang.h: lang.y
	$(YACC) -d -v lang.y
	$(MV) $(YACCTAB)c lang.c
	
	$(MV) $(YACCTAB)h lang.h

$(MALLOC).o : $(MALLOC).c driver.h config.h machine.h port.h
	$(RM) smalloc.o
	$(RM) sysmalloc.o
	$(CC) $(CFLAGS) -c $(MALLOC).c

#--------------------------------------------------------
# Dependencies, manual and automatic.

array.o : instrs.h lang.h

closure.o : instrs.h lang.h

dumpstat.o : instrs.h lang.h

gcollect.o : instrs.h lang.h

interpret.o : instrs.h lang.h

lang.o : driver.h config.h machine.h lex.h interpret.h object.h \
    instrs.h port.h switch.h stralloc.h

lex.o : instrs.h lang.h efun_defs.c

simul_efun.o : instrs.h lang.h

simulate.o : instrs.h

sprintf.o : instrs.h

# --- DO NOT MODIFY THIS LINE -- AUTO-DEPENDS FOLLOW ---
access_check.o : filestat.h comm.h access_check.h driver.h config.h sent.h \
    object.h interpret.h port.h machine.h exec.h datatypes.h \
    hosts/amiga/patchfloat.h

array.o : wiz_list.h swap.h stralloc.h simulate.h rxcache.h regexp.h \
    prolang.h object.h mapping.h main.h interpret.h instrs.h exec.h \
    datatypes.h backend.h array.h my-alloca.h driver.h smalloc.h sent.h \
    lang.h port.h machine.h config.h hosts/amiga/patchfloat.h

backend.o : wiz_list.h swap.h stralloc.h smalloc.h simulate.h rxcache.h \
    regexp.h object.h my-alloca.h mapping.h main.h lex.h interpret.h \
    gcollect.h filestat.h exec.h ed.h comm.h call_out.h array.h backend.h \
    hosts/amiga/nsignal.h driver.h datatypes.h sent.h instrs.h config.h \
    port.h machine.h hosts/amiga/patchfloat.h

call_out.o : wiz_list.h stralloc.h simulate.h object.h main.h interpret.h \
    gcollect.h exec.h comm.h closure.h backend.h array.h call_out.h \
    driver.h smalloc.h sent.h instrs.h datatypes.h port.h machine.h \
    config.h hosts/amiga/patchfloat.h

closure.o : switch.h simul_efun.h simulate.h stralloc.h prolang.h object.h \
    main.h lex.h instrs.h interpret.h exec.h array.h closure.h my-alloca.h \
    driver.h sent.h smalloc.h lang.h datatypes.h port.h machine.h config.h \
    hosts/amiga/patchfloat.h

comm1.o : util/erq.h wiz_list.h stralloc.h simulate.h sent.h object.h \
    main.h interpret.h gcollect.h filestat.h exec.h ed.h backend.h array.h \
    access_check.h comm.h hosts/amiga/nsignal.h telnet.h my-alloca.h \
    driver.h smalloc.h instrs.h datatypes.h config.h port.h machine.h \
    hosts/amiga/patchfloat.h

dumpstat.o : smalloc.h simulate.h prolang.h object.h mapping.h instrs.h \
    interpret.h filestat.h exec.h comm.h array.h dumpstat.h driver.h \
    datatypes.h sent.h lang.h config.h port.h machine.h \
    hosts/amiga/patchfloat.h

ed.o : stralloc.h simulate.h rxcache.h regexp.h object.h main.h interpret.h \
    gcollect.h filestat.h comm.h closure.h ed.h driver.h smalloc.h sent.h \
    instrs.h exec.h datatypes.h config.h port.h machine.h \
    hosts/amiga/patchfloat.h

filestat.o : stralloc.h simulate.h hash.h gcollect.h comm.h backend.h \
    filestat.h driver.h smalloc.h sent.h object.h instrs.h interpret.h \
    exec.h config.h port.h machine.h datatypes.h hosts/amiga/patchfloat.h

gcollect.o : wiz_list.h swap.h stralloc.h smalloc.h simul_efun.h simulate.h \
    sent.h rxcache.h prolang.h otable.h object.h mapping.h main.h lex.h \
    instrs.h interpret.h filestat.h exec.h ed.h comm.h closure.h call_out.h \
    backend.h array.h gcollect.h driver.h datatypes.h regexp.h lang.h \
    config.h port.h machine.h hosts/amiga/patchfloat.h

hash.o : hash.h

interpret.o : wiz_list.h switch.h swap.h sprintf.h smalloc.h stralloc.h \
    simul_efun.h simulate.h sent.h random.h prolang.h parse.h otable.h \
    object.h mapping.h main.h lex.h instrs.h gcollect.h exec.h ed.h comm.h \
    closure.h call_out.h backend.h array.h interpret.h my-rusage.h \
    my-alloca.h driver.h datatypes.h lang.h port.h machine.h config.h \
    hosts/amiga/patchfloat.h

lang.o : wiz_list.h switch.h swap.h stralloc.h simul_efun.h simulate.h \
    object.h mapping.h main.h lex.h instrs.h interpret.h gcollect.h exec.h \
    closure.h backend.h array.h prolang.h my-alloca.h driver.h smalloc.h \
    sent.h datatypes.h lang.h port.h machine.h config.h \
    hosts/amiga/patchfloat.h

lex.o : efun_defs.c hosts/amiga/socket.h stralloc.h simul_efun.h simulate.h \
    prolang.h patchlevel.h main.h instrs.h hash.h gcollect.h filestat.h \
    exec.h comm.h closure.h array.h lex.h my-alloca.h driver.h \
    hosts/amiga/socket_sim.h hosts/amiga/socket_tcp.h smalloc.h object.h \
    interpret.h sent.h lang.h config.h datatypes.h port.h machine.h \
    hosts/amiga/socket_sim_protos.h hosts/amiga/patchfloat.h

main.o : wiz_list.h swap.h stralloc.h simul_efun.h simulate.h rxcache.h \
    random.h patchlevel.h otable.h object.h mapping.h lex.h interpret.h \
    gcollect.h filestat.h comm.h array.h backend.h main.h \
    hosts/amiga/socket.h my-alloca.h driver.h exec.h smalloc.h sent.h \
    instrs.h regexp.h datatypes.h config.h hosts/amiga/socket_sim.h \
    hosts/amiga/socket_tcp.h port.h machine.h \
    hosts/amiga/socket_sim_protos.h hosts/amiga/patchfloat.h

make_func.o : interpret.h hash.h exec.h my-alloca.h driver.h datatypes.h \
    port.h machine.h config.h hosts/amiga/patchfloat.h

mapping.o : wiz_list.h stralloc.h smalloc.h simulate.h regexp.h object.h \
    main.h interpret.h gcollect.h backend.h array.h mapping.h my-alloca.h \
    driver.h datatypes.h sent.h instrs.h exec.h port.h machine.h config.h \
    hosts/amiga/patchfloat.h

object.o : wiz_list.h swap.h stralloc.h simulate.h sent.h random.h otable.h \
    mapping.h main.h interpret.h filestat.h exec.h comm.h closure.h \
    backend.h array.h object.h my-alloca.h driver.h smalloc.h instrs.h \
    datatypes.h config.h port.h machine.h hosts/amiga/patchfloat.h

otable.o : simulate.h object.h interpret.h hash.h gcollect.h comm.h \
    otable.h driver.h sent.h instrs.h exec.h datatypes.h port.h machine.h \
    config.h hosts/amiga/patchfloat.h

parse.o : wiz_list.h stralloc.h simulate.h object.h main.h instrs.h \
    interpret.h closure.h array.h parse.h driver.h smalloc.h sent.h exec.h \
    datatypes.h port.h machine.h config.h hosts/amiga/patchfloat.h

parse_old.o : wiz_list.h simulate.h random.h object.h main.h interpret.h \
    array.h parse.h driver.h sent.h instrs.h exec.h datatypes.h smalloc.h \
    port.h machine.h config.h hosts/amiga/patchfloat.h

port.o : hosts/crypt.c main.h backend.h my-rusage.h driver.h interpret.h \
    object.h port.h machine.h config.h datatypes.h exec.h \
    hosts/amiga/patchfloat.h

random.o : random.h driver.h port.h machine.h config.h \
    hosts/amiga/patchfloat.h

regexp.o : regexp.h driver.h port.h machine.h config.h \
    hosts/amiga/patchfloat.h

rxcache.o : stralloc.h smalloc.h regexp.h hash.h gcollect.h comm.h \
    rxcache.h driver.h datatypes.h object.h interpret.h exec.h sent.h \
    port.h machine.h config.h hosts/amiga/patchfloat.h

simul_efun.o : swap.h stralloc.h simulate.h prolang.h parse.h object.h \
    lex.h instrs.h interpret.h gcollect.h exec.h array.h simul_efun.h \
    my-alloca.h driver.h smalloc.h sent.h lang.h datatypes.h port.h \
    machine.h config.h hosts/amiga/patchfloat.h

simulate.o : wiz_list.h swap.h stralloc.h simul_efun.h sent.h rxcache.h \
    prolang.h otable.h object.h mapping.h main.h lex.h instrs.h interpret.h \
    filestat.h exec.h ed.h dumpstat.h comm.h closure.h call_out.h backend.h \
    array.h simulate.h hosts/amiga/nsignal.h my-alloca.h driver.h smalloc.h \
    regexp.h lang.h datatypes.h config.h port.h machine.h \
    hosts/amiga/patchfloat.h

sprintf.o : swap.h stralloc.h simul_efun.h simulate.h sent.h prolang.h \
    object.h main.h instrs.h interpret.h exec.h array.h sprintf.h driver.h \
    smalloc.h lang.h datatypes.h port.h machine.h config.h \
    hosts/amiga/patchfloat.h

stralloc.o : simulate.h hash.h gcollect.h comm.h stralloc.h driver.h sent.h \
    object.h instrs.h interpret.h exec.h smalloc.h port.h machine.h \
    config.h datatypes.h hosts/amiga/patchfloat.h

swap.o : hosts/amiga/socket.h wiz_list.h stralloc.h simul_efun.h simulate.h \
    prolang.h object.h mapping.h main.h interpret.h gcollect.h exec.h \
    comm.h array.h swap.h driver.h hosts/amiga/socket_sim.h \
    hosts/amiga/socket_tcp.h smalloc.h sent.h instrs.h lang.h datatypes.h \
    port.h machine.h config.h hosts/amiga/socket_sim_protos.h \
    hosts/amiga/patchfloat.h

sysmalloc.o :

wiz_list.o : stralloc.h object.h main.h interpret.h gcollect.h backend.h \
    array.h wiz_list.h driver.h smalloc.h exec.h datatypes.h instrs.h \
    port.h machine.h config.h hosts/amiga/patchfloat.h

# --- DO NOT MODIFY THIS LINE -- AUTO-DEPENDS PRECEDE ---
