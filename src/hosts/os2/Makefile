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

# Debugging options:
#   -DDEBUG: Enable run time debugging. It will use more time and space.
#            Nevertheless you are strongly encouraged to keep it defined.
#   -DYYDEBUG[=1]: Debug output for the LPC compiler.
#   -DTRACE_CODE: Enable the LPC code tracing.
#   -DCHECK_STRINGS: Enable the shared string checking.
#   -DKEEP_STRINGS: Shared strings are never really freed.
#   -DDEBUG_TELNET: Activates debug prints in the telnet machine
DEBUG = -DDEBUG -DTRACE_CODE -DCHECK_STRINGS # -DKEEP_STRINGS -DDEBUG_TELNET

#
MPATH=-DMUD_LIB='"$(MUD_LIB)"' -DBINDIR='"$(BINDIR)"'
#
CFLAGS=  $(OPTIMIZE) $(DEBUG) $(WARN) $(MPATH) $(PROFIL)
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
    comm.c ed.c regexp.c mapping.c wiz_list.c swap.c xalloc.c\
    call_out.c otable.c dumpstat.c stralloc.c hash.c port.c files.c\
    ptrtable.c heartbeat.c actions.c mempools.c bitstrings.c \
    access_check.c parse_old.c parse.c prolang.y\
    efuns.c simul_efun.c strfuns.c sprintf.c gcollect.c closure.c
OBJ=lang.o lex.o main.o interpret.o simulate.o object.o backend.o array.o\
    comm.o ed.o regexp.o mapping.o wiz_list.o swap.o xalloc.o\
    call_out.o otable.o dumpstat.o stralloc.o hash.o port.o files.o\
    ptrtable.o heartbeat.o actions.o mempools.o bitstrings.o \
    access_check.o parse_old.o parse.o simul_efun.o strfuns.o sprintf.o\
    efuns.o gcollect.o closure.o

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
#	$(RM) driver config.status config.log config.cache machine.h Makefile
#	$(RM) config.h

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
#	./make_func

lang.c lang.h: lang.y
	$(YACC) -d -v lang.y
	$(MV) $(YACCTAB)c lang.c
	
	$(MV) $(YACCTAB)h lang.h

#--------------------------------------------------------
# Dependencies, manual and automatic.

# --- DO NOT MODIFY THIS LINE -- AUTO-DEPENDS FOLLOW ---
access_check.o : xalloc.h filestat.h comm.h access_check.h driver.h \
    svalue.h simulate.h typedefs.h config.h port.h strfuns.h sent.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

actions.o : ../mudlib/sys/driver_hook.h ../mudlib/sys/commands.h xalloc.h \
    wiz_list.h stralloc.h smalloc.h simulate.h sent.h object.h mapping.h \
    interpret.h exec.h efuns.h dumpstat.h comm.h closure.h backend.h \
    array.h actions.h typedefs.h driver.h svalue.h main.h strfuns.h port.h \
    config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

array.o : smalloc.h xalloc.h wiz_list.h swap.h stralloc.h svalue.h \
    simulate.h rxcache.h regexp.h object.h mempools.h mapping.h main.h \
    interpret.h backend.h array.h my-alloca.h typedefs.h driver.h strfuns.h \
    sent.h port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

backend.o : ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h swap.h \
    stralloc.h stdstrings.h smalloc.h simulate.h rxcache.h regexp.h \
    random.h otable.h object.h my-alloca.h mapping.h main.h lex.h \
    interpret.h heartbeat.h gcollect.h filestat.h exec.h ed.h comm.h \
    closure.h call_out.h array.h actions.h backend.h hosts/amiga/nsignal.h \
    typedefs.h driver.h svalue.h strfuns.h sent.h port.h config.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

bitstrings.o : smalloc.h xalloc.h svalue.h simulate.h main.h lex.h \
    interpret.h bitstrings.h typedefs.h driver.h strfuns.h sent.h port.h \
    config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

call_out.o : ../mudlib/sys/debug_info.h xalloc.h wiz_list.h swap.h svalue.h \
    strfuns.h stralloc.h simulate.h object.h main.h interpret.h gcollect.h \
    exec.h comm.h closure.h backend.h array.h actions.h call_out.h \
    typedefs.h driver.h smalloc.h sent.h port.h config.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

closure.o : xalloc.h switch.h swap.h svalue.h stralloc.h stdstrings.h \
    simul_efun.h simulate.h prolang.h object.h main.h lex.h lang.h \
    interpret.h exec.h backend.h array.h closure.h my-alloca.h typedefs.h \
    driver.h smalloc.h strfuns.h ptrtable.h sent.h port.h config.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

comm.o : util/erq/erq.h ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h \
    swap.h stralloc.h stdstrings.h smalloc.h simulate.h sent.h object.h \
    main.h interpret.h gcollect.h filestat.h exec.h ed.h closure.h \
    backend.h array.h actions.h access_check.h comm.h hosts/amiga/nsignal.h \
    telnet.h my-alloca.h typedefs.h driver.h svalue.h strfuns.h config.h \
    port.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

dumpstat.o : svalue.h smalloc.h simulate.h ptrtable.h object.h mapping.h \
    filestat.h exec.h closure.h array.h dumpstat.h typedefs.h driver.h \
    strfuns.h sent.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

ed.o : xalloc.h svalue.h stralloc.h stdstrings.h simulate.h rxcache.h \
    regexp.h object.h main.h interpret.h gcollect.h filestat.h comm.h \
    closure.h actions.h ed.h typedefs.h driver.h smalloc.h strfuns.h sent.h \
    exec.h port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

efuns.o : ../mudlib/sys/strings.h ../mudlib/sys/objectinfo.h \
    ../mudlib/sys/debug_info.h xalloc.h wiz_list.h svalue.h swap.h \
    strfuns.h stralloc.h stdstrings.h smalloc.h simulate.h rxcache.h \
    random.h ptrtable.h otable.h object.h mapping.h main.h interpret.h \
    heartbeat.h dumpstat.h comm.h closure.h call_out.h backend.h array.h \
    actions.h efuns.h my-rusage.h my-alloca.h typedefs.h driver.h sent.h \
    regexp.h exec.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

files.o : ../mudlib/sys/files.h xalloc.h svalue.h smalloc.h simulate.h \
    main.h lex.h interpret.h filestat.h comm.h array.h files.h my-alloca.h \
    typedefs.h driver.h strfuns.h sent.h port.h config.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

gcollect.o : ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h swap.h \
    stralloc.h stdstrings.h smalloc.h simul_efun.h simulate.h sent.h \
    rxcache.h prolang.h parse.h otable.h object.h mapping.h main.h lex.h \
    interpret.h heartbeat.h filestat.h exec.h ed.h comm.h closure.h \
    call_out.h backend.h array.h actions.h gcollect.h typedefs.h driver.h \
    svalue.h strfuns.h ptrtable.h regexp.h port.h config.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

hash.o : hash.h

heartbeat.o : ../mudlib/sys/debug_info.h xalloc.h wiz_list.h svalue.h \
    strfuns.h simulate.h sent.h object.h interpret.h gcollect.h comm.h \
    backend.h array.h actions.h heartbeat.h typedefs.h driver.h main.h \
    port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

interpret.o : ../mudlib/sys/trace.h ../mudlib/sys/driver_hook.h xalloc.h \
    wiz_list.h switch.h swap.h svalue.h sprintf.h smalloc.h stralloc.h \
    stdstrings.h simul_efun.h simulate.h sent.h ptrtable.h prolang.h \
    parse.h otable.h object.h mapping.h main.h lex.h heartbeat.h gcollect.h \
    filestat.h exec.h efuns.h ed.h comm.h closure.h call_out.h backend.h \
    array.h actions.h interpret.h my-alloca.h typedefs.h driver.h strfuns.h \
    port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

lang.o : ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h switch.h swap.h \
    svalue.h stralloc.h stdstrings.h simul_efun.h simulate.h object.h \
    mapping.h main.h lex.h interpret.h gcollect.h exec.h closure.h \
    backend.h array.h prolang.h my-alloca.h typedefs.h driver.h smalloc.h \
    strfuns.h ptrtable.h sent.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

lex.o : efun_defs.c hosts/amiga/socket.h ../mudlib/sys/driver_hook.h \
    xalloc.h strfuns.h stralloc.h stdstrings.h simul_efun.h simulate.h \
    prolang.h patchlevel.h object.h mempools.h main.h lang.h interpret.h \
    hash.h gcollect.h filestat.h exec.h comm.h closure.h backend.h array.h \
    lex.h my-alloca.h typedefs.h driver.h ./hosts/amiga/socket_sim.h \
    ./hosts/amiga/socket_tcp.h smalloc.h ptrtable.h svalue.h sent.h port.h \
    config.h ./hosts/amiga/socket_sim_protos.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

main.o : xalloc.h wiz_list.h swap.h svalue.h stralloc.h stdstrings.h \
    simul_efun.h simulate.h rxcache.h random.h patchlevel.h otable.h \
    object.h mapping.h lex.h interpret.h gcollect.h filestat.h comm.h \
    array.h backend.h main.h hosts/amiga/socket.h my-alloca.h typedefs.h \
    driver.h smalloc.h strfuns.h ptrtable.h exec.h sent.h regexp.h \
    ./hosts/amiga/socket_sim.h ./hosts/amiga/socket_tcp.h port.h config.h \
    ./hosts/amiga/socket_sim_protos.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

make_func.o : ../mudlib/sys/driver_hook.h hash.h exec.h my-alloca.h \
    driver.h typedefs.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

mapping.o : xalloc.h wiz_list.h svalue.h stralloc.h smalloc.h simulate.h \
    regexp.h object.h main.h interpret.h gcollect.h closure.h backend.h \
    array.h mapping.h my-alloca.h typedefs.h driver.h strfuns.h sent.h \
    exec.h port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

mempools.o : xalloc.h gcollect.h mempools.h driver.h typedefs.h port.h \
    config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

object.o : ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h svalue.h swap.h \
    strfuns.h stralloc.h stdstrings.h simul_efun.h simulate.h smalloc.h \
    sent.h random.h ptrtable.h prolang.h otable.h mapping.h main.h \
    interpret.h filestat.h exec.h comm.h closure.h backend.h array.h \
    actions.h object.h my-alloca.h typedefs.h driver.h port.h config.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

otable.o : ../mudlib/sys/debug_info.h xalloc.h svalue.h simulate.h \
    strfuns.h object.h hash.h gcollect.h backend.h otable.h typedefs.h \
    driver.h sent.h main.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

parse.o : xalloc.h wiz_list.h svalue.h stralloc.h stdstrings.h simulate.h \
    object.h main.h interpret.h gcollect.h closure.h array.h actions.h \
    parse.h typedefs.h driver.h smalloc.h strfuns.h sent.h exec.h port.h \
    config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

parse_old.o : xalloc.h wiz_list.h svalue.h stralloc.h stdstrings.h \
    simulate.h random.h object.h main.h gcollect.h interpret.h closure.h \
    array.h actions.h parse.h typedefs.h driver.h smalloc.h strfuns.h \
    sent.h exec.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

port.o : hosts/crypt.c main.h backend.h my-rusage.h driver.h typedefs.h \
    port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

ptrtable.o : simulate.h mempools.h ptrtable.h driver.h svalue.h strfuns.h \
    sent.h typedefs.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

random.o : random.h driver.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

regexp.o : main.h xalloc.h simulate.h comm.h regexp.h driver.h typedefs.h \
    svalue.h strfuns.h sent.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

rxcache.o : ../mudlib/sys/debug_info.h xalloc.h svalue.h strfuns.h \
    stralloc.h smalloc.h regexp.h hash.h gcollect.h rxcache.h driver.h \
    typedefs.h main.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

simul_efun.o : xalloc.h swap.h svalue.h stralloc.h stdstrings.h simulate.h \
    ptrtable.h object.h lex.h interpret.h gcollect.h exec.h array.h \
    simul_efun.h my-alloca.h typedefs.h driver.h main.h smalloc.h strfuns.h \
    sent.h port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

simulate.o : ../mudlib/sys/rtlimits.h ../mudlib/sys/files.h \
    ../mudlib/sys/driver_hook.h ../mudlib/sys/debug_info.h xalloc.h \
    wiz_list.h svalue.h swap.h strfuns.h stralloc.h stdstrings.h \
    simul_efun.h sent.h rxcache.h prolang.h otable.h object.h mapping.h \
    main.h lex.h interpret.h heartbeat.h gcollect.h filestat.h exec.h ed.h \
    dumpstat.h comm.h closure.h call_out.h backend.h array.h actions.h \
    simulate.h hosts/amiga/nsignal.h my-alloca.h typedefs.h driver.h \
    smalloc.h ptrtable.h regexp.h port.h config.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

sprintf.o : xalloc.h swap.h stralloc.h stdstrings.h simul_efun.h simulate.h \
    sent.h ptrtable.h object.h mapping.h main.h interpret.h exec.h comm.h \
    closure.h array.h actions.h sprintf.h typedefs.h driver.h smalloc.h \
    strfuns.h svalue.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

stdstrings.o : stralloc.h stdstrings.h driver.h main.h smalloc.h strfuns.h \
    port.h config.h typedefs.h svalue.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

stralloc.o : ../mudlib/sys/debug_info.h xalloc.h svalue.h strfuns.h \
    stdstrings.h smalloc.h simulate.h main.h hash.h gcollect.h stralloc.h \
    driver.h typedefs.h sent.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

strfuns.o : xalloc.h smalloc.h svalue.h simulate.h comm.h strfuns.h \
    typedefs.h driver.h sent.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

swap.o : hosts/amiga/socket.h ../mudlib/sys/debug_info.h xalloc.h \
    wiz_list.h svalue.h strfuns.h stralloc.h simul_efun.h simulate.h \
    random.h prolang.h otable.h object.h mapping.h main.h interpret.h \
    gcollect.h exec.h comm.h backend.h array.h swap.h typedefs.h driver.h \
    ./hosts/amiga/socket_sim.h ./hosts/amiga/socket_tcp.h smalloc.h \
    ptrtable.h sent.h port.h config.h ./hosts/amiga/socket_sim_protos.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

wiz_list.o : xalloc.h svalue.h stralloc.h stdstrings.h simulate.h object.h \
    main.h interpret.h gcollect.h backend.h array.h ../mudlib/sys/wizlist.h \
    wiz_list.h typedefs.h driver.h smalloc.h strfuns.h sent.h port.h \
    config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

xalloc.o : smalloc.c simulate.h interpret.h xalloc.h driver.h \
    ../mudlib/sys/debug_info.h object.h exec.h svalue.h main.h gcollect.h \
    backend.h smalloc.h typedefs.h strfuns.h sent.h port.h config.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

# --- DO NOT MODIFY THIS LINE -- AUTO-DEPENDS PRECEDE ---
# --- DO NOT MODIFY THIS LINE -- SELECTED AUTO-DEPENDS FOLLOW ---
actions.o :

array.o :

backend.o : stdstrings.h

bitstrings.o :

call_out.o :

closure.o : stdstrings.h lang.h

comm.o : stdstrings.h

dumpstat.o :

ed.o : stdstrings.h

efuns.o : stdstrings.h

files.o :

gcollect.o : stdstrings.h

heartbeat.o :

interpret.o : stdstrings.h

lang.o : stdstrings.h

lex.o : efun_defs.c stdstrings.h lang.h

main.o : stdstrings.h

mapping.o :

object.o : stdstrings.h

otable.o :

parse.o : stdstrings.h

parse_old.o : stdstrings.h

simul_efun.o : stdstrings.h

simulate.o : stdstrings.h

sprintf.o : stdstrings.h

stdstrings.o : stdstrings.h

stralloc.o : stdstrings.h

swap.o :

wiz_list.o : stdstrings.h

xalloc.o :

# --- DO NOT MODIFY THIS LINE -- SELECTED AUTO-DEPENDS PRECEDE ---
