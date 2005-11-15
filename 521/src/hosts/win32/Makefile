# Generated automatically from Makefile.in by configure.
# TODO: Add some of the symbols in here to configure
# These lines are needed on some machines.
MAKE=make
SHELL=/bin/sh
INSTALL=/usr/bin/install -c
mkinstalldirs=$(SHELL) ./mkinstalldirs
#
CC=gcc
#
YACC = byacc
YACCTAB = y.tab.
#
RM = rm -f
MV = mv
CP = cp
#
# some standard stuff...
prefix= \\mud
exec_prefix= ${prefix}
#
#
# Set MUD_LIB to the directory which contains the mud data. Was formerly
# defined in config.h !
MUD_LIB = \\mud\\mudlib
#
# Set BINDIR to the directory where you want to install the executables.
BINDIR = \\mud\\mudbin
#
# Set ERQ_DIR to directory which contains the stuff which ERQ can
# execute (hopefully) savely. Was formerly defined in config.h!
ERQ_DIR= \\mud\\mudbin\\erqdir
#
# Typical profiling, warning and optimizing options.
#   -p -DMARK: together they enable the profiling of VM instructions.
#   Additional profiling can be activated in config.h .
#PROFIL=-p -DMARK
#PROFIL=-pg
PROFIL=
#Enable warnings from the compiler, if wanted.
WARN= # no warning options - will work with all compilers :-)
#WARN= -Wall -Wshadow -Dlint
#WARN= -Wall -Wshadow -Wparentheses # gcc settings
#
# Optimization and source level debugging options.
# adding a -fomit-frame-pointer on the NeXT (gcc version 1.93 (68k, MIT syntax))
# will corrupt the driver.
#OPTIMIZE = -O4 -fomit-frame-pointer  -g -fstrength-reduce -fno-force-mem -fno-force-addr -fno-inline-functions # high optimization
OPTIMIZE= -O2 -g -fstrength-reduce -fno-force-mem -fno-force-addr -fno-inline-functions # medium optimization
#OPTIMIZE= -O -g # low optimization
#OPTIMIZE= -g # no optimization; for frequent recompilations.
#
# Debugging options:
#   Debugging options should be set in config.h to guarantee a clean
#   recompile.
#DEBUG = -DDEBUG -DCHECK_STRINGS
DEBUG=
#
MPATH=-DMUD_LIB='"$(MUD_LIB)"' -DBINDIR='"$(BINDIR)"' -DERQ_DIR='"$(ERQ_DIR)"'
#
CFLAGS=  -Dlint $(OPTIMIZE) $(DEBUG) $(WARN) $(MPATH) $(PROFIL)
#
LIBS=-lm -lcrypt -luser32 -lwinmm -lkernel32 -lwsock32
#
LDFLAGS=
#
#Note that, if you use smalloc with SBRK_OK(which is the default), and
#don't link statically, it could happen that the system malloc is linked
#along with the one from smalloc; this combination is bound to cause crashes.
MFLAGS = "BINDIR=$(BINDIR)" "MUD_LIB=$(MUD_LIB)"
#
SRC = access_check.c actions.c array.c backend.c call_out.c closure.c comm.c \
      dumpstat.c ed.c efuns.c gcollect.c hash.c heartbeat.c interpret.c \
      parser.c lex.c main.c mapping.c mempools.c object.c otable.c parse.c \
      parse_old.c pkg-mysql.c port.c ptrtable.c md5.c \
      random.c regexp.c rxcache.c simulate.c simul_efun.c stdstrings.c \
      stralloc.c strfuns.c sprintf.c swap.c wiz_list.c xalloc.c
OBJ = access_check.o actions.o array.o backend.o call_out.o closure.o comm.o \
      dumpstat.o ed.o efuns.o gcollect.o hash.o heartbeat.o interpret.o \
      parser.o lex.o main.o mapping.o mempools.o object.o otable.o parse.o \
      parse_old.o pkg-mysql.o port.o ptrtable.o md5.o \
      random.o regexp.o rxcache.o simulate.o simul_efun.o stdstrings.o \
      stralloc.o strfuns.o sprintf.o swap.o wiz_list.o xalloc.o 

ldmud: $(OBJ)
	$(CC) $(OPTIMIZE) $(LDFLAGS) $(OBJ) -o $@ $(LIBS)

install: ldmud
	$(mkinstalldirs) $(BINDIR)
	$(INSTALL) -c $? $(BINDIR)/parse

install-utils:
	(cd util; $(MAKE) $(MFLAGS) install)

utils:
	(cd util; $(MAKE) $(MFLAGS))

parse: ldmud
	-$(MV) parse parse.old
	$(CP) ldmud parse

lint: *.c
	lint *.c

clean:
	$(RM) $(YACCTAB)h $(YACCTAB)c make_func.c *.o make_func
	$(RM) stdstrings.c stdstrings.h
	$(RM) efun_defs.c instrs.h lang.y lang.h lang.c y.output tags TAGS
	$(RM) parse core mudlib/core mudlib/debug.log lpmud.log ldmud
	(cd util ; echo "Cleaning in util." ; $(MAKE) clean)

distclean: clean
	$(RM) ldmud config.status machine.h Makefile config.cache config.log
	$(RM) config.h config.status config.status.old
	$(RM) util/Makefile util/erq/Makefile util/xerq/Makefile

tags: $(SRC)
	ctags $(SRC)

TAGS: $(SRC)
	etags $(SRC)

make_func.c: make_func.y
	$(YACC) make_func.y
	$(MV) $(YACCTAB)c make_func.c
	

make_func.o : make_func_main.c driver.h config.h machine.h port.h
	  $(CC) $(CFLAGS) -DYACC='"$(YACC)"' -c make_func.c -o make_func.o

make_func: make_func.o hash.o exec.h
	$(CC) $(OPTIMIZE) $(LDFLAGS) make_func.o hash.o -o make_func

lang.y: make_func prolang.y config.h
	$(RM) lang.y
	./make_func lang

efun_defs.c instrs.h: func_spec make_func config.h
	$(RM) instrs.h
	$(RM) efun_defs.c
	./make_func instrs

stdstrings.c stdstrings.h: string_spec make_func config.h
	$(RM) stdstrings.c
	$(RM) stdstrings.h
	./make_func strings

lang.c lang.h: lang.y
	$(YACC) -d -v lang.y
	$(MV) $(YACCTAB)c lang.c
	
	$(MV) $(YACCTAB)h lang.h

#--------------------------------------------------------
# Dependencies, manual and automatic.

# --- DO NOT MODIFY THIS LINE -- AUTO-DEPENDS FOLLOW ---
access_check.o : xalloc.h filestat.h comm.h access_check.h driver.h \
    svalue.h simulate.h typedefs.h config.h port.h strfuns.h sent.h \
    instrs.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h exec.h ./hosts/amiga/ixfile.h

actions.o : ../mudlib/sys/driver_hook.h ../mudlib/sys/commands.h xalloc.h \
    wiz_list.h stralloc.h smalloc.h simulate.h sent.h object.h mapping.h \
    interpret.h exec.h efuns.h dumpstat.h comm.h closure.h backend.h \
    array.h actions.h typedefs.h driver.h svalue.h main.h strfuns.h \
    instrs.h port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

array.o : smalloc.h xalloc.h wiz_list.h swap.h stralloc.h svalue.h \
    simulate.h rxcache.h regexp.h object.h mempools.h mapping.h main.h \
    interpret.h instrs.h backend.h array.h my-alloca.h typedefs.h driver.h \
    strfuns.h sent.h exec.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

backend.o : ../mudlib/sys/debug_message.h ../mudlib/sys/driver_hook.h \
    xalloc.h wiz_list.h swap.h stralloc.h stdstrings.h smalloc.h simulate.h \
    rxcache.h regexp.h random.h otable.h object.h my-alloca.h mapping.h \
    main.h lex.h interpret.h heartbeat.h gcollect.h filestat.h exec.h ed.h \
    comm.h closure.h call_out.h array.h actions.h backend.h \
    hosts/amiga/nsignal.h typedefs.h driver.h svalue.h strfuns.h sent.h \
    instrs.h port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

call_out.o : ../mudlib/sys/debug_info.h xalloc.h wiz_list.h swap.h svalue.h \
    strfuns.h stralloc.h simulate.h object.h main.h interpret.h gcollect.h \
    exec.h comm.h closure.h backend.h array.h actions.h call_out.h \
    typedefs.h driver.h smalloc.h sent.h instrs.h port.h config.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

closure.o : xalloc.h switch.h svalue.h stralloc.h stdstrings.h simul_efun.h \
    simulate.h prolang.h object.h main.h lex.h lang.h interpret.h instrs.h \
    exec.h backend.h array.h closure.h my-alloca.h typedefs.h driver.h \
    smalloc.h strfuns.h ptrtable.h sent.h port.h config.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

comm.o : util/erq/erq.h ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h \
    swap.h stralloc.h stdstrings.h smalloc.h simulate.h sent.h object.h \
    main.h interpret.h gcollect.h filestat.h exec.h ed.h closure.h \
    backend.h array.h actions.h access_check.h comm.h hosts/amiga/nsignal.h \
    telnet.h my-alloca.h typedefs.h driver.h svalue.h strfuns.h instrs.h \
    config.h port.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

dumpstat.o : svalue.h smalloc.h simulate.h ptrtable.h object.h mapping.h \
    instrs.h filestat.h exec.h closure.h array.h dumpstat.h typedefs.h \
    driver.h strfuns.h sent.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

ed.o : xalloc.h svalue.h stralloc.h stdstrings.h simulate.h rxcache.h \
    regexp.h object.h main.h interpret.h gcollect.h filestat.h comm.h \
    closure.h actions.h ed.h typedefs.h driver.h smalloc.h strfuns.h sent.h \
    instrs.h exec.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

efuns.o : ../mudlib/sys/strings.h ../mudlib/sys/objectinfo.h \
    ../mudlib/sys/debug_info.h xalloc.h wiz_list.h svalue.h swap.h \
    strfuns.h stralloc.h stdstrings.h smalloc.h simulate.h rxcache.h \
    ptrtable.h otable.h object.h mapping.h main.h interpret.h instrs.h \
    heartbeat.h dumpstat.h comm.h closure.h call_out.h backend.h array.h \
    actions.h efuns.h my-alloca.h typedefs.h driver.h sent.h regexp.h \
    exec.h port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

gcollect.o : ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h swap.h \
    stralloc.h stdstrings.h smalloc.h simul_efun.h simulate.h sent.h \
    rxcache.h prolang.h parse.h otable.h object.h mapping.h main.h lex.h \
    interpret.h instrs.h heartbeat.h filestat.h exec.h ed.h comm.h \
    closure.h call_out.h backend.h array.h actions.h gcollect.h typedefs.h \
    driver.h svalue.h strfuns.h ptrtable.h regexp.h port.h config.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

hash.o : hash.h

heartbeat.o : ../mudlib/sys/debug_info.h xalloc.h wiz_list.h svalue.h \
    strfuns.h simulate.h sent.h object.h interpret.h gcollect.h comm.h \
    backend.h array.h actions.h heartbeat.h typedefs.h driver.h instrs.h \
    main.h port.h config.h exec.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

interpret.o : ../mudlib/sys/trace.h ../mudlib/sys/debug_info.h \
    ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h switch.h swap.h \
    svalue.h sprintf.h smalloc.h stralloc.h stdstrings.h simul_efun.h \
    simulate.h sent.h random.h ptrtable.h prolang.h parse.h otable.h \
    object.h mapping.h main.h lex.h instrs.h heartbeat.h gcollect.h \
    filestat.h exec.h efuns.h ed.h comm.h closure.h call_out.h backend.h \
    array.h actions.h interpret.h my-rusage.h my-alloca.h typedefs.h \
    driver.h strfuns.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

lang.o : ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h switch.h swap.h \
    svalue.h stralloc.h stdstrings.h simul_efun.h simulate.h object.h \
    mapping.h main.h lex.h instrs.h interpret.h gcollect.h exec.h closure.h \
    backend.h array.h prolang.h my-alloca.h typedefs.h driver.h smalloc.h \
    strfuns.h ptrtable.h sent.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

lex.o : efun_defs.c hosts/amiga/socket.h ../mudlib/sys/driver_hook.h \
    xalloc.h strfuns.h stralloc.h stdstrings.h simul_efun.h simulate.h \
    prolang.h patchlevel.h object.h mempools.h main.h lang.h interpret.h \
    instrs.h hash.h gcollect.h filestat.h exec.h comm.h closure.h backend.h \
    array.h lex.h my-alloca.h typedefs.h driver.h \
    ./hosts/amiga/socket_sim.h ./hosts/amiga/socket_tcp.h smalloc.h \
    ptrtable.h svalue.h sent.h port.h config.h \
    ./hosts/amiga/socket_sim_protos.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

main.o : xalloc.h wiz_list.h swap.h svalue.h stralloc.h stdstrings.h \
    simul_efun.h simulate.h rxcache.h random.h patchlevel.h otable.h \
    object.h mapping.h lex.h interpret.h gcollect.h filestat.h comm.h \
    array.h backend.h main.h hosts/amiga/socket.h my-alloca.h typedefs.h \
    driver.h smalloc.h strfuns.h ptrtable.h exec.h sent.h instrs.h regexp.h \
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
    instrs.h exec.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

mempools.o : xalloc.h gcollect.h mempools.h driver.h typedefs.h port.h \
    config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

object.o : ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h svalue.h swap.h \
    strfuns.h stralloc.h stdstrings.h simulate.h smalloc.h sent.h random.h \
    ptrtable.h prolang.h otable.h mapping.h main.h interpret.h filestat.h \
    exec.h comm.h closure.h backend.h array.h actions.h object.h \
    my-alloca.h typedefs.h driver.h instrs.h port.h config.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

otable.o : ../mudlib/sys/debug_info.h xalloc.h svalue.h simulate.h \
    strfuns.h object.h hash.h gcollect.h backend.h otable.h typedefs.h \
    driver.h sent.h instrs.h main.h port.h config.h exec.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

parse.o : xalloc.h wiz_list.h svalue.h stralloc.h stdstrings.h simulate.h \
    object.h main.h interpret.h instrs.h gcollect.h closure.h array.h \
    actions.h parse.h typedefs.h driver.h smalloc.h strfuns.h sent.h exec.h \
    port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

parse_old.o : xalloc.h wiz_list.h svalue.h stralloc.h stdstrings.h \
    simulate.h random.h object.h main.h gcollect.h interpret.h closure.h \
    array.h actions.h parse.h typedefs.h driver.h smalloc.h strfuns.h \
    sent.h instrs.h exec.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

pkg-mysql.o : xalloc.h svalue.h interpret.h array.h pkg-mysql.h my-alloca.h \
    typedefs.h driver.h instrs.h port.h config.h exec.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

port.o : hosts/crypt.c main.h backend.h my-rusage.h driver.h typedefs.h \
    port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

ptrtable.o : simulate.h mempools.h ptrtable.h driver.h svalue.h strfuns.h \
    sent.h instrs.h typedefs.h port.h config.h exec.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

random.o : random.h driver.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

regexp.o : main.h xalloc.h simulate.h comm.h regexp.h driver.h typedefs.h \
    svalue.h strfuns.h sent.h instrs.h port.h config.h exec.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

rxcache.o : ../mudlib/sys/debug_info.h xalloc.h svalue.h strfuns.h \
    stralloc.h smalloc.h regexp.h hash.h gcollect.h rxcache.h driver.h \
    typedefs.h main.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

simul_efun.o : xalloc.h swap.h svalue.h stralloc.h stdstrings.h simulate.h \
    ptrtable.h object.h lex.h interpret.h instrs.h gcollect.h exec.h \
    array.h simul_efun.h my-alloca.h typedefs.h driver.h main.h smalloc.h \
    strfuns.h sent.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

simulate.o : ../mudlib/sys/rtlimits.h ../mudlib/sys/files.h \
    ../mudlib/sys/driver_hook.h ../mudlib/sys/debug_info.h xalloc.h \
    wiz_list.h svalue.h swap.h strfuns.h stralloc.h stdstrings.h \
    simul_efun.h sent.h rxcache.h prolang.h otable.h object.h mapping.h \
    main.h lex.h instrs.h interpret.h heartbeat.h gcollect.h filestat.h \
    exec.h ed.h dumpstat.h comm.h closure.h call_out.h backend.h array.h \
    actions.h simulate.h hosts/amiga/nsignal.h my-alloca.h typedefs.h \
    driver.h smalloc.h ptrtable.h regexp.h port.h config.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

sprintf.o : xalloc.h swap.h stralloc.h stdstrings.h simul_efun.h simulate.h \
    sent.h ptrtable.h object.h mapping.h main.h instrs.h interpret.h exec.h \
    closure.h array.h sprintf.h typedefs.h driver.h smalloc.h strfuns.h \
    svalue.h port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

stdstrings.o : stralloc.h stdstrings.h driver.h main.h smalloc.h strfuns.h \
    port.h config.h typedefs.h svalue.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

stralloc.o : ../mudlib/sys/debug_info.h xalloc.h svalue.h strfuns.h \
    stdstrings.h smalloc.h simulate.h main.h hash.h gcollect.h stralloc.h \
    driver.h typedefs.h sent.h instrs.h port.h config.h exec.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

strfuns.o : xalloc.h smalloc.h svalue.h simulate.h comm.h strfuns.h \
    typedefs.h driver.h sent.h instrs.h port.h config.h exec.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

swap.o : hosts/amiga/socket.h ../mudlib/sys/debug_info.h xalloc.h \
    wiz_list.h svalue.h strfuns.h stralloc.h simul_efun.h simulate.h \
    random.h prolang.h otable.h object.h mapping.h main.h interpret.h \
    gcollect.h exec.h comm.h backend.h array.h swap.h typedefs.h driver.h \
    ./hosts/amiga/socket_sim.h ./hosts/amiga/socket_tcp.h smalloc.h \
    ptrtable.h sent.h instrs.h port.h config.h \
    ./hosts/amiga/socket_sim_protos.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

wiz_list.o : xalloc.h svalue.h stralloc.h stdstrings.h object.h mapping.h \
    main.h interpret.h gcollect.h backend.h array.h ../mudlib/sys/wizlist.h \
    wiz_list.h typedefs.h driver.h smalloc.h strfuns.h instrs.h sent.h \
    port.h config.h exec.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

xalloc.o : smalloc.c simulate.h interpret.h xalloc.h driver.h \
    ../mudlib/sys/debug_info.h object.h exec.h svalue.h main.h gcollect.h \
    backend.h smalloc.h typedefs.h strfuns.h sent.h instrs.h port.h \
    config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

# --- DO NOT MODIFY THIS LINE -- AUTO-DEPENDS PRECEDE ---
# --- DO NOT MODIFY THIS LINE -- SELECTED AUTO-DEPENDS FOLLOW ---
access_check.o : instrs.h

actions.o : instrs.h

array.o : instrs.h

backend.o : stdstrings.h instrs.h

call_out.o : instrs.h

closure.o : stdstrings.h lang.h instrs.h

comm.o : stdstrings.h instrs.h

dumpstat.o : instrs.h

ed.o : stdstrings.h instrs.h

efuns.o : stdstrings.h instrs.h

gcollect.o : stdstrings.h instrs.h

heartbeat.o : instrs.h

interpret.o : stdstrings.h instrs.h

lang.o : stdstrings.h instrs.h

lex.o : efun_defs.c stdstrings.h lang.h instrs.h

main.o : stdstrings.h instrs.h

mapping.o : instrs.h

object.o : stdstrings.h instrs.h

otable.o : instrs.h

parse.o : stdstrings.h instrs.h

parse_old.o : stdstrings.h instrs.h

pkg-mysql.o : instrs.h

ptrtable.o : instrs.h

regexp.o : instrs.h

simul_efun.o : stdstrings.h instrs.h

simulate.o : stdstrings.h instrs.h

sprintf.o : stdstrings.h instrs.h

stdstrings.o : stdstrings.h

stralloc.o : stdstrings.h instrs.h

strfuns.o : instrs.h

swap.o : instrs.h

wiz_list.o : stdstrings.h instrs.h

xalloc.o : instrs.h

# --- DO NOT MODIFY THIS LINE -- SELECTED AUTO-DEPENDS PRECEDE ---

