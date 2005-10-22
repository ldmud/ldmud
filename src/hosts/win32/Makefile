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
#   -DOPCPROF: enable usage statistics of VM instructions
#   -DVERBOSE_OPCPROF: with OPCPROF, the dump of the statistics includes
#              the names of the instructions
#   -p -DMARK: together they enable the profiling of VM instructions.
#PROFIL= -DOPCPROF -DVERBOSE_OPCPROF
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
OPTIMIZE = -O4 -fomit-frame-pointer  -g -fstrength-reduce -fno-force-mem -fno-force-addr -fno-inline-functions # high optimization
#OPTIMIZE= -O2 -g -fstrength-reduce -fno-force-mem -fno-force-addr -fno-inline-functions # medium optimization
#OPTIMIZE= -O -g # low optimization
#OPTIMIZE= -g # no optimization; for frequent recompilations.
#
# Debugging options:
#   -DDEBUG: Enable run time debugging. It will use more time and space.
#            Nevertheless you are strongly encouraged to keep it defined.
#   -DYYDEBUG[=1]: Debug output for the LPC compiler.
#   -DTRACE_CODE: Enable the LPC code tracing.
#   -DNO_INLINES: Disable inlining
#   -DCHECK_STRINGS: Enable the shared string checking.
#   -DKEEP_STRINGS: Shared strings are never really freed.
#   -DDEBUG_TELNET: Activates debug prints in the telnet machine
#   -DDEBUG_SMALLOC_ALLOCS: Activates allocation debug prints in smalloc.
#DEBUG = -DDEBUG -DTRACE_CODE
DEBUG = -DDEBUG -DTRACE_CODE -DCHECK_STRINGS # -DKEEP_STRINGS -DDEBUG_TELNET
#DEBUG=
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
      parse_old.c pkg-alists.c pkg-mysql.c port.c ptrtable.c md5.c \
      random.c regexp.c rxcache.c simulate.c simul_efun.c stdstrings.c \
      stralloc.c strfuns.c sprintf.c swap.c wiz_list.c xalloc.c \
      pcre-get.c pcre-maketables.c pcre-pcre.c pcre-study.c
OBJ = access_check.o actions.o array.o backend.o call_out.o closure.o comm.o \
      dumpstat.o ed.o efuns.o gcollect.o hash.o heartbeat.o interpret.o \
      parser.o lex.o main.o mapping.o mempools.o object.o otable.o parse.o \
      parse_old.o pkg-alists.o pkg-mysql.o port.o ptrtable.o md5.o\
      random.o regexp.o rxcache.o simulate.o simul_efun.o stdstrings.o \
      stralloc.o strfuns.o sprintf.o swap.o wiz_list.o xalloc.o \
      pcre-get.o pcre-maketables.o pcre-pcre.o pcre-study.o

driver: $(OBJ)
	$(CC) $(OPTIMIZE) $(LDFLAGS) $(OBJ) -o $@ $(LIBS)

install: driver
	$(mkinstalldirs) $(BINDIR)
	$(INSTALL) -c $? $(BINDIR)/parse

install-utils:
	(cd util; $(MAKE) $(MFLAGS) install)

utils:
	(cd util; $(MAKE) $(MFLAGS))

parse: driver
	-$(MV) parse parse.old
	$(CP) driver parse

lint: *.c
	lint *.c

clean:
	$(RM) $(YACCTAB)h $(YACCTAB)c make_func.c *.o mkfunc.exe
	$(RM) dftables.exe pcre/chartables.c
	$(RM) stdstrings.c stdstrings.h
	$(RM) efun_defs.c instrs.h lang.y lang.h lang.c y.output tags TAGS
	$(RM) parse core mudlib/core mudlib/debug.log lpmud.log driver
	(cd util ; echo "Cleaning in util." ; $(MAKE) clean)

distclean: clean
	$(RM) driver config.status machine.h Makefile config.cache config.log
	$(RM) config.h config.status config.status.old
	$(RM) util/Makefile util/erq/Makefile util/xerq/Makefile

tags: $(SRC)
	ctags $(SRC)

TAGS: $(SRC)
	etags $(SRC)

make_func.c: make_func.y
	$(YACC) make_func.y
	$(MV) $(YACCTAB)c make_func.c
	

mkfunc.o : mkfunc.c make_func.c driver.h config.h machine.h port.h
	  $(CC) $(CFLAGS) -DYACC='"$(YACC)"' -c mkfunc.c

mkfunc: mkfunc.o hash.o exec.h
	$(CC) $(OPTIMIZE) $(LDFLAGS) mkfunc.o hash.o -o mkfunc

dftables.o : pcre/dftables.c pcre/maketables.c pcre/config.h
	$(CC) $(CFLAGS) -c dftables.c -o dftables.o

dftables : dftables.o
	$(CC) $(OPTIMIZE) (LDFLAGS) dftables.o -o dftables

lang.y: mkfunc prolang.y config.h
	$(RM) lang.y
	./mkfunc lang

efun_defs.c instrs.h: func_spec mkfunc config.h
	$(RM) instrs.h
	$(RM) efun_defs.c
	./mkfunc instrs

stdstrings.c stdstrings.h: string_spec mkfunc config.h
	$(RM) stdstrings.c
	$(RM) stdstrings.h
	./mkfunc strings

lang.c lang.h: lang.y
	$(YACC) -d -v lang.y
	$(MV) $(YACCTAB)c lang.c
	
	$(MV) $(YACCTAB)h lang.h

pcre/chartables.c : dftables
	dftables > pcre/chartables.c

#--------------------------------------------------------
# Dependencies, manual and automatic.

# --- DO NOT MODIFY THIS LINE -- AUTO-DEPENDS FOLLOW ---
access_check.o : xalloc.h filestat.h comm.h access_check.h driver.h \
    svalue.h simulate.h typedefs.h config.h port.h strfuns.h sent.h exec.h \
    hosts/unix.h hosts/be/be.h machine.h

actions.o : ../mudlib/sys/driver_hook.h ../mudlib/sys/commands.h xalloc.h \
    wiz_list.h svalue.h simulate.h sent.h stdstrings.h object.h mstrings.h \
    mapping.h interpret.h exec.h efuns.h dumpstat.h comm.h closure.h \
    backend.h array.h actions.h typedefs.h driver.h strfuns.h instrs.h \
    main.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

array.o : xalloc.h wiz_list.h swap.h svalue.h simulate.h stdstrings.h \
    rxcache.h regexp.h object.h mstrings.h mempools.h mapping.h main.h \
    interpret.h closure.h backend.h array.h my-alloca.h typedefs.h driver.h \
    strfuns.h sent.h exec.h instrs.h port.h config.h hosts/unix.h \
    hosts/be/be.h machine.h

backend.o : ../mudlib/sys/debug_message.h ../mudlib/sys/driver_hook.h \
    xalloc.h wiz_list.h swap.h svalue.h stdstrings.h simulate.h rxcache.h \
    regexp.h random.h otable.h object.h mstrings.h mapping.h main.h lex.h \
    interpret.h heartbeat.h gcollect.h filestat.h exec.h ed.h comm.h \
    closure.h call_out.h array.h actions.h backend.h my-alloca.h typedefs.h \
    driver.h strfuns.h sent.h instrs.h port.h config.h hosts/unix.h \
    hosts/be/be.h machine.h

bitstrings.o : xalloc.h svalue.h simulate.h mstrings.h main.h lex.h \
    interpret.h bitstrings.h typedefs.h driver.h strfuns.h sent.h exec.h \
    instrs.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

call_out.o : ../mudlib/sys/debug_info.h xalloc.h wiz_list.h swap.h svalue.h \
    strfuns.h stdstrings.h simulate.h object.h mstrings.h main.h \
    interpret.h gcollect.h exec.h comm.h closure.h backend.h array.h \
    actions.h call_out.h typedefs.h driver.h sent.h instrs.h port.h \
    config.h hosts/unix.h hosts/be/be.h machine.h

closure.o : xalloc.h switch.h swap.h svalue.h stdstrings.h simul_efun.h \
    simulate.h prolang.h object.h mstrings.h main.h lex.h lang.h \
    interpret.h instrs.h exec.h backend.h array.h closure.h my-alloca.h \
    typedefs.h driver.h ptrtable.h strfuns.h sent.h port.h config.h \
    hosts/unix.h hosts/be/be.h machine.h

comm.o : util/erq/erq.h ../mudlib/sys/input_to.h \
    ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h swap.h svalue.h \
    stdstrings.h simulate.h sent.h object.h mstrings.h main.h interpret.h \
    gcollect.h filestat.h exec.h ed.h closure.h backend.h array.h actions.h \
    access_check.h comm.h telnet.h my-alloca.h typedefs.h driver.h \
    strfuns.h instrs.h config.h port.h hosts/unix.h hosts/be/be.h machine.h

dumpstat.o : svalue.h smalloc.h stdstrings.h simulate.h ptrtable.h object.h \
    mstrings.h mapping.h filestat.h exec.h closure.h array.h dumpstat.h \
    typedefs.h driver.h strfuns.h sent.h instrs.h port.h config.h \
    hosts/unix.h hosts/be/be.h machine.h

ed.o : xalloc.h svalue.h stdstrings.h simulate.h rxcache.h regexp.h \
    object.h mstrings.h main.h interpret.h gcollect.h filestat.h comm.h \
    closure.h actions.h ed.h typedefs.h driver.h strfuns.h sent.h exec.h \
    instrs.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

efuns.o : ../mudlib/sys/time.h ../mudlib/sys/strings.h \
    ../mudlib/sys/objectinfo.h ../mudlib/sys/debug_info.h xalloc.h \
    wiz_list.h svalue.h swap.h strfuns.h smalloc.h simulate.h stdstrings.h \
    rxcache.h random.h ptrtable.h otable.h object.h mstrings.h md5.h \
    mapping.h main.h interpret.h instrs.h heartbeat.h dumpstat.h comm.h \
    closure.h call_out.h backend.h array.h actions.h efuns.h my-rusage.h \
    my-alloca.h typedefs.h driver.h sent.h exec.h regexp.h port.h config.h \
    hosts/unix.h hosts/be/be.h machine.h

files.o : ../mudlib/sys/files.h xalloc.h svalue.h stdstrings.h simulate.h \
    mstrings.h main.h lex.h interpret.h filestat.h comm.h array.h files.h \
    my-alloca.h typedefs.h driver.h strfuns.h sent.h exec.h instrs.h port.h \
    config.h hosts/unix.h hosts/be/be.h machine.h

gcollect.o : ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h swap.h \
    stdstrings.h smalloc.h simul_efun.h simulate.h sent.h rxcache.h \
    prolang.h parse.h otable.h object.h mstrings.h mapping.h main.h lex.h \
    interpret.h instrs.h heartbeat.h filestat.h exec.h ed.h comm.h \
    closure.h call_out.h backend.h array.h actions.h gcollect.h typedefs.h \
    driver.h svalue.h ptrtable.h strfuns.h regexp.h port.h config.h \
    hosts/unix.h hosts/be/be.h machine.h

hash.o : hash.h

heartbeat.o : ../mudlib/sys/debug_info.h xalloc.h wiz_list.h svalue.h \
    strfuns.h simulate.h sent.h object.h mstrings.h interpret.h gcollect.h \
    comm.h backend.h array.h actions.h heartbeat.h typedefs.h driver.h \
    exec.h instrs.h main.h port.h config.h hosts/unix.h hosts/be/be.h \
    machine.h

interpret.o : ../mudlib/sys/trace.h ../mudlib/sys/debug_info.h \
    ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h switch.h swap.h \
    svalue.h sprintf.h smalloc.h stdstrings.h simul_efun.h simulate.h \
    sent.h ptrtable.h prolang.h parse.h otable.h object.h mstrings.h \
    mapping.h main.h lex.h instrs.h heartbeat.h gcollect.h filestat.h \
    exec.h efuns.h ed.h comm.h closure.h call_out.h backend.h array.h \
    actions.h interpret.h my-alloca.h typedefs.h driver.h strfuns.h port.h \
    config.h hosts/unix.h hosts/be/be.h machine.h

lex.o : efun_defs.c ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h \
    svalue.h strfuns.h stdstrings.h simul_efun.h simulate.h prolang.h \
    patchlevel.h object.h mstrings.h mempools.h main.h lang.h interpret.h \
    instrs.h hash.h gcollect.h filestat.h exec.h comm.h closure.h backend.h \
    array.h lex.h my-alloca.h typedefs.h driver.h ptrtable.h sent.h port.h \
    config.h hosts/unix.h hosts/be/be.h machine.h

main.o : xalloc.h wiz_list.h swap.h svalue.h stdstrings.h simul_efun.h \
    simulate.h rxcache.h random.h pcre/pcre.h patchlevel.h otable.h \
    object.h mstrings.h mapping.h lex.h interpret.h gcollect.h filestat.h \
    comm.h array.h backend.h main.h my-alloca.h typedefs.h driver.h \
    ptrtable.h exec.h strfuns.h sent.h regexp.h instrs.h port.h config.h \
    hosts/unix.h hosts/be/be.h machine.h

mapping.o : xalloc.h wiz_list.h svalue.h smalloc.h simulate.h object.h \
    mstrings.h main.h interpret.h gcollect.h closure.h backend.h array.h \
    mapping.h my-alloca.h typedefs.h driver.h strfuns.h sent.h exec.h \
    instrs.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

md5.o : strfuns.h md5.h machine.h typedefs.h driver.h port.h config.h \
    hosts/unix.h hosts/be/be.h

mempools.o : xalloc.h gcollect.h mempools.h driver.h typedefs.h port.h \
    config.h hosts/unix.h hosts/be/be.h machine.h

mkfunc.o : make_func.c ../mudlib/sys/driver_hook.h hash.h exec.h \
    my-alloca.h driver.h typedefs.h port.h config.h hosts/unix.h \
    hosts/be/be.h machine.h

mstrings.o : ../mudlib/sys/debug_info.h xalloc.h svalue.h strfuns.h \
    stdstrings.h simulate.h main.h hash.h gcollect.h mstrings.h driver.h \
    typedefs.h sent.h exec.h port.h config.h hosts/unix.h hosts/be/be.h \
    machine.h

object.o : ../mudlib/sys/inherit_list.h ../mudlib/sys/include_list.h \
    ../mudlib/sys/functionlist.h ../mudlib/sys/driver_hook.h xalloc.h \
    wiz_list.h svalue.h swap.h strfuns.h stdstrings.h simul_efun.h \
    simulate.h sent.h random.h ptrtable.h prolang.h otable.h mstrings.h \
    mempools.h mapping.h main.h instrs.h interpret.h filestat.h exec.h \
    comm.h closure.h backend.h array.h actions.h object.h my-alloca.h \
    typedefs.h driver.h ../mudlib/sys/lpctypes.h port.h config.h \
    hosts/unix.h hosts/be/be.h machine.h

otable.o : ../mudlib/sys/debug_info.h xalloc.h svalue.h simulate.h \
    strfuns.h object.h mstrings.h hash.h gcollect.h backend.h otable.h \
    typedefs.h driver.h sent.h exec.h instrs.h main.h port.h config.h \
    hosts/unix.h hosts/be/be.h machine.h

parse.o : xalloc.h wiz_list.h svalue.h stdstrings.h simulate.h object.h \
    mstrings.h main.h interpret.h instrs.h gcollect.h closure.h array.h \
    actions.h parse.h typedefs.h driver.h strfuns.h sent.h exec.h port.h \
    config.h hosts/unix.h hosts/be/be.h machine.h

parse_old.o : xalloc.h wiz_list.h svalue.h stdstrings.h simulate.h random.h \
    object.h mstrings.h main.h gcollect.h interpret.h closure.h array.h \
    actions.h parse.h my-alloca.h typedefs.h driver.h strfuns.h sent.h \
    exec.h instrs.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

parser.o : lang.c pkg-alists.h ../mudlib/sys/driver_hook.h xalloc.h \
    wiz_list.h switch.h swap.h svalue.h stdstrings.h simul_efun.h \
    simulate.h object.h mstrings.h mapping.h main.h lex.h instrs.h \
    interpret.h gcollect.h exec.h closure.h backend.h array.h prolang.h \
    my-alloca.h typedefs.h driver.h ptrtable.h strfuns.h sent.h port.h \
    config.h hosts/unix.h hosts/be/be.h machine.h

pcre-get.o : pcre/get.c driver.h pcre/internal.h port.h config.h \
    pcre/pcre.h pcre/config.h hosts/unix.h hosts/be/be.h machine.h

pcre-maketables.o : pcre/maketables.c driver.h pcre/internal.h port.h \
    config.h pcre/pcre.h pcre/config.h hosts/unix.h hosts/be/be.h machine.h

pcre-pcre.o : pcre/pcre.c driver.h pcre/chartables.c pcre/internal.h port.h \
    config.h pcre/pcre.h pcre/config.h hosts/unix.h hosts/be/be.h machine.h

pcre-study.o : pcre/study.c driver.h pcre/internal.h port.h config.h \
    pcre/pcre.h pcre/config.h hosts/unix.h hosts/be/be.h machine.h

dftables.o : pcre/maketables.c pcre/internal.h pcre/pcre.h pcre/config.h

pkg-alists.o : xalloc.h svalue.h simulate.h mstrings.h main.h interpret.h \
    array.h my-alloca.h pkg-alists.h typedefs.h driver.h strfuns.h sent.h \
    exec.h instrs.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

pkg-mysql.o : xalloc.h svalue.h simulate.h mstrings.h interpret.h array.h \
    pkg-mysql.h my-alloca.h typedefs.h driver.h strfuns.h sent.h exec.h \
    instrs.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

port.o : hosts/crypt.c main.h backend.h my-rusage.h driver.h typedefs.h \
    port.h config.h hosts/unix.h hosts/be/be.h machine.h

ptrtable.o : simulate.h mempools.h ptrtable.h driver.h svalue.h strfuns.h \
    sent.h exec.h typedefs.h port.h config.h hosts/unix.h hosts/be/be.h \
    machine.h

random.o : random.h driver.h port.h config.h hosts/unix.h hosts/be/be.h \
    machine.h

regexp.o : main.h xalloc.h simulate.h comm.h regexp.h driver.h typedefs.h \
    svalue.h strfuns.h sent.h exec.h port.h config.h hosts/unix.h \
    hosts/be/be.h machine.h

rxcache.o : ../mudlib/sys/debug_info.h xalloc.h svalue.h strfuns.h regexp.h \
    mstrings.h hash.h gcollect.h rxcache.h driver.h typedefs.h port.h \
    config.h hosts/unix.h hosts/be/be.h machine.h

simul_efun.o : xalloc.h swap.h svalue.h stdstrings.h simulate.h ptrtable.h \
    object.h mstrings.h lex.h interpret.h instrs.h gcollect.h exec.h \
    array.h simul_efun.h my-alloca.h typedefs.h driver.h strfuns.h sent.h \
    port.h config.h hosts/unix.h hosts/be/be.h machine.h

simulate.o : ../mudlib/sys/rtlimits.h ../mudlib/sys/files.h \
    ../mudlib/sys/driver_hook.h ../mudlib/sys/debug_info.h xalloc.h \
    wiz_list.h svalue.h swap.h strfuns.h stdstrings.h smalloc.h \
    simul_efun.h sent.h rxcache.h prolang.h otable.h object.h mstrings.h \
    mapping.h main.h lex.h instrs.h interpret.h heartbeat.h gcollect.h \
    filestat.h exec.h ed.h dumpstat.h comm.h closure.h call_out.h backend.h \
    array.h actions.h simulate.h my-alloca.h typedefs.h driver.h ptrtable.h \
    regexp.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

sprintf.o : xalloc.h swap.h svalue.h stdstrings.h simul_efun.h simulate.h \
    sent.h random.h ptrtable.h object.h mstrings.h mapping.h main.h \
    instrs.h interpret.h exec.h comm.h closure.h array.h actions.h \
    sprintf.h my-alloca.h typedefs.h driver.h strfuns.h port.h config.h \
    hosts/unix.h hosts/be/be.h machine.h

stdstrings.o : mstrings.h stdstrings.h typedefs.h driver.h port.h config.h \
    hosts/unix.h hosts/be/be.h machine.h

strfuns.o : xalloc.h svalue.h stdstrings.h simulate.h mstrings.h comm.h \
    strfuns.h my-alloca.h typedefs.h driver.h sent.h exec.h port.h config.h \
    hosts/unix.h hosts/be/be.h machine.h

swap.o : ../mudlib/sys/debug_info.h xalloc.h wiz_list.h svalue.h strfuns.h \
    stdstrings.h simul_efun.h simulate.h random.h prolang.h otable.h \
    object.h mstrings.h mapping.h main.h interpret.h gcollect.h exec.h \
    comm.h backend.h array.h swap.h typedefs.h driver.h ptrtable.h sent.h \
    instrs.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

wiz_list.o : xalloc.h svalue.h stdstrings.h simulate.h object.h mstrings.h \
    mapping.h main.h interpret.h gcollect.h backend.h array.h \
    ../mudlib/sys/wizlist.h wiz_list.h my-alloca.h typedefs.h driver.h \
    strfuns.h sent.h exec.h instrs.h port.h config.h hosts/unix.h \
    hosts/be/be.h machine.h

xalloc.o : smalloc.c main.h simulate.h interpret.h xalloc.h driver.h \
    ../mudlib/sys/debug_info.h object.h exec.h svalue.h stdstrings.h \
    mstrings.h gcollect.h backend.h smalloc.h typedefs.h strfuns.h sent.h \
    instrs.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

# --- DO NOT MODIFY THIS LINE -- AUTO-DEPENDS PRECEDE ---
