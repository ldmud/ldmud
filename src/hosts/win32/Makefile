# Generated automatically from Makefile.in by configure.
# These lines are needed on some machines.
MAKE=make
SHELL=/bin/sh
INSTALL=/usr/BIN/install -c
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
# Set MUD_LIB to the directory which contains the mud data. Was formerly
# defined in config.h !
MUD_LIB = /mud/lib
#
# Set BINDIR to the directory where you want to install the executables.
BINDIR = /mud/bin
#
#PROFIL= -DOPCPROF -DVERBOSE_OPCPROF
#PROFIL=-p -DMARK
#PROFIL=-pg
PROFIL=
#Enable warnings from the compiler, if wanted.
WARN= # no warning options - will work with all compilers :-)
#WARN= -Wall -Wshadow -Dlint
#WARN= -Wall -Wshadow -Wno-parentheses # gcc settings
#
# Optimization and source level debugging options.
# adding a -fomit-frame-pointer on the NeXT (gcc version 1.93 (68k, MIT syntax))
# will corrupt the driver.
#OPTIMIZE = -O4 -g -fstrength-reduce -fno-force-mem -fno-force-addr -fno-inline-functions -fomit-frame-pointer # high optimization
#OPTIMIZE= -O4 -g -fstrength-reduce -fno-force-mem -fno-force-addr -fno-inline-functions # for better debugging
OPTIMIZE= -g # no optimization; for frequent recompilations.

# Debugging options:
#   -DDEBUG: Enable run time debugging. It will use more time and space.
#            Nevertheless you are strongly encouraged to keep it defined.
#   -DYYDEBUG[=1]: Debug output for the LPC compiler.
#   -DTRACE_CODE: Enable the LPC code tracing.
#   -DCHECK_STRINGS: Enable the shared string checking.
#   -DKEEP_STRINGS: Shared strings are never really freed.
#   -DDEBUG_TELNET: Activates debug prints in the telnet machine
DEBUG = -DDEBUG -DTRACE_CODE -DCHECK_STRINGS # -DKEEP_STRINGS -DDEBUG_TELNET

MPATH=-DMUD_LIB='"$(MUD_LIB)"' -DBINDIR='"$(BINDIR)"'
#
CFLAGS=  $(OPTIMIZE) $(DEBUG) $(WARN) $(MPATH) $(PROFIL)
#
LIBS= -lkernel32 -luser32 -lwsock32 -lwinmm -lm 
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
    pkg-mysql.c ptrtable.c heartbeat.c actions.c mempools.c bitstrings.c \
    access_check.c parse_old.c parse.c prolang.y\
    efuns.c simul_efun.c strfuns.c sprintf.c gcollect.c closure.c random.c
OBJ=lang.o lex.o main.o interpret.o simulate.o object.o backend.o array.o\
    comm.o ed.o regexp.o mapping.o wiz_list.o swap.o xalloc.o\
    call_out.o otable.o dumpstat.o stralloc.o hash.o port.o files.o\
    pkg-mysql.o ptrtable.o heartbeat.o actions.o mempools.o bitstrings.o \
    access_check.o parse_old.o parse.o simul_efun.o strfuns.o sprintf.o\
    efuns.o gcollect.o closure.o random.o

driver: $(OBJ)
	$(CC) $(OPTIMIZE) $(LDFLAGS) $(OBJ) -o $@ $(LIBS)

install: driver
	$(INSTALL) -c $? $(BINDIR)/parse.exe

install.utils:
	(cd util; $(MAKE) $(MFLAGS) install)

utils: 
	(cd util; $(MAKE) $(MFLAGS))

parse: driver
	-$(MV) parse parse.old
	$(CP) driver parse

lint: *.c
	lint *.c

clean:
	$(RM) $(YACCTAB)h $(YACCTAB)c make_func.c *.o make_func efun_defs.c lang.y
	$(RM) lang.h lang.c y.output tags TAGS
	$(RM) parse core mudlib/core mudlib/debug.log lpmud.log
	(cd util ; echo "Cleaning in util." ; $(MAKE) clean)

distclean: clean
	$(RM) driver config.status config.log config.cache machine.h Makefile
	$(RM) config.h

tags: $(SRC)
	ctags $(SRC)

TAGS: $(SRC)
	etags $(SRC)

make_func.c: make_func.y
	$(YACC) make_func.y
	$(MV) $(YACCTAB)c make_func.c
	

make_func: make_func.o hash.o exec.h
	$(CC) $(OPTIMIZE) $(LDFLAGS) make_func.o hash.o -o make_func

efun_defs.c instrs.h: func_spec make_func config.h
  	$(RM) efun_defs.c
	$(RM) instrs.h
	./make_func instrs

lang.y: make_func prolang.y config.h
  	$(RM) lang.y
	./make_func lang

lang.c lang.h: lang.y
	$(YACC) -d -v lang.y
	$(MV) $(YACCTAB)c lang.c
	$(MV) $(YACCTAB)h lang.h

access_check.o : access_check.c lint.h config.h machine.h comm.h interpret.h

alloca.o : alloca.c

array.o : array.c config.h machine.h lint.h interpret.h object.h wiz_list.h \
  regexp.h exec.h lang.h instrs.h stralloc.h

backend.o : backend.c lint.h config.h machine.h interpret.h object.h \
  wiz_list.h exec.h comm.h

call_out.o : call_out.c lint.h config.h machine.h interpret.h object.h comm.h \
  stralloc.h exec.h wiz_list.h

closure.o : closure.c lint.h config.h machine.h lex.h exec.h interpret.h \
  object.h lang.h instrs.h

comm.o : comm.c config.h machine.h lint.h interpret.h comm.h object.h \
  sent.h wiz_list.h

dumpstat.o : dumpstat.c lint.h config.h machine.h interpret.h object.h exec.h \
  smalloc.h lang.h instrs.h

ed.o : ed.c lint.h config.h machine.h regexp.h interpret.h object.h comm.h

gcollect.o : gcollect.c lint.h config.h machine.h interpret.h object.h exec.h \
  sent.h comm.h smalloc.h instrs.h lang.h wiz_list.h stralloc.h

hash.o : hash.c

interpret.o : interpret.c lint.h config.h machine.h interpret.h lang.h exec.h \
  object.h wiz_list.h instrs.h comm.h sent.h switch.h smalloc.h stralloc.h

lang.o : lang.c lint.h config.h machine.h lex.h interpret.h object.h exec.h \
  instrs.h incralloc.h switch.h stralloc.h

lex.o : lex.c lint.h config.h machine.h interpret.h lang.h exec.h lex.h \
  instrs.h patchlevel.h stralloc.h efun_defs.c

main.o : main.c lint.h config.h machine.h interpret.h object.h lex.h \
  patchlevel.h

make_func.o : make_func.c lint.h config.h machine.h
	$(CC) $(CFLAGS) -DYACC='"$(YACC)"' -c make_func.c

mapping.o : mapping.c config.h machine.h lint.h interpret.h \
  object.h wiz_list.h regexp.h stralloc.h smalloc.h

object.o : object.c lint.h config.h machine.h comm.h interpret.h object.h \
  sent.h wiz_list.h exec.h

otable.o : otable.c lint.h config.h machine.h interpret.h object.h

parse.o : parse.c lint.h config.h machine.h interpret.h object.h wiz_list.h

parse_old.o : parse_old.c lint.h config.h machine.h interpret.h object.h \
  wiz_list.h

port.o : port.c lint.h config.h machine.h

regexp.o : regexp.c regexp.h lint.h config.h machine.h

simul_efun.o : simul_efun.c lint.h config.h machine.h interpret.h object.h \
  exec.h lex.h lang.h instrs.h stralloc.h

simulate.o : simulate.c lint.h config.h machine.h interpret.h instrs.h \
  object.h sent.h wiz_list.h exec.h comm.h stralloc.h

sprintf.o : sprintf.c config.h machine.h lint.h interpret.h lang.h instrs.h \
  exec.h object.h sent.h

stralloc.o : stralloc.c stralloc.h config.h machine.h lint.h

swap.o : swap.c lint.h config.h machine.h interpret.h object.h exec.h

wiz_list.o : wiz_list.c config.h machine.h lint.h interpret.h object.h \
  wiz_list.h stralloc.h
