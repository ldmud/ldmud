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
# Gnu malloc, fastest but uses most memory.
#MALLOC=gmalloc
#
# Use the standard malloc on your system:
#MALLOC=sysmalloc
#
# Set MUD_LIB to the directory which contains the mud data. Was formerly
# defined in config.h !
MUD_LIB = /user/mud/mudlib
#
# Set BINDIR to the directory where you want to install the executables.
BINDIR = /mud/bin
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
	$(CC) $(OPTIMIZE) $(LDFLAGS) $(OBJ) -o $@ -lpml
	fixstk 96k driver

install: driver
	install -c $? $(BINDIR)/parse

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
	$(RM) lang.h lang.c y.output tags TAGS frontend.c frontend
	$(RM) parse core mudlib/core mudlib/debug.log lpmud.log
	(cd util ; echo "Cleaning in util." ; $(MAKE) clean)

#include has problems when the toplevel source file is in a subdirectory.
frontend: hosts/atari/frontend.c config.h comm.h
	cp hosts/atari/frontend.c .
	$(CC) $(CFLAGS) frontend.c -o frontend $(LIBS)
	rm frontend.c
	fixstk 16k frontend

distclean: clean
	$(RM) driver config.status machine.h Makefile

tags: $(SRC)
	ctags $(SRC)

TAGS: $(SRC)
	etags $(SRC)

make_func.c: make_func.y
	$(YACC) make_func.y
	$(MV) $(YACCTAB)c make_func.c
	

make_func: make_func.o hash.o
	$(CC) $(OPTIMIZE) $(LDFLAGS) make_func.o hash.o -o make_func
	fixstk 96k make_func

lang.y efun_defs.c instrs.h: func_spec make_func prolang.y config.h
	$(RM) efun_defs.c
	$(RM) lang.y
	./make_func > efun_defs.c

lang.c lang.h: lang.y
	$(YACC) -d -v lang.y
	$(MV) $(YACCTAB)c lang.c
	
	$(MV) $(YACCTAB)h lang.h

$(MALLOC).o : $(MALLOC).c lint.h config.h machine.h
	$(RM) smalloc.o
	$(RM) gmalloc.o
	$(RM) sysmalloc.o
	$(RM) malloc.o
	$(CC) $(CFLAGS) -c $(MALLOC).c

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

comm1.o : comm1.c config.h machine.h lint.h interpret.h comm.h object.h \
  sent.h wiz_list.h

dumpstat.o : dumpstat.c lint.h config.h machine.h interpret.h object.h exec.h \
  smalloc.h lang.h instrs.h

ed.o : ed.c lint.h config.h machine.h regexp.h interpret.h object.h comm.h

gcollect.o : gcollect.c lint.h config.h machine.h interpret.h object.h exec.h \
  sent.h comm.h smalloc.h instrs.h lang.h wiz_list.h stralloc.h

hash.o : hash.c

interpret.s : interpret.c lint.h config.h machine.h interpret.h lang.h exec.h \
  object.h wiz_list.h instrs.h comm.h sent.h switch.h smalloc.h stralloc.h
	$(CC) $(CFLAGS) -S interpret.c

interpret.o : interpret.s
	$(CC) $(CFLAGS) -c interpret.s

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
