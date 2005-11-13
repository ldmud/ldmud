# LPMud-Makefile for Dennis Vadura's DMake 3.8p4 -- 01-Nov-95
#   See also README.Amiga.
#
# DMake supports a generic startup with common macro definitions, so for me
# the following macros are defined automatically:
#
#   MAKE= $(MAKECMD) $(MFLAGS)
#
# The first part of the Makefile contains all this to be modified in standard
# environments. For special compilers or installations also the second part
# with the compiler specifics has to be modified. The third part covers the
# specifics of the net implementation.
# The fourth part contains the generic LPMud rules, the fifth any additional
# rules. There should be no need for modifications in both.

#-----------------------------------------------------------------------------
#			    Standard Configuration
#-----------------------------------------------------------------------------

# What Amiga-compiler do we use? Known by this Makefile (and some sources) are:
#   Compiler families (CCFAMILY):
#     DICE: any DICE compiler
#   Compiler versions (WHATCC, for more subtle differences):
#     DICE206: DICE 2.06.40
#     DICE207: DICE 2.07.53
#     DICE30 : DICE 3.0
#     DICE32 : DICE 3.2
# The macros CC,LD,AS and CFLAGS are set to decent values suiting the compiler.
# If you have a special installation, check the compiler specifics below.
# The CFLAGS will be modified further.

CCFAMILY= DICE
WHATCC= DICE32

# What type of net package do we use? Known by this Makefile are:
#   AS225  : Commodores AS225 software
#   AMITCP : The AmiTCP package from hut.fi, version < 3.0
#   AMITCP3: The AmiTCP package from hut.fi, version >= 3.0
#
# Leave it empty for the simulation (the default).

NETTYPE= AMITCP

# DMake's default-yacc 'Bison' won't work with Amylaars lang.y,
# but byacc will.

YACC= byacc
YFLAGS=

# Standard macros

YTAB= y.tab
CP= c:copy
RM= c:delete
MV= c:rename

# Unix has an option '-f' for it's 'rm' - not the Amiga, so I define
#   RMF= $(RM) -f
# and modify it for Amiga as:

RMF= $(RM)

# Compiling on a plain Amiga is slow, so keep some intermediate files:

PREC= .PRECIOUS


#-----------------------------------------------------------------------------
#			    Compiler Specifics
#-----------------------------------------------------------------------------

# Set CC,LD,AS and CFLAGS to the values needed by your configuration. Note
# the CFLAGS might be further modified.
#
# Set CCOPTS to any options you need to compile LPMud, what includes
# warning- and optimization options!
#
# If your compiler doesn't define the cpp-symbol AMIGA, it's a good idea
# to add it here.
# If your compiler doesn't have own code for float maths, you must
# define _NO_OWN_FLOATS so OS 1.3- or 68040-versions will compile properly.
#
# interpret.c and lang.c need lots of RAM to compile, thus it might be wise
# to put the temporaries somewhere else than into T: in RAM:.
# Use these macros to insert the needed compiler-options, including any
# non-standard code-model options.
#   LOPT: The options for lang.c
#   IOPT: The options for interpret.c
#
# To compile the hosts/amiga/signal, your compiler must be able to
# generate C-Code with registerized parameters. If it needs a special
# option to do this, put it into ROPT.
#
# If your compiler needs additional sources, put them into XSRC, and
# the object files to generate into XOBJ.
# If your compiler doesn't have alloca(), add alloca.c/alloca.o to
# the additional sources.

.IF $(CCFAMILY) == DICE

# Standard macros.

CC= dcc
LD= dlink
AS= das
CFLAGS= -O ""

# -gs: dynamic stack code (needed for its alloca)
# -mC: large code model
# -mD: the large data model.
# -s : include symbolic information into the executable.
# -d1: include line number information
# DICE uses the OS' math libraries, so OS-1.3/68040 versions can't use simple
# floats.
# AMIGA is defined automatically.

CCOPTS = -s -gs -mC -mD -D_NO_OWN_FLOATS # -DAMIGA

# Biggies: Large Code Model, temporary directory on disk

LOPT= -T DTMP:
IOPT= -T DTMP:

ROPT= -mRR

# DICE 2.xx/3.0 has problems in compiling the switch() in interpret.c and
# lang.c (it insists on using branches even in the large code model)
# and thus needs some afterpatching.
# All the necessary actions are enabled by defining DICE2.
# The intermediate assembler files are put into the directory defined
# with DTMP.
# Set IOPT2/LOPT2 appropriate to speed up the action, if possible or
# necessary. AOPT2 holds extra options ofr the assembler.

DTMP= DTMP:
IOPT2=
LOPT2=
AOPT2= -s

.IF $(WHATCC) == DICE206
DICE2= DICE2
.END
.IF $(WHATCC) == DICE207
DICE2= DICE2
.END
.IF $(WHATCC) == DICE30
DICE2= DICE2
.END
.IF $(WHATCC) == DICE32
DICE2= DICE2
.END

.END


#-----------------------------------------------------------------------------
#			      Net Specifics
#-----------------------------------------------------------------------------

# Set NETDEF to define the appropriate symbol selecting the special
# interface code.
# Set NETLIB to specify any additionally needed linker library.
#
# Default first...

NETDEF=
NETLIB=

.IF $(NETTYPE) == AS225
NETDEF= -DAMIGA_TCP -DAS255
NETLIB= -l socket
.END

.IF $(NETTYPE) == AMITCP
NETDEF= -DAMIGA_TCP -DAMITCP=2
NETLIB= -l net
.END

.IF $(NETTYPE) == AMITCP3
NETDEF= -DAMIGA_TCP -DAMITCP=3
NETLIB= -l net
.END


#-----------------------------------------------------------------------------
#			    LPMud Generic Rules
#-----------------------------------------------------------------------------

# Chose one of these mallocs:
#
# Satoria's malloc, tries to prevent fragmentation.
# Uses little memory. With FAST_FIT, it is also one of the fastest.
# Required for garbage collection.

MALLOC=smalloc

# Gnu malloc, fastest but uses most memory.
# MALLOC=gmalloc
#
# Use the standard malloc on your system.
# MALLOC=sysmalloc

# Set MUD_LIB to the directory which contains the mud data.

MUD_LIB = mudlib:

#
# Set BINDIR to the directory where you want to install the executables.

BINDIR = mudbin:

# Profiling is mostly useless for us.
#   PROFIL= -DOPCPROF -DVERBOSE_OPCPROF
#     would enable the profiling of the virtual stackmachine.

PROFIL=

# Debugging options: [-DDEBUG] [-DYYDEBUG|-DYYDEBUG=1] [-DTRACE_CODE]
# Defining DEBUG enables run time debugging. It will use more time and space.
# When the flag is changed, be sure to recompile everything.
# Simply comment out this line if not wanted.
# If you change anything in the source, you are strongly encouraged to have
# DEBUG defined.
# If you will not change anything, you are still encouraged to have
# it defined, if you want to locate any game driver bugs.

DEBUG=-DDEBUG -DTRACE_CODE

# Extra object files and libraries.

XOBJ += signal.o signal_rr.o socket.o amiga.o ixfile.o
XSRC += hosts/amiga/signal.c hosts/amiga/signal_rr.c\
	hosts/amiga/socket.c hosts/amiga/amiga.c hosts/amiga/ixfile.c

LIBS= $(NETLIB) -lm


# ----------------------- The Rules -------------------------------------

MPATH=-DMUD_LIB="$(MUD_LIB)" -DBINDIR="$(BINDIR)"

CCOPTS+= -D$(WHATCC)
LDFLAGS= $(CCOPTS)
DEFINES= $(DEBUG) $(PROFIL) -DMALLOC_$(MALLOC) $(MPATH) $(NETDEF)
CFLAGS+= $(CCOPTS) $(DEFINES)
YFLAGS += -d

SRC=lex.c main.c interpret.c simulate.c object.c backend.c array.c\
    comm1.c ed.c regexp.c mapping.c wiz_list.c swap.c $(MALLOC).c\
    call_out.c otable.c dumpstat.c stralloc.c hash.c port.c\
    access_check.c parse_old.c parse.c prolang.y\
    simul_efun.c sprintf.c gcollect.c closure.c random.c $(XSRC)
OBJ=lang.o lex.o main.o interpret.o simulate.o object.o backend.o array.o\
    comm1.o ed.o regexp.o mapping.o wiz_list.o swap.o $(MALLOC).o\
    call_out.o otable.o dumpstat.o stralloc.o hash.o port.o\
    access_check.o parse_old.o parse.o simul_efun.o sprintf.o\
    gcollect.o closure.o random.o $(XOBJ)

driver: $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) -o $@ $(LIBS)

# Utils and auto installation are not supported
# install: driver
#	 $(INSTALL) -c $? $(BINDIR)/parse
#
# install.utils:
#	 (cd util; $(MAKE) $(MFLAGS) install)
#
# utils:
#	  (cd util; $(MAKE) $(MFLAGS))

parse: driver
	+-$(MV) parse parse.old
	$(CP) driver parse

clean:
	-$(RMF) *.o *.a lang.c lang.h lang.y instrs.h efun_defs.c y.output
	-$(RMF) make_func.c make_func
	-$(RMF) driver astrip dstol PlayMud

tags: $(SRC)
	ctags $(SRC)

TAGS: $(SRC)
	etags $(SRC)

$(PREC) make_func.c: make_func.y
	-$(RM) make_func.c
	+$(YACC) $(YFLAGS) make_func.y
	$(MV) $(YTAB).c make_func.c

$(PREC) make_func: make_func.o hash.o ixfile.o exec.h
	$(CC) $(LDFLAGS) make_func.o hash.o ixfile.o -o make_func $(LIBS)

.PRECIOUS lang.y efun_defs.c instrs.h: func_spec make_func prolang.y config.h
	-$(RMF) efun_defs.c
	-$(RMF) lang.y
	-$(RMF) instrs.h
	make_func > efun_defs.c
# was:	./make_func > efun_defs.c

# Byacc will warn a 'shift/reduce' conflict.
# This is a known wart in the C-grammar.

lang.c lang.h: lang.y
	-$(RM) lang.c lang.h
	$(YACC) $(YFLAGS) -v lang.y
	$(MV) $(YTAB).c lang.c
	$(MV) $(YTAB).h lang.h

$(MALLOC).o: $(MALLOC).c lint.h config.h machine.h
	-$(RMF) smalloc.o
	-$(RMF) gmalloc.o
	-$(RMF) sysmalloc.o
	-$(RMF) malloc.o
	$(CC) $(CFLAGS) -c $(MALLOC).c

access_check.o : access_check.c lint.h config.h machine.h comm.h interpret.h

alloca.o : alloca.c

array.o: array.c config.h machine.h lint.h interpret.h object.h wiz_list.h \
  regexp.h exec.h lang.h instrs.h stralloc.h

backend.o: backend.c lint.h config.h machine.h interpret.h object.h \
  wiz_list.h exec.h comm.h hosts/amiga/patchfloat.h

call_out.o: call_out.c lint.h config.h machine.h interpret.h object.h comm.h \
  stralloc.h exec.h wiz_list.h

closure.o: closure.c lint.h config.h machine.h lex.h exec.h interpret.h \
  object.h lang.h instrs.h

comm1.o: comm1.c config.h machine.h lint.h interpret.h comm.h object.h \
  sent.h wiz_list.h

dumpstat.o: dumpstat.c lint.h config.h machine.h interpret.h object.h exec.h \
  smalloc.h lang.h instrs.h

ed.o: ed.c lint.h config.h machine.h regexp.h interpret.h object.h comm.h

gcollect.o : gcollect.c lint.h config.h machine.h interpret.h object.h exec.h \
  sent.h comm.h smalloc.h instrs.h lang.h wiz_list.h stralloc.h

$(PREC) hash.o: hash.c

.IF $(DICE2)
# I prefer compiling interpret.c in two steps, in case the three iterations
# over the assembler source aren't enough.

$(PREC) interpret.a: interpret.c lint.h config.h machine.h interpret.h lang.h exec.h \
  object.h wiz_list.h instrs.h comm.h sent.h switch.h smalloc.h stralloc.h \
  astrip hosts/amiga/patchfloat.h
	$(CC) $(IOPT2) $(CFLAGS) -a interpret.c -o $(DTMP)interpret.x
	astrip $(DTMP)interpret.x interpret.a
	$(RM) $(DTMP)interpret.x

interpret.o: interpret.a dstol
	-$(RM) $(DTMP)interpret.err
	-$(AS) interpret.a -o interpret.o -E $(DTMP)interpret.err
	dstol interpret.a $(DTMP)interpret.err $(DTMP)interpret.d z
	$(RM) $(DTMP)interpret.err
	-$(AS) $(DTMP)interpret.d -o interpret.o -E $(DTMP)interpret.err
	dstol $(DTMP)interpret.d $(DTMP)interpret.err $(DTMP)interpret.e y
	$(RM) $(DTMP)interpret.d $(DTMP)interpret.err
	astrip $(DTMP)interpret.e $(DTMP)interpret.f redebug
	-$(AS) $(DTMP)interpret.f -o interpret.o $(AOPT2)
	$(RM) $(DTMP)interpret.e $(DTMP)interpret.f
.ELSE
interpret.o: interpret.c lint.h config.h machine.h interpret.h lang.h exec.h \
  object.h wiz_list.h instrs.h comm.h sent.h switch.h smalloc.h stralloc.h \
  hosts/amiga/patchfloat.h
	$(CC) $(IOPT) $(CFLAGS) -c interpret.c -o interpret.o $(LIBS)
.END

.IF $(DICE2)
# I prefer compiling lang.c in two steps, in case the three iterations
# over the assembler source aren't enough.

$(PREC) lang.a: lang.c lint.h config.h machine.h lex.h interpret.h object.h exec.h \
  instrs.h incralloc.h switch.h stralloc.h \
  astrip
	$(CC) $(LOPT2) $(CFLAGS) -a lang.c -o $(DTMP)lang.x
	astrip $(DTMP)lang.x lang.a
	$(RM) $(DTMP)lang.x

lang.o: lang.a dstol
	-$(RM) $(DTMP)lang.err
	-$(AS) lang.a -o lang.o -E $(DTMP)lang.err
	dstol lang.a $(DTMP)lang.err $(DTMP)lang.d z
	$(RM) $(DTMP)lang.err
	-$(AS) $(DTMP)lang.d -o lang.o -E $(DTMP)lang.err
	dstol $(DTMP)lang.d $(DTMP)lang.err $(DTMP)lang.e y
	$(RM) $(DTMP)lang.err $(DTMP)lang.d
	-$(AS) $(DTMP)lang.e -o lang.o -E $(DTMP)lang.err
	dstol $(DTMP)lang.e $(DTMP)lang.err $(DTMP)lang.f x
	$(RM) $(DTMP)lang.err $(DTMP)lang.e
	astrip $(DTMP)lang.f $(DTMP)lang.g redebug
	-$(AS) $(DTMP)lang.g -o lang.o $(AOPT2)
	$(RM) $(DTMP)lang.f $(DTMP)lang.g
.ELSE
$(PREC) lang.o: lang.c lint.h config.h machine.h lex.h interpret.h object.h exec.h \
  instrs.h incralloc.h switch.h stralloc.h
	$(CC) $(LOPT) $(CFLAGS) -c lang.c -o $@
.END

lex.o : lex.c lint.h config.h machine.h interpret.h lang.h exec.h lex.h \
  instrs.h patchlevel.h stralloc.h efun_defs.c

main.o : main.c lint.h config.h machine.h interpret.h object.h lex.h \
  patchlevel.h

$(PREC) make_func.o : make_func.c lint.h config.h machine.h
	$(CC) $(CFLAGS) -DYACC="$(YACC)" -c make_func.c

mapping.o : mapping.c config.h machine.h lint.h interpret.h \
  object.h wiz_list.h regexp.h stralloc.h smalloc.h

object.o : object.c lint.h config.h machine.h comm.h interpret.h object.h \
  sent.h wiz_list.h exec.h hosts/amiga/patchfloat.h

otable.o : otable.c lint.h config.h machine.h interpret.h object.h \
  hosts/amiga/patchfloat.h

parse.o : parse.c lint.h config.h machine.h interpret.h object.h wiz_list.h

parse_old.o : parse_old.c lint.h config.h machine.h interpret.h object.h \
  wiz_list.h

port.o : port.c lint.h config.h machine.h

regexp.o : regexp.c regexp.h lint.h config.h machine.h #/usr/include/string.h

simul_efun.o : simul_efun.c lint.h config.h machine.h interpret.h object.h \
  exec.h lex.h lang.h instrs.h stralloc.h

simulate.o : simulate.c lint.h config.h machine.h interpret.h instrs.h \
  object.h sent.h wiz_list.h exec.h comm.h stralloc.h hosts/amiga/patchfloat.h

sprintf.o : sprintf.c config.h machine.h lint.h interpret.h lang.h instrs.h \
  exec.h object.h sent.h

stralloc.o : stralloc.c stralloc.h config.h machine.h lint.h \
  hosts/amiga/patchfloat.h

swap.o : swap.c lint.h config.h machine.h interpret.h object.h exec.h

wiz_list.o : wiz_list.c config.h machine.h lint.h interpret.h object.h \
  wiz_list.h stralloc.h

# It's not that easy under AmigaOS...
#
# timestamp:
#	  echo '#define RELEASE_DATE "'`date`'"' > tmp
#	  tail +1 patchlevel.h >> tmp
#	  $(MV) tmp patchlevel.h

#-----------------------------------------------------------------------------
#			    Extra Rules
#-----------------------------------------------------------------------------

#   crypt.o  : BSD-implementation of crypt()
#   fcrypt.o : Crack-implementation of crypt() (just in case)
#   PlayMud  : a simple client to access the running mud.
#   netclean : removes all object files containing communication-specific code.
#   osclean  : removes all object files which contain OS-specific code.
#   amiga.o  : some amiga specific routines.
#   ixfile.o : Unix-filesystem disguise.
#   signal.o : timers and interruption. It uses interrupt code and therefore
#	       needs fully registered args.
#   socket.o : includes the appropriate socket-code.
#
# DICE 2.xx also needs:
#   astrip: to strip comments from intermediate assembler files
#   dstol : to patch assembler files with large switch()es

fcrypt.o :
	$(CC) $(CFLAGS) -Dfcrypt=crypt -Ihosts/fcrypt -c hosts/fcrypt/crack-fcrypt.c -o fcrypt.o

crypt.o :
	$(CC) $(CFLAGS) -c hosts/crypt.c -o crypt.o

playmud: PlayMud

PlayMud: hosts/amiga/mudmsgs.h
PlayMud: hosts/amiga/PlayMud.c
	$(CC) $< -o $@

netclean:
	-$(RM) access_check.o comm1.o main.o swap.o socket.o amiga.o

osclean:
	-$(RM) $(XOBJ)
	-$(RM) backend.o interpret.o object.o otable.o simulate.o stralloc.o
	-$(RM) PlayMud driver
.IF $(DICE2)
	-$(RM) interpret.a astrip dstol
.END

amiga.o : patchlevel.h config.h hosts/amiga/nsignal.h
amiga.o : hosts/amiga/amiga.c
	$(CC) $(CFLAGS) -Ihosts/amiga -c $< -o $@

ixfile.o : hosts/amiga/ixfile.c
	$(CC) $(CFLAGS) -Ihosts/amiga -c $< -o $@

signal.o : hosts/amiga/nsignal.h
signal.o : hosts/amiga/signal.c
	$(CC) $(CFLAGS) -Ihosts/amiga -c $< -o $@

signal_rr.o : hosts/amiga/nsignal.h
signal_rr.o : hosts/amiga/signal_rr.c
	$(CC) $(CFLAGS) $(ROPT) -Ihosts/amiga -c $< -o $@

socket.o : hosts/amiga/nsignal.h hosts/amiga/socket_sim.c hosts/amiga/socket_tcp.c\
	   hosts/amiga/mudmsgs.h
socket.o : hosts/amiga/socket.c
	$(CC) $(CFLAGS) -Ihosts/amiga -c $< -o $@

astrip: hosts/amiga/astrip.c
	$(CC) $? -o $@

dstol: hosts/amiga/dstol.c
	$(CC) $? -o $@
