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
# interpret.c and parser.c need lots of RAM to compile, thus it might be wise
# to put the temporaries somewhere else than into T: in RAM:.
# Use these macros to insert the needed compiler-options, including any
# non-standard code-model options.
#   LOPT: The options for parser.c
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
CFLAGS= -Dlint -O ""

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
# parser.c (it insists on using branches even in the large code model)
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

# Set MUD_LIB to the directory which contains the mud data.

MUD_LIB = mudlib:

#
# Set BINDIR to the directory where you want to install the executables.

BINDIR = mudbin:

#
# Set ERQDIR to where executables are stored which ERQ may start.

ERQDIR = mudbin:erq

# Profiling is mostly useless for us, except for the options available
# in config.h .

PROFIL=

# Debugging options:
#   Debugging options should be set in config.h to guarantee a clean
#   recompile.
DEBUG=

# Extra object files and libraries.

XOBJ += signal.o signal_rr.o socket.o amiga.o ixfile.o
XSRC += hosts/amiga/signal.c hosts/amiga/signal_rr.c\
	hosts/amiga/socket.c hosts/amiga/amiga.c hosts/amiga/ixfile.c

LIBS= $(NETLIB) -lm


# ----------------------- The Rules -------------------------------------

MPATH=-DMUD_LIB="$(MUD_LIB)" -DBINDIR="$(BINDIR)" -DERQ_DIR="$(ERQDIR)"

CCOPTS+= -D$(WHATCC)
LDFLAGS= $(CCOPTS)
DEFINES= $(DEBUG) $(PROFIL) $(MPATH) $(NETDEF)
CFLAGS+= $(CCOPTS) $(DEFINES)
YFLAGS += -d

SRC=lex.c main.c interpret.c simulate.c object.c backend.c array.c\
    comm.c ed.c regexp.c mapping.c wiz_list.c swap.c xalloc.c\
    call_out.c otable.c dumpstat.c stralloc.c hash.c port.c\
    pkg-mccp.c pkg-mysql.c pkg-tls.c ptrtable.c heartbeat.c actions.c mempools.c \
    access_check.c parse_old.c parse.c prolang.y rxcache.c md5.c\
    simul_efun.c strfuns.c sprintf.c gcollect.c closure.c random.c efuns.c\
    $(XSRC)
OBJ=parser.o lex.o main.o interpret.o simulate.o object.o backend.o array.o\
    comm.o ed.o regexp.o mapping.o wiz_list.o swap.o xalloc.o\
    call_out.o otable.o dumpstat.o stralloc.o hash.o port.o\
    pkg-mccp.o pkg-mysql.o pkg-tls.o ptrtable.o heartbeat.o actions.o mempools.o md5.o\
    access_check.o parse_old.o parse.o simul_efun.o strfuns.o sprintf.o\
    efuns.o gcollect.o closure.o random.o rxcache.o $(XOBJ)

ldmud: $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) -o $@ $(LIBS)

# Utils and auto installation are not supported
# install: ldmud
#	 $(INSTALL) -c $? $(BINDIR)/ldmud
#
# install.utils:
#	 (cd util; $(MAKE) $(MFLAGS) install)
#
# utils:
#	  (cd util; $(MAKE) $(MFLAGS))

clean:
	-$(RMF) *.o *.a lang.c lang.h lang.y instrs.h efun_defs.c y.output
	-$(RMF) stdstrings.c stdstrings.h
	-$(RMF) make_func.c make_func
	-$(RMF) ldmud astrip dstol PlayMud

tags: $(SRC)
	ctags $(SRC)

TAGS: $(SRC)
	etags $(SRC)

$(PREC) make_func.c: make_func.y
	-$(RM) make_func.c
	+$(YACC) $(YFLAGS) make_func.y
	$(MV) $(YTAB).c make_func.c

$(PREC) make_func: make_func_main.o hash.o ixfile.o exec.h
	$(CC) $(LDFLAGS) make_func_main.o hash.o ixfile.o -o make_func $(LIBS)

.PRECIOUS efun_defs.c instrs.h: func_spec make_func config.h
	-$(RMF) efun_defs.c
	-$(RMF) instrs.h
	make_func instrs

.PRECIOUS stdstrings.c stdstrings.h: string_spec make_func config.h
	$(RM) stdstrings.c
	$(RM) stdstrings.h
	./make_func strings

.PRECIOUS lang.y: make_func prolang.y config.h
	-$(RMF) lang.y
	make_func lang

# Byacc will warn a 'shift/reduce' conflict.
# This is a known wart in the C-grammar.

lang.c lang.h: lang.y
	-$(RM) lang.c lang.h
	$(YACC) $(YFLAGS) -v lang.y
	$(MV) $(YTAB).c lang.c
	$(MV) $(YTAB).h lang.h

#--------------------------------------------------------
# Dependencies, manual and automatic.

.IF $(DICE2)
# I prefer compiling interpret.c in two steps, in case the three iterations
# over the assembler source aren't enough.

$(PREC) interpret.a : interpret.c closure.h swap.h sprintf.h lex.h gcollect.h \
    parse.h call_out.h simul_efun.h simulate.h mapping.h array.h main.h \
    backend.h stralloc.h switch.h sent.h comm.h instrs.h wiz_list.h \
    object.h exec.h prolang.h smalloc.h interpret.h lint.h config.h \
    machine.h lang.h port.h hosts/amiga/patchfloat.h \
    astrip
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
interpret.o : interpret.c closure.h swap.h sprintf.h lex.h gcollect.h \
    parse.h call_out.h simul_efun.h simulate.h mapping.h array.h main.h \
    backend.h stralloc.h switch.h sent.h comm.h instrs.h wiz_list.h \
    object.h exec.h prolang.h smalloc.h interpret.h lint.h config.h \
    machine.h lang.h port.h hosts/amiga/patchfloat.h
	$(CC) $(IOPT) $(CFLAGS) -c interpret.c -o interpret.o $(LIBS)
.END

.IF $(DICE2)
# I prefer compiling parser.c in two steps, in case the three iterations
# over the assembler source aren't enough.

$(PREC) parser.a : parser.c lang.c simul_efun.h mapping.h swap.h backend.h \
    array.h \
    closure.h main.h simulate.h stralloc.h switch.h instrs.h config.h \
    exec.h object.h interpret.h lex.h prolang.h lint.h sent.h smalloc.h \
    port.h machine.h lang.h hosts/amiga/patchfloat.h \
    astrip
	$(CC) $(LOPT2) $(CFLAGS) -a parser.c -o $(DTMP)parser.x
	astrip $(DTMP)parser.x parser.a
	$(RM) $(DTMP)parser.x

parser.o: parser.a dstol
	-$(RM) $(DTMP)parser.err
	-$(AS) parser.a -o parser.o -E $(DTMP)parser.err
	dstol parser.a $(DTMP)parser.err $(DTMP)parser.d z
	$(RM) $(DTMP)parser.err
	-$(AS) $(DTMP)parser.d -o parser.o -E $(DTMP)parser.err
	dstol $(DTMP)parser.d $(DTMP)parser.err $(DTMP)parser.e y
	$(RM) $(DTMP)parser.err $(DTMP)parser.d
	-$(AS) $(DTMP)parser.e -o parser.o -E $(DTMP)parser.err
	dstol $(DTMP)parser.e $(DTMP)parser.err $(DTMP)parser.f x
	$(RM) $(DTMP)parser.err $(DTMP)parser.e
	astrip $(DTMP)parser.f $(DTMP)parser.g redebug
	-$(AS) $(DTMP)parser.g -o parser.o $(AOPT2)
	$(RM) $(DTMP)parser.f $(DTMP)parser.g
.ELSE
$(PREC) lang.o : parser.c lang.c simul_efun.h mapping.h swap.h backend.h array.h \
    closure.h main.h simulate.h stralloc.h switch.h instrs.h config.h \
    exec.h object.h interpret.h lex.h prolang.h lint.h sent.h smalloc.h \
    port.h machine.h lang.h hosts/amiga/patchfloat.h
	$(CC) $(LOPT) $(CFLAGS) -c parser.c -o $@
.END

#-----------------------------------------------------------------------------
#			    Extra Rules
#-----------------------------------------------------------------------------

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

playmud: PlayMud

PlayMud: hosts/amiga/mudmsgs.h
PlayMud: hosts/amiga/PlayMud.c
	$(CC) $< -o $@

netclean:
	-$(RM) access_check.o comm.o main.o swap.o socket.o amiga.o

osclean:
	-$(RM) $(XOBJ)
	-$(RM) backend.o interpret.o object.o otable.o simulate.o stralloc.o
	-$(RM) PlayMud ldmud
.IF $(DICE2)
	-$(RM) interpret.a astrip dstol
.END

amiga.o : hosts/amiga/amiga.c
	$(CC) $(CFLAGS) -Ihosts/amiga -c $< -o $@

ixfile.o : hosts/amiga/ixfile.c
	$(CC) $(CFLAGS) -Ihosts/amiga -c $< -o $@

signal.o : hosts/amiga/signal.c
	$(CC) $(CFLAGS) -Ihosts/amiga -c $< -o $@

signal_rr.o : hosts/amiga/signal_rr.c
	$(CC) $(CFLAGS) $(ROPT) -Ihosts/amiga -c $< -o $@

socket.o : hosts/amiga/socket.c
	$(CC) $(CFLAGS) -Ihosts/amiga -c $< -o $@

astrip: hosts/amiga/astrip.c
	$(CC) $? -o $@

dstol: hosts/amiga/dstol.c
	$(CC) $? -o $@

# --- DO NOT MODIFY THIS LINE -- AUTO-DEPENDS FOLLOW ---
access_check.o : access_check.c xalloc.h filestat.h comm.h access_check.h \
    driver.h pkg-tls.h svalue.h simulate.h typedefs.h config.h port.h \
    strfuns.h sent.h instrs.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h exec.h \
    ./hosts/amiga/ixfile.h

actions.o : actions.c ../mudlib/sys/driver_hook.h ../mudlib/sys/commands.h \
    xalloc.h wiz_list.h stralloc.h smalloc.h simulate.h sent.h object.h \
    mapping.h interpret.h exec.h efuns.h dumpstat.h comm.h closure.h \
    backend.h array.h actions.h typedefs.h driver.h svalue.h main.h \
    strfuns.h instrs.h pkg-tls.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

array.o : array.c ../mudlib/sys/inherit_list.h ../mudlib/sys/include_list.h \
    ../mudlib/sys/functionlist.h smalloc.h xalloc.h wiz_list.h swap.h \
    stralloc.h svalue.h simulate.h rxcache.h regexp.h object.h mempools.h \
    mapping.h main.h interpret.h instrs.h closure.h backend.h array.h \
    my-alloca.h typedefs.h driver.h ../mudlib/sys/lpctypes.h strfuns.h \
    sent.h exec.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

backend.o : backend.c ../mudlib/sys/debug_message.h \
    ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h swap.h stralloc.h \
    stdstrings.h smalloc.h simulate.h rxcache.h regexp.h random.h otable.h \
    object.h my-alloca.h mempools.h mapping.h main.h lex.h interpret.h \
    heartbeat.h gcollect.h filestat.h exec.h ed.h comm.h closure.h \
    call_out.h array.h actions.h backend.h hosts/amiga/nsignal.h typedefs.h \
    driver.h svalue.h strfuns.h sent.h instrs.h pkg-tls.h port.h config.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

call_out.o : call_out.c ../mudlib/sys/debug_info.h xalloc.h wiz_list.h \
    swap.h svalue.h strfuns.h stralloc.h simulate.h object.h main.h \
    interpret.h gcollect.h exec.h comm.h closure.h backend.h array.h \
    actions.h call_out.h typedefs.h driver.h smalloc.h sent.h instrs.h \
    pkg-tls.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

closure.o : closure.c xalloc.h switch.h swap.h svalue.h stralloc.h \
    stdstrings.h simul_efun.h simulate.h prolang.h object.h main.h lex.h \
    lang.h interpret.h instrs.h exec.h backend.h array.h closure.h \
    my-alloca.h typedefs.h driver.h smalloc.h strfuns.h ptrtable.h sent.h \
    port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

comm.o : comm.c util/erq/erq.h ../mudlib/sys/driver_hook.h \
    ../mudlib/sys/input_to.h ../mudlib/sys/comm.h xalloc.h wiz_list.h \
    swap.h stralloc.h stdstrings.h smalloc.h simulate.h sent.h pkg-tls.h \
    pkg-mccp.h object.h main.h interpret.h gcollect.h filestat.h exec.h \
    ed.h closure.h backend.h array.h actions.h access_check.h comm.h \
    hosts/amiga/nsignal.h telnet.h my-alloca.h typedefs.h driver.h svalue.h \
    strfuns.h instrs.h config.h port.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

dumpstat.o : dumpstat.c svalue.h xalloc.h smalloc.h simulate.h ptrtable.h \
    object.h mapping.h instrs.h filestat.h exec.h closure.h array.h \
    dumpstat.h typedefs.h driver.h strfuns.h sent.h port.h config.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

ed.o : ed.c xalloc.h svalue.h stralloc.h stdstrings.h simulate.h rxcache.h \
    regexp.h object.h main.h interpret.h gcollect.h filestat.h comm.h \
    closure.h actions.h ed.h typedefs.h driver.h smalloc.h strfuns.h sent.h \
    instrs.h pkg-tls.h exec.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

efuns.o : efuns.c ../mudlib/sys/time.h ../mudlib/sys/strings.h \
    ../mudlib/sys/objectinfo.h ../mudlib/sys/debug_info.h xalloc.h \
    wiz_list.h svalue.h swap.h strfuns.h stralloc.h stdstrings.h smalloc.h \
    simulate.h rxcache.h ptrtable.h otable.h object.h mempools.h md5.h \
    mapping.h main.h interpret.h instrs.h heartbeat.h dumpstat.h comm.h \
    closure.h call_out.h backend.h array.h actions.h efuns.h my-alloca.h \
    typedefs.h driver.h sent.h regexp.h exec.h pkg-tls.h port.h config.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

gcollect.o : gcollect.c ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h \
    swap.h stralloc.h stdstrings.h smalloc.h simul_efun.h simulate.h sent.h \
    rxcache.h prolang.h parse.h otable.h object.h mempools.h mapping.h \
    main.h lex.h interpret.h instrs.h heartbeat.h filestat.h exec.h ed.h \
    comm.h closure.h call_out.h backend.h array.h actions.h gcollect.h \
    typedefs.h driver.h svalue.h strfuns.h ptrtable.h regexp.h pkg-tls.h \
    port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

hash.o : hash.c hash.h

heartbeat.o : heartbeat.c ../mudlib/sys/debug_info.h xalloc.h wiz_list.h \
    svalue.h strfuns.h simulate.h sent.h object.h interpret.h gcollect.h \
    comm.h backend.h array.h actions.h heartbeat.h typedefs.h driver.h \
    instrs.h pkg-tls.h main.h port.h config.h exec.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

amiga.o : hosts/amiga/amiga.c hosts/amiga/nsignal.h ./patchlevel.h \
    hosts/amiga/config.h

ixfile.o : hosts/amiga/ixfile.c

signal.o : hosts/amiga/signal.c hosts/amiga/nsignal.h

signal_rr.o : hosts/amiga/signal_rr.c hosts/amiga/nsignal.h

socket.o : hosts/amiga/socket.c hosts/amiga/socket_sim.c \
    hosts/amiga/socket_tcp.c hosts/amiga/nsignal.h hosts/amiga/mudmsgs.h \
    hosts/amiga/telnet.h hosts/amiga/socket_sim_protos.h \
    hosts/amiga/socket.h ./hosts/amiga/socket_sim.h \
    ./hosts/amiga/socket_tcp.h

interpret.o : interpret.c ../mudlib/sys/trace.h ../mudlib/sys/debug_info.h \
    ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h switch.h swap.h \
    svalue.h sprintf.h smalloc.h stralloc.h stdstrings.h simul_efun.h \
    simulate.h sent.h random.h ptrtable.h prolang.h parse.h otable.h \
    object.h mapping.h main.h lex.h instrs.h heartbeat.h gcollect.h \
    filestat.h exec.h efuns.h ed.h comm.h closure.h call_out.h backend.h \
    array.h actions.h interpret.h my-rusage.h my-alloca.h typedefs.h \
    driver.h strfuns.h pkg-tls.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

lex.o : lex.c efun_defs.c hosts/amiga/socket.h ../mudlib/sys/driver_hook.h \
    xalloc.h wiz_list.h strfuns.h stralloc.h stdstrings.h simul_efun.h \
    simulate.h prolang.h ./patchlevel.h object.h mempools.h main.h lang.h \
    interpret.h instrs.h hash.h gcollect.h filestat.h exec.h comm.h \
    closure.h backend.h array.h lex.h my-alloca.h typedefs.h driver.h \
    ./hosts/amiga/socket_sim.h ./hosts/amiga/socket_tcp.h svalue.h \
    smalloc.h ptrtable.h sent.h pkg-tls.h port.h config.h \
    hosts/amiga/socket_sim_protos.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

main.o : main.c pkg-mysql.h xalloc.h wiz_list.h swap.h svalue.h stralloc.h \
    stdstrings.h simul_efun.h simulate.h rxcache.h random.h ./patchlevel.h \
    otable.h object.h mempools.h mapping.h lex.h interpret.h gcollect.h \
    filestat.h comm.h array.h backend.h main.h hosts/amiga/socket.h \
    my-alloca.h typedefs.h driver.h smalloc.h strfuns.h ptrtable.h exec.h \
    sent.h instrs.h regexp.h pkg-tls.h ./hosts/amiga/socket_sim.h \
    ./hosts/amiga/socket_tcp.h port.h config.h \
    hosts/amiga/socket_sim_protos.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

make_func_main.o : make_func_main.c make_func.c ../mudlib/sys/driver_hook.h \
    hash.h exec.h my-alloca.h driver.h typedefs.h port.h config.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

mapping.o : mapping.c xalloc.h wiz_list.h svalue.h stralloc.h smalloc.h \
    simulate.h regexp.h object.h main.h interpret.h gcollect.h closure.h \
    backend.h array.h mapping.h my-alloca.h typedefs.h driver.h strfuns.h \
    sent.h instrs.h exec.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

md5.o : md5.c md5.h machine.h

mempools.o : mempools.c ../mudlib/sys/debug_info.h xalloc.h svalue.h \
    strfuns.h simulate.h gcollect.h mempools.h driver.h typedefs.h sent.h \
    instrs.h port.h config.h exec.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

object.o : object.c ../mudlib/sys/driver_hook.h \
    ../mudlib/sys/functionlist.h xalloc.h wiz_list.h svalue.h swap.h \
    strfuns.h stralloc.h stdstrings.h simul_efun.h simulate.h smalloc.h \
    sent.h random.h ptrtable.h prolang.h otable.h mempools.h mapping.h \
    main.h lex.h lang.h interpret.h filestat.h exec.h comm.h closure.h \
    backend.h array.h actions.h object.h my-alloca.h typedefs.h driver.h \
    ../mudlib/sys/lpctypes.h instrs.h pkg-tls.h port.h config.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

otable.o : otable.c ../mudlib/sys/debug_info.h xalloc.h svalue.h simulate.h \
    strfuns.h object.h hash.h gcollect.h backend.h otable.h typedefs.h \
    driver.h sent.h instrs.h main.h port.h config.h exec.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

parse.o : parse.c xalloc.h wiz_list.h svalue.h stralloc.h stdstrings.h \
    simulate.h object.h main.h interpret.h instrs.h gcollect.h closure.h \
    array.h actions.h parse.h typedefs.h driver.h smalloc.h strfuns.h \
    sent.h exec.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

parse_old.o : parse_old.c xalloc.h wiz_list.h svalue.h stralloc.h \
    stdstrings.h simulate.h random.h object.h main.h gcollect.h interpret.h \
    closure.h array.h actions.h parse.h typedefs.h driver.h smalloc.h \
    strfuns.h sent.h instrs.h exec.h port.h config.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

parser.o : parser.c lang.c ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h \
    switch.h swap.h svalue.h stralloc.h stdstrings.h simul_efun.h \
    simulate.h object.h mapping.h main.h lex.h instrs.h interpret.h \
    gcollect.h exec.h closure.h backend.h array.h prolang.h my-alloca.h \
    typedefs.h driver.h smalloc.h strfuns.h ptrtable.h sent.h port.h \
    config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

pkg-mccp.o : pkg-mccp.c telnet.h xalloc.h svalue.h interpret.h object.h \
    comm.h array.h pkg-mccp.h typedefs.h driver.h instrs.h sent.h pkg-tls.h \
    simulate.h port.h config.h exec.h strfuns.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

pkg-mysql.o : pkg-mysql.c xalloc.h svalue.h simulate.h main.h interpret.h \
    array.h pkg-mysql.h my-alloca.h typedefs.h driver.h strfuns.h sent.h \
    instrs.h port.h config.h exec.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

pkg-tls.o : pkg-tls.c ../mudlib/sys/tls.h xalloc.h svalue.h object.h main.h \
    interpret.h comm.h array.h actions.h pkg-tls.h machine.h driver.h \
    typedefs.h sent.h instrs.h simulate.h port.h config.h exec.h strfuns.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    ./hosts/amiga/ixfile.h

port.o : port.c hosts/crypt.c main.h backend.h my-rusage.h driver.h \
    typedefs.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

ptrtable.o : ptrtable.c simulate.h mempools.h ptrtable.h driver.h svalue.h \
    strfuns.h sent.h instrs.h typedefs.h port.h config.h exec.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

random.o : random.c random.h driver.h port.h config.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

regexp.o : regexp.c main.h xalloc.h simulate.h comm.h regexp.h driver.h \
    typedefs.h svalue.h strfuns.h sent.h instrs.h pkg-tls.h port.h config.h \
    exec.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

rxcache.o : rxcache.c ../mudlib/sys/debug_info.h xalloc.h svalue.h \
    strfuns.h stralloc.h smalloc.h regexp.h hash.h gcollect.h rxcache.h \
    driver.h typedefs.h main.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

simul_efun.o : simul_efun.c xalloc.h swap.h svalue.h stralloc.h \
    stdstrings.h simulate.h ptrtable.h object.h lex.h interpret.h instrs.h \
    gcollect.h exec.h array.h simul_efun.h my-alloca.h typedefs.h driver.h \
    main.h smalloc.h strfuns.h sent.h port.h config.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

simulate.o : simulate.c ../mudlib/sys/rtlimits.h ../mudlib/sys/files.h \
    ../mudlib/sys/driver_hook.h ../mudlib/sys/debug_info.h xalloc.h \
    wiz_list.h svalue.h swap.h strfuns.h stralloc.h stdstrings.h \
    simul_efun.h sent.h rxcache.h prolang.h pkg-tls.h otable.h object.h \
    mempools.h mapping.h main.h lex.h instrs.h interpret.h heartbeat.h \
    gcollect.h filestat.h exec.h ed.h dumpstat.h comm.h closure.h \
    call_out.h backend.h array.h actions.h simulate.h hosts/amiga/nsignal.h \
    my-alloca.h typedefs.h driver.h smalloc.h ptrtable.h regexp.h port.h \
    config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

sprintf.o : sprintf.c xalloc.h swap.h stralloc.h stdstrings.h simul_efun.h \
    simulate.h sent.h random.h ptrtable.h object.h mapping.h main.h \
    instrs.h interpret.h exec.h closure.h array.h sprintf.h my-alloca.h \
    typedefs.h driver.h smalloc.h strfuns.h svalue.h port.h config.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

stdstrings.o : stdstrings.c stralloc.h stdstrings.h driver.h main.h \
    smalloc.h strfuns.h port.h config.h typedefs.h svalue.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

stralloc.o : stralloc.c ../mudlib/sys/debug_info.h xalloc.h svalue.h \
    strfuns.h stdstrings.h smalloc.h simulate.h main.h hash.h gcollect.h \
    stralloc.h driver.h typedefs.h sent.h instrs.h port.h config.h exec.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

strfuns.o : strfuns.c xalloc.h svalue.h simulate.h comm.h strfuns.h \
    typedefs.h driver.h sent.h instrs.h pkg-tls.h port.h config.h exec.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

swap.o : swap.c hosts/amiga/socket.h ../mudlib/sys/debug_info.h xalloc.h \
    wiz_list.h svalue.h strfuns.h stralloc.h simul_efun.h simulate.h \
    random.h prolang.h otable.h object.h mempools.h mapping.h main.h \
    interpret.h gcollect.h exec.h comm.h backend.h array.h swap.h \
    typedefs.h driver.h ./hosts/amiga/socket_sim.h \
    ./hosts/amiga/socket_tcp.h smalloc.h ptrtable.h sent.h instrs.h \
    pkg-tls.h port.h config.h hosts/amiga/socket_sim_protos.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

wiz_list.o : wiz_list.c xalloc.h svalue.h stralloc.h stdstrings.h \
    simulate.h object.h mapping.h main.h interpret.h gcollect.h backend.h \
    array.h ../mudlib/sys/wizlist.h wiz_list.h typedefs.h driver.h \
    smalloc.h strfuns.h sent.h instrs.h port.h config.h exec.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

xalloc.o : xalloc.c smalloc.c simulate.h main.h interpret.h xalloc.h \
    driver.h ../mudlib/sys/debug_info.h object.h exec.h svalue.h gcollect.h \
    backend.h smalloc.h typedefs.h strfuns.h sent.h instrs.h port.h \
    config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

# --- DO NOT MODIFY THIS LINE -- AUTO-DEPENDS PRECEDE ---
# --- DO NOT MODIFY THIS LINE -- SELECTED AUTO-DEPENDS FOLLOW ---
access_check.o : access_check.c instrs.h

actions.o : actions.c instrs.h

array.o : array.c instrs.h

backend.o : backend.c stdstrings.h instrs.h

call_out.o : call_out.c instrs.h

closure.o : closure.c stdstrings.h lang.h instrs.h

comm.o : comm.c stdstrings.h instrs.h

dumpstat.o : dumpstat.c instrs.h

ed.o : ed.c stdstrings.h instrs.h

efuns.o : efuns.c stdstrings.h instrs.h

gcollect.o : gcollect.c stdstrings.h instrs.h

heartbeat.o : heartbeat.c instrs.h

interpret.o : interpret.c stdstrings.h instrs.h

lex.o : lex.c efun_defs.c stdstrings.h lang.h instrs.h

main.o : main.c stdstrings.h instrs.h

make_func_main.o : make_func_main.c make_func.c

mapping.o : mapping.c instrs.h

mempools.o : mempools.c instrs.h

object.o : object.c stdstrings.h lang.h instrs.h

otable.o : otable.c instrs.h

parse.o : parse.c stdstrings.h instrs.h

parse_old.o : parse_old.c stdstrings.h instrs.h

parser.o : parser.c lang.c stdstrings.h instrs.h

pkg-mccp.o : pkg-mccp.c instrs.h

pkg-mysql.o : pkg-mysql.c instrs.h

pkg-tls.o : pkg-tls.c instrs.h

ptrtable.o : ptrtable.c instrs.h

regexp.o : regexp.c instrs.h

simul_efun.o : simul_efun.c stdstrings.h instrs.h

simulate.o : simulate.c stdstrings.h instrs.h

sprintf.o : sprintf.c stdstrings.h instrs.h

stdstrings.o : stdstrings.c stdstrings.h

stralloc.o : stralloc.c stdstrings.h instrs.h

strfuns.o : strfuns.c instrs.h

swap.o : swap.c instrs.h

wiz_list.o : wiz_list.c stdstrings.h instrs.h

xalloc.o : xalloc.c instrs.h

# --- DO NOT MODIFY THIS LINE -- SELECTED AUTO-DEPENDS PRECEDE ---
