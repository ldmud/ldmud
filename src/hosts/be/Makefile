#-------------------------------------------------------
# LDMud - Makefile for BeOS
#-------------------------------------------------------
#
# Usage: make [CPU=ppc|x86] [O=<opt-spec>]
#             [W=few|norm|all]
#             [S=yes|no]
#             [D=<flags>]
#             [<target>]
#
# The variable CPU determines the platform for which
# the program is compiled. If left unspecified, the
# executing platform is targeted. The created objects
# and binaries are located in subdirectories name
# 'obj.<CPU>'.
#
# The variable O determines the optimization, the value
# <opt-spec> is passed directly to the compiler. If the
# variable is not specified, '1' is used as opt-spec.
#
# The variable W determines the amount of warning.
# 'few' (the default) limits the warnings to those the
# yet unmodified source can pass. 'norm' are full warnings,
# except for those parts surrounded by pragmas. 'all' warnings
# are full warnings.
#
# If S is given and set to 'yes', the compiler runs in strict
# ansi mode, which also turns off inlines.
#
# If D is given, the <flags> are passed to the compiler on the
# commandline.
#
# <target> is one of the following targets:
#
#  driver:     create executable for the selected platform
#  all:        create executables for all platforms
#  clean:      delete all object files of the selected platform
#                and all unmodified source files.
#  cleanall:   delete all object files and all unmodified
#                source files.
#  clobber:    delete the object/binary subdirectory
#                for the selected platform
#  clobberall: delete all object/binary subdirectories.
#  depend:     update the dependency information in this file
#  dependall:  update the dependency in all known Makefiles.
#
# If no <target> is given, 'driver' is assumed.
#--------------------------------------------------------

#--------------------------------------------------------
# Application Specific Information
#--------------------------------------------------------

# The name of the application.
    IMG_NAME = driver

# The malloc to use:
#    smalloc: Satoria's malloc. Uses little memory; with FAST_FIT it
#             is also one of the fastest. Required for garbage collection.
#    sysmalloc: standard malloc.
    MALLOC=smalloc

# General mud directory, by default containing all other directories.
    prefix= /boot/home/mud

# The directory the mudlib usually is in.
    MUDLIB = ${prefix}/lib

# The directory where the executables are installed. The default
# erq path starts there.
    BINDIR = ${prefix}/bin

# The directory containing all executables which can be executed
# by the erq. Only safe programs belong here!
    ERQDIR = ${prefix}/erq

# Debugging options:
#   -DDEBUG: Enable run time debugging. It will use more time and space.
#            Nevertheless you are strongly encouraged to keep it defined.
#   -DYYDEBUG[=1]: Debug output for the LPC compiler.
#   -DTRACE_CODE: Enable the LPC code tracing.
    DEBUG = -DDEBUG -DTRACE_CODE

# Typical profiling, warning and optimizing options.
#   -DOPCPROF: enable usage statistics of VM instructions
#   -DVERBOSE_OPCPROF: with OPCPROF, the dump of the statistics includes
#              the names of the instructions
#   -p -DMARK: together they enable the profiling of VM instructions.
    PROFIL =
    WARN =
ifndef O
    OPTIMIZE = -O1
else
    OPTIMIZE = -O${O}
endif

# The yacc used and the typically generated parser files
# Note that bison is not yacc compatible enough.
    YACC = byacc
    YACCTAB = y.tab.

# The C source files.
# Though lang.c is listed, it is in fact generated from prolang.y
    SRCS = access_check.c actions.c array.c backend.c call_out.c closure.c \
           comm.c dumpstat.c ed.c efuns.c gcollect.c hash.c heartbeat.c \
           interpret.c lang.c lex.c main.c mapping.c object.c otable.c \
           parse.c parse_old.c port.c ptrtable.c \
           random.c regexp.c rxcache.c simulate.c simul_efun.c stralloc.c \
           strfuns.c sprintf.c swap.c wiz_list.c $(MALLOC).c

# Initialize the CFLAGS with the values above
    CFLAGS += $(OPTIMIZE) $(DEBUG) -DMALLOC_$(MALLOC) $(WARN) $(PROFIL)
    CFLAGS += -DMUD_LIB='"$(MUDLIB)"' -DBINDIR='"$(BINDIR)"'
    CFLAGS += -DERQ_DIR='"$(ERQDIR)"'

# Add additional flags if given

ifdef D
    CFLAGS += ${D}
endif

# The following variables are used to describe the version
# of the driver binary.

# $Format: "SETVERFLAGS = -app $ReleaseMajor$ $ReleaseMinor$ $ReleaseMicro$"$
SETVERFLAGS = -app 3 2 7

# $Format: "ifeq ($ReleaseType$, rel)"$
ifeq (rel, rel)
# $Format: "  SETVERFLAGS += f"$
  SETVERFLAGS += f
# $Format: "  VSTRING = $ReleaseMajor$.$ReleaseMinor$.$ReleaseMicro$"$
  VSTRING = 3.2.7
endif
# $Format: "ifeq ($ReleaseType$, pre)"$
ifeq (rel, pre)
# $Format: "  SETVERFLAGS += d $ProjectMinorVersion$"$
  SETVERFLAGS += d 7
# $Format: "  VSTRING = $ReleaseMajor$.$ReleaseMinor$.$ReleaseMicro$-pre.$ProjectMinorVersion$"$
  VSTRING = 3.2.7-pre.7
endif
# $Format: "ifeq ($ReleaseType$, dev)"$
ifeq (rel, dev)
# $Format: "  SETVERFLAGS += d $ProjectMinorVersion$"$
  SETVERFLAGS += d 7
# $Format: "  VSTRING = $ReleaseMajor$.$ReleaseMinor$.$ReleaseMicro$-dev.$ProjectMinorVersion$"$
  VSTRING = 3.2.7-dev.7
endif

SETVERFLAGS += -short $(VSTRING) -long "LDMud $(VSTRING) - a LPMud Gamedriver."

#--------------------------------------------------------
# Application Independent Information
#--------------------------------------------------------

# determine the CPU if not specified on the command line
# set MACHINE to the host machine
# set NATIVE to the host cpu
# set CROSS to the non-host cpu
    MACHINE =$(shell uname -m)
ifndef CPU
ifeq ($(MACHINE), BePC)
    CPU = x86
else
    CPU = ppc
endif
endif
ifeq ($(MACHINE), BePC)
    NATIVE = x86
    CROSS = ppc
else
    NATIVE = ppc
    CROSS = x86
endif

# set the object directory
    OBJ	    := obj.$(CPU)
    OBJ_NATIVE := obj.$(NATIVE)
    OBJ_CROSS := obj.$(CROSS)

# specify where to create the application binary
    TARGET :=$(OBJ)/$(IMG_NAME)

# The resource file, with and without path
    RSRC      = $(IMG_NAME).rsrc
    RSRC_FULL = hosts/be/$(RSRC)

# Tools
    MIMESET = mimeset
    MWBRES  = mwbres
    XRES    = xres

# platform specific settings
ifeq ($(CPU), x86)
ifeq ($(NATIVE), x86)
    CC = gcc
    LD = gcc
else
    CC = /boot/develop/tools/gnupro/bin/i586-beos-gcc
    LD = /boot/develop/tools/gnupro/bin/i586-beos-gcc
endif

    CFLAGS += -g -Wall -Wshadow -Wno-parentheses -Winline

ifeq ($(S), yes)
    CFLAGS += -ansi -DNO_INLINES
    # *sigh* -pedantic chokes on Be's include files
endif

    LDFLAGS += -Xlinker -soname=_APP_
endif

ifeq ($(CPU), ppc)
ifeq ($(NATIVE), ppc)
    CC = mwcc$(CPU)

    CFLAGS += -g -relax_pointers -r

ifndef W
    CFLAGS += -w8
else
ifeq ($(W), few)
    CFLAGS += -w8
else
ifeq ($(W), all)
    CFLAGS += -DWARN_ALL -w9 -w=notinlined
else
    CFLAGS += -w9 -w=nonotinlined
endif
endif
endif

ifeq ($(S), yes)
    CFLAGS += -ansi strict -DNO_INLINES
endif

    # TODO: relax_pointers should go
    LD = mwld$(CPU)
    LDFLAGS += -sym full -xma  -map $(MAP_FILE) -export pragma \
               -init _init_routine_ -term _term_routine_

    # generate mapfiles for metrowerks.
    MAP_FILE	:= $(TARGET).xMAP
    SYMBOL_FILE	:= $(TARGET).xSYM

else
    CC = echo Cross-compilation is not supported!; exit 2;
    LD = $(CC)
endif
endif


# pseudo-function for converting a list of source files in SRCS variable
# to a corresponding list of object files in $(OBJ)/xxx.o
# The "function" strips off the src file suffix (.ccp or .c or whatever)
# and then strips of the directory name, leaving just the root file name.
# It then appends the .o suffix and prepends the $(OBJ)/ path
define SRCS_LIST_TO_OBJS
	$(addprefix $(OBJ)/, $(addsuffix .o, $(foreach file, $(SRCS), $(basename $(notdir $(file))))))
endef

# specify the list of objects
    OBJS := $(SRCS_LIST_TO_OBJS)

#-------------------------------------------------------
# Targets and their rules
#-------------------------------------------------------

# define the actual work to be done	
default: $(TARGET)
$(IMG_NAME): $(TARGET)

all: $(TARGET)
	@make CPU=$(CROSS) $(IMG_NAME)

$(TARGET): $(OBJ) depend $(OBJS) $(RSRC_FULL)
	$(LD) -o $@ $(OBJS) $(LDFLAGS)
ifeq ($(NATIVE), $(CPU))
	$(XRES) -o $@ $(RSRC_FULL)
endif
	$(MIMESET) -f $@
	setversion $@ $(SETVERFLAGS)

clean :: FORCE
	-rm -f $(YACCTAB)h $(YACCTAB)c make_func.c $(OBJ)/make_func
	-rm -f *~ efun_defs.c instrs.h lang.y lang.h lang.c y.output tags TAGS
	-rm -f hosts/*~ hosts/*/*~ bugs/*~ done/*~ $(OBJ)/*.o
	-rm -f wk/*~ settings/*~

cleanall :: clean
	-rm -f $(OBJ_CROSS)/*.o

clobber :: clean FORCE
	-rm -rf $(OBJ) config.cache config.status config.log

clobberall :: clobber cleanall
	-rm -rf $(OBJ_CROSS)

depend: $(SRCS) make_func.c $(OBJ)
	@$(SHELL) -ec "if type mkdepend > /dev/null 2>&1; then \
          echo Updating dependencies.; \
          mkdepend $(SRCS) make_func.c -I. -x efun_defs.c -p .c:$$\(OBJ\)/%n.o -fMakefile; \
          mkdepend $(SRCS) make_func.c -I. -x efun_defs.c -p .c:$$\(OBJ\)/%n.o -fhosts/be/Makefile; \
        else\
          echo mkdepend utility not available.; \
        fi"

.PHONY dependall: depend depend-generic depend-amiga

depend-generic: $(SRCS) make_func.c $(OBJ)
	@$(SHELL) -ec "if type mkdepend > /dev/null 2>&1; then \
          echo Updating dependencies in Makefile.in.; \
          mkdepend $(SRCS) make_func.c -I. -x efun_defs.c -p .c:%n.o -fMakefile.in; \
          echo Updating dependencies in hosts/os2/Makefile.; \
          mkdepend $(SRCS) make_func.c -I. -x efun_defs.c -p .c:%n.o -fhosts/os2/Makefile; \
        else\
          echo mkdepend utility not available.; \
        fi"

AMIGASRCS:= $(addprefix hosts/amiga/, amiga.c signal.c signal_rr.c socket.c ixfile.c)

depend-amiga: $(SRCS) make_func.c $(AMIGASRCS)  $(OBJ)
	@$(SHELL) -ec "if type mkdepend > /dev/null 2>&1; then \
          echo Updating dependencies in hosts/amiga/DMakefile.; \
          mkdepend $(SRCS) make_func.c $(AMIGASRCS) -I. -x efun_defs.c -p .c+:%n.o -Ihosts/amiga -fhosts/amiga/DMakefile; \
          echo Updating dependencies in hosts/amiga/Makefile.; \
          mkdepend $(SRCS) make_func.c $(AMIGASRCS) -I. -x efun_defs.c -p .c+:%n.o -Ihosts/amiga -fhosts/amiga/Makefile; \
        else\
          echo mkdepend utility not available.; \
        fi"

# Special rules for making make_func, depending on whether we're
# crosscompiling or not.

make_func.c : make_func.y
	$(YACC) make_func.y
	mv $(YACCTAB)c make_func.c

$(OBJ)/make_func.o : make_func.c driver.h config.h machine.h port.h
	$(CC) $(INCLUDES) $(CFLAGS) -DYACC='"$(YACC)"' -c $< -o $@

$(OBJ)/make_func : $(OBJ)/make_func.o $(OBJ)/hash.o
	$(LD) -o $@ $^ $(LDFLAGS)
	$(MIMESET) -f $@

ifneq ($(CPU), $(NATIVE))
$(OBJ_NATIVE)/make_func :
	@[ -d $(OBJ_NATIVE) ] || mkdir $(OBJ_NATIVE) > /dev/null 2>&1
	make CPU=$(NATIVE) $@
endif

# The making of the compiler and associated files.

lang.y efun_defs.c instrs.h : func_spec prolang.y config.h $(OBJ_NATIVE)/make_func
	rm -f efun_defs.c lang.y
	$(OBJ_NATIVE)/make_func

lang.c lang.h : lang.y
	$(YACC) -d -v lang.y
	mv $(YACCTAB)c lang.c
	mv $(YACCTAB)h lang.h

# The making of the mallocator.

$(OBJ)/$(MALLOC).o : $(MALLOC).c config.h driver.h machine.h port.h
	rm -f $(OBJ)/smalloc.o $(OBJ)/sysmalloc.o
	$(CC) $(INCLUDES) $(CFLAGS) -c $< -o $@

# Be Resource Mangling

$(RSRC_FULL) : hosts/be/driver.r hosts/be/icon-32x32.raw hosts/be/icon-16x16.raw
	cd hosts/be; $(MWBRES) -o $(RSRC) driver.r

#--------------------------------------------------------
#	Rules for the whole system
#--------------------------------------------------------

# rule to create the object file directory if needed
$(OBJ)::
	@[ -d $(OBJ) ] || mkdir $(OBJ) > /dev/null 2>&1

# Don't stumble over non-existing include files.
%.h :
	@echo "Warning: Can't find $@."

# default rule for xxx.c files to compile into $(OBJ)/xxx.o
$(OBJ)/%.o : %.c
	$(CC) $(INCLUDES) $(CFLAGS) -c $< -o $@

# empty rule. Things that depend on this rule will always get triggered
FORCE:

# remove just the application from the object folder
rmapp ::
	-rm -f $(TARGET)

#--------------------------------------------------------
# Dependencies, manual and automatic.

$(OBJ)/array.o : instrs.h lang.h

$(OBJ)/closure.o : instrs.h lang.h

$(OBJ)/dumpstat.o : instrs.h lang.h

$(OBJ)/gcollect.o : instrs.h lang.h

$(OBJ)/interpret.o : instrs.h lang.h

$(OBJ)/lang.o : driver.h config.h machine.h lex.h interpret.h object.h \
    instrs.h port.h switch.h stralloc.h

$(OBJ)/lex.o : instrs.h lang.h efun_defs.c

$(OBJ)/simul_efun.o : instrs.h lang.h

$(OBJ)/simulate.o : instrs.h

$(OBJ)/sprintf.o : instrs.h

# --- DO NOT MODIFY THIS LINE -- AUTO-DEPENDS FOLLOW ---
$(OBJ)/access_check.o : filestat.h comm.h access_check.h driver.h sent.h \
    object.h interpret.h config.h port.h exec.h instrs.h datatypes.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

$(OBJ)/actions.o : wiz_list.h stralloc.h smalloc.h simulate.h sent.h \
    object.h mapping.h interpret.h dumpstat.h datatypes.h comm.h closure.h \
    backend.h array.h actions.h driver.h strfuns.h instrs.h exec.h main.h \
    port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

$(OBJ)/array.o : wiz_list.h swap.h stralloc.h simulate.h rxcache.h regexp.h \
    prolang.h object.h mapping.h main.h interpret.h instrs.h exec.h \
    datatypes.h backend.h array.h my-alloca.h driver.h smalloc.h strfuns.h \
    sent.h lang.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

$(OBJ)/backend.o : wiz_list.h swap.h stralloc.h smalloc.h simulate.h \
    rxcache.h regexp.h random.h otable.h object.h my-alloca.h mapping.h \
    main.h lex.h interpret.h heartbeat.h gcollect.h filestat.h exec.h ed.h \
    comm.h closure.h call_out.h array.h actions.h backend.h \
    hosts/amiga/nsignal.h driver.h strfuns.h datatypes.h sent.h instrs.h \
    port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

$(OBJ)/call_out.o : wiz_list.h swap.h strfuns.h stralloc.h simulate.h \
    object.h main.h interpret.h gcollect.h exec.h comm.h closure.h \
    backend.h array.h actions.h call_out.h driver.h datatypes.h smalloc.h \
    sent.h instrs.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

$(OBJ)/closure.o : switch.h simul_efun.h simulate.h stralloc.h prolang.h \
    object.h main.h lex.h instrs.h interpret.h exec.h backend.h array.h \
    closure.h my-alloca.h driver.h ptrtable.h strfuns.h sent.h smalloc.h \
    lang.h datatypes.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

$(OBJ)/comm.o : util/erq/erq.h wiz_list.h stralloc.h simulate.h sent.h \
    object.h main.h interpret.h gcollect.h filestat.h exec.h ed.h closure.h \
    backend.h array.h actions.h access_check.h comm.h hosts/amiga/nsignal.h \
    telnet.h my-alloca.h driver.h smalloc.h strfuns.h instrs.h datatypes.h \
    config.h port.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

$(OBJ)/dumpstat.o : smalloc.h simulate.h ptrtable.h prolang.h object.h \
    mapping.h instrs.h interpret.h filestat.h exec.h comm.h closure.h \
    array.h dumpstat.h driver.h strfuns.h datatypes.h sent.h lang.h port.h \
    config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

$(OBJ)/ed.o : stralloc.h simulate.h rxcache.h regexp.h object.h main.h \
    interpret.h gcollect.h filestat.h comm.h closure.h actions.h ed.h \
    driver.h smalloc.h strfuns.h sent.h instrs.h exec.h datatypes.h \
    backend.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

$(OBJ)/efuns.o : ../mudlib/sys/strings.h ../mudlib/sys/objectinfo.h \
    ../mudlib/sys/debug_info.h swap.h strfuns.h stralloc.h smalloc.h \
    simulate.h ptrtable.h mapping.h main.h prolang.h instrs.h interpret.h \
    dumpstat.h datatypes.h comm.h closure.h array.h efuns.h driver.h \
    object.h exec.h sent.h lang.h port.h config.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

$(OBJ)/gcollect.o : wiz_list.h swap.h stralloc.h smalloc.h simul_efun.h \
    simulate.h sent.h rxcache.h prolang.h otable.h object.h mapping.h \
    main.h lex.h instrs.h interpret.h heartbeat.h filestat.h exec.h ed.h \
    comm.h closure.h call_out.h backend.h array.h gcollect.h driver.h \
    strfuns.h datatypes.h ptrtable.h regexp.h lang.h port.h config.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

$(OBJ)/hash.o : hash.h

$(OBJ)/heartbeat.o : wiz_list.h strfuns.h simulate.h sent.h object.h \
    gcollect.h datatypes.h comm.h backend.h array.h actions.h heartbeat.h \
    driver.h interpret.h instrs.h exec.h main.h smalloc.h port.h config.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

$(OBJ)/interpret.o : ../mudlib/sys/trace.h wiz_list.h switch.h swap.h \
    sprintf.h smalloc.h stralloc.h simul_efun.h simulate.h sent.h random.h \
    ptrtable.h prolang.h parse.h otable.h object.h mapping.h main.h lex.h \
    instrs.h heartbeat.h gcollect.h filestat.h exec.h efuns.h ed.h comm.h \
    closure.h call_out.h backend.h array.h actions.h interpret.h \
    my-rusage.h my-alloca.h driver.h strfuns.h datatypes.h lang.h port.h \
    config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

$(OBJ)/lang.o : wiz_list.h switch.h swap.h stralloc.h simul_efun.h \
    simulate.h object.h mapping.h main.h lex.h instrs.h interpret.h \
    gcollect.h exec.h closure.h backend.h array.h prolang.h my-alloca.h \
    driver.h smalloc.h strfuns.h ptrtable.h sent.h datatypes.h lang.h \
    port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

$(OBJ)/lex.o : efun_defs.c hosts/amiga/socket.h strfuns.h stralloc.h \
    simul_efun.h simulate.h prolang.h patchlevel.h main.h instrs.h hash.h \
    gcollect.h filestat.h exec.h comm.h closure.h backend.h array.h lex.h \
    my-alloca.h driver.h ./hosts/amiga/socket_sim.h \
    ./hosts/amiga/socket_tcp.h datatypes.h smalloc.h ptrtable.h object.h \
    interpret.h sent.h lang.h port.h config.h \
    ./hosts/amiga/socket_sim_protos.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

$(OBJ)/main.o : wiz_list.h swap.h stralloc.h simul_efun.h simulate.h \
    rxcache.h random.h patchlevel.h otable.h object.h mapping.h lex.h \
    interpret.h gcollect.h filestat.h comm.h array.h backend.h main.h \
    hosts/amiga/socket.h my-alloca.h driver.h exec.h smalloc.h strfuns.h \
    ptrtable.h sent.h instrs.h regexp.h datatypes.h \
    ./hosts/amiga/socket_sim.h ./hosts/amiga/socket_tcp.h port.h config.h \
    ./hosts/amiga/socket_sim_protos.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

$(OBJ)/make_func.o : hash.h exec.h my-alloca.h driver.h datatypes.h port.h \
    config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

$(OBJ)/mapping.o : wiz_list.h stralloc.h smalloc.h simulate.h regexp.h \
    object.h main.h interpret.h gcollect.h datatypes.h closure.h backend.h \
    array.h mapping.h my-alloca.h driver.h strfuns.h sent.h instrs.h exec.h \
    port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

$(OBJ)/object.o : wiz_list.h swap.h stralloc.h simulate.h sent.h random.h \
    ptrtable.h prolang.h otable.h mapping.h main.h interpret.h filestat.h \
    exec.h comm.h closure.h backend.h array.h actions.h object.h \
    my-alloca.h driver.h smalloc.h strfuns.h instrs.h lang.h datatypes.h \
    port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

$(OBJ)/otable.o : simulate.h strfuns.h object.h interpret.h hash.h \
    gcollect.h backend.h otable.h driver.h sent.h instrs.h datatypes.h \
    exec.h main.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

$(OBJ)/parse.o : wiz_list.h stralloc.h simulate.h object.h main.h instrs.h \
    interpret.h closure.h array.h actions.h parse.h driver.h smalloc.h \
    strfuns.h sent.h exec.h datatypes.h backend.h port.h config.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

$(OBJ)/parse_old.o : wiz_list.h simulate.h random.h object.h main.h \
    interpret.h array.h actions.h parse.h driver.h strfuns.h sent.h \
    instrs.h exec.h datatypes.h smalloc.h backend.h port.h config.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

$(OBJ)/port.o : hosts/crypt.c main.h backend.h my-rusage.h driver.h \
    interpret.h object.h datatypes.h port.h config.h instrs.h sent.h exec.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

$(OBJ)/ptrtable.o : simulate.h ptrtable.h driver.h strfuns.h sent.h \
    object.h instrs.h interpret.h port.h config.h datatypes.h exec.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

$(OBJ)/random.o : random.h driver.h port.h config.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

$(OBJ)/regexp.o : regexp.h driver.h port.h config.h hosts/unix.h \
    hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

$(OBJ)/rxcache.o : strfuns.h stralloc.h smalloc.h regexp.h hash.h \
    gcollect.h rxcache.h driver.h datatypes.h object.h interpret.h exec.h \
    port.h config.h sent.h instrs.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

$(OBJ)/simul_efun.o : swap.h stralloc.h simulate.h prolang.h parse.h \
    object.h lex.h instrs.h interpret.h gcollect.h exec.h array.h \
    simul_efun.h my-alloca.h driver.h smalloc.h strfuns.h sent.h lang.h \
    datatypes.h ptrtable.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

$(OBJ)/simulate.o : ../mudlib/sys/rtlimits.h wiz_list.h swap.h strfuns.h \
    stralloc.h simul_efun.h sent.h rxcache.h prolang.h otable.h object.h \
    mapping.h main.h lex.h instrs.h interpret.h heartbeat.h filestat.h \
    exec.h ed.h dumpstat.h comm.h closure.h call_out.h backend.h array.h \
    actions.h simulate.h hosts/amiga/nsignal.h my-alloca.h driver.h \
    datatypes.h smalloc.h ptrtable.h regexp.h lang.h port.h config.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

$(OBJ)/smalloc.o : object.h exec.h simulate.h main.h gcollect.h backend.h \
    comm.h smalloc.h driver.h sent.h interpret.h datatypes.h strfuns.h \
    instrs.h port.h config.h hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h \
    hosts/amiga/patchfloat.h machine.h ./hosts/amiga/ixfile.h

$(OBJ)/sprintf.o : swap.h stralloc.h simul_efun.h simulate.h sent.h \
    prolang.h object.h mapping.h main.h instrs.h interpret.h exec.h \
    closure.h array.h sprintf.h driver.h smalloc.h strfuns.h ptrtable.h \
    lang.h datatypes.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

$(OBJ)/stralloc.o : strfuns.h simulate.h hash.h gcollect.h stralloc.h \
    driver.h datatypes.h sent.h object.h instrs.h interpret.h exec.h \
    smalloc.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

$(OBJ)/strfuns.o : smalloc.h simulate.h datatypes.h comm.h strfuns.h \
    driver.h sent.h object.h instrs.h interpret.h port.h config.h exec.h \
    hosts/unix.h hosts/be/be.h hosts/amiga/amiga.h hosts/amiga/patchfloat.h \
    machine.h ./hosts/amiga/ixfile.h

$(OBJ)/swap.o : hosts/amiga/socket.h wiz_list.h stralloc.h simul_efun.h \
    simulate.h prolang.h random.h otable.h object.h mapping.h main.h \
    interpret.h gcollect.h exec.h comm.h backend.h array.h swap.h driver.h \
    ./hosts/amiga/socket_sim.h ./hosts/amiga/socket_tcp.h smalloc.h \
    strfuns.h ptrtable.h sent.h instrs.h lang.h datatypes.h port.h config.h \
    ./hosts/amiga/socket_sim_protos.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

$(OBJ)/wiz_list.o : stralloc.h object.h main.h interpret.h gcollect.h \
    backend.h array.h wiz_list.h driver.h smalloc.h strfuns.h sent.h exec.h \
    instrs.h datatypes.h port.h config.h hosts/unix.h hosts/be/be.h \
    hosts/amiga/amiga.h hosts/amiga/patchfloat.h machine.h \
    ./hosts/amiga/ixfile.h

# --- DO NOT MODIFY THIS LINE -- AUTO-DEPENDS PRECEDE ---
