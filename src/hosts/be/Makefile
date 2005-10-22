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
#   -DNO_INLINES: Disable inlining
#   -DDEBUG_TELNET: Activates debug prints in the telnet machine
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
    YACC = hosts/be/beyacc
    YACCTAB = y.tab.

# The C source files.
    SRCS = access_check.c actions.c array.c backend.c bitstrings.c \
           call_out.c closure.c \
           comm.c dumpstat.c ed.c efuns.c files.c gcollect.c hash.c \
           heartbeat.c \
           interpret.c lex.c main.c mapping.c md5.c mempools.c mstrings.c \
           object.c otable.c parser.c parse.c parse_old.c \
           pkg-alists.c pkg-mysql.c port.c \
           ptrtable.c random.c \
           regexp.c rxcache.c simulate.c simul_efun.c stdstrings.c \
           strfuns.c sprintf.c swap.c wiz_list.c xalloc.c \
           pcre-get.c pcre-maketables.c pcre-pcre.c pcre-study.c

# Generated source files (overlaps with of SRCS) which need to be
# present for mkdepend to work.
    GENSRCS = make_func.c stdstrings.c lang.c instrs.h pcre/chartables.c

# Macros for MkDepend:
    SKELETON = $(SRCS) mkfunc.c pcre/dftables.c
    EXCEPT = -x efun_defs.c
    SELECT = -S instrs.h -S stdstrings.h -S make_func.c -S lang.c -S lang.h -S efun_defs.c

# Initialize the CFLAGS with the values above
    CFLAGS += $(OPTIMIZE) $(DEBUG) $(WARN) $(PROFIL)
    CFLAGS += -DMUD_LIB='"$(MUDLIB)"' -DBINDIR='"$(BINDIR)"'
    CFLAGS += -DERQ_DIR='"$(ERQDIR)"'

# Add additional flags if given

ifdef D
    CFLAGS += ${D}
endif

# The following variables are used to describe the version
# of the driver binary.

# $Format: "ifeq (\$(shell echo \$\$(($ReleaseMinor$ / 2))), 0)"$
ifeq ($(shell echo $$((3 / 2))), 0)

# --- Stable Versions ---

# $Format: "SETVERFLAGS = -app $ReleaseMajor$ $ReleaseMinor$ $ReleaseMicro$"$
SETVERFLAGS = -app 3 3 0

# $Format: "ifeq ($ReleaseType$, rel)"$
ifeq (dev, rel)
# $Format: "  SETVERFLAGS += f"$
  SETVERFLAGS += f
# $Format: "  VSTRING = $ReleaseMajor$.$ReleaseMinor$.$ReleaseMicro$"$
  VSTRING = 3.3.0
endif
# $Format: "ifeq ($ReleaseType$, pre)"$
ifeq (dev, pre)
# $Format: "  SETVERFLAGS += d $ProjectMinorVersion$"$
  SETVERFLAGS += d 184
# $Format: "  VSTRING = $ReleaseMajor$.$ReleaseMinor$.$ReleaseMicro$-pre.$ProjectMinorVersion$"$
  VSTRING = 3.3.0-pre.184
endif
# $Format: "ifeq ($ReleaseType$, dev)"$
ifeq (dev, dev)
# $Format: "  SETVERFLAGS += d $ProjectMinorVersion$"$
  SETVERFLAGS += d 184
# $Format: "  VSTRING = $ReleaseMajor$.$ReleaseMinor$.$ReleaseMicro$-dev.$ProjectMinorVersion$"$
  VSTRING = 3.3.0-dev.184
endif

else

# --- Development Versions ---

# $Format: "SETVERFLAGS = -app $ReleaseMajor$ $ReleaseMinor$ $ProjectMinorVersion$"$
SETVERFLAGS = -app 3 3 184

# $Format: "  SETVERFLAGS += f"$
  SETVERFLAGS += f
# $Format: "  VSTRING = $ReleaseMajor$.$ReleaseMinor$.$ProjectMinorVersion$"$
  VSTRING = 3.3.184

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

# The targets 'BEOS:TYPE' resource - empty as default for native compiles
    MIMS      =

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
    MIMS = -a MIMS:2:BEOS:TYPE -s 'application/x-vnd.Be-elfexecutable'
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
    LD = mwld$(CPU)
else
    CC = /boot/develop_ppc/bin/mwccppc
    LD = /boot/develop_ppc/bin/mwldppc

    LDFLAGS += /boot/develop_ppc/lib/ppc/start_dyn.o \
               /boot/develop_ppc/lib/ppc/glue-noinit.a \
               /boot/develop_ppc/lib/ppc/init_term_dyn.o \
               -L/boot/develop_ppc/lib/ppc \
               -lroot -lnet

    MIMS = -a MIMS:2:BEOS:TYPE -s 'application/x-be-executable'
endif

    # generate mapfiles for metrowerks.
    MAP_FILE	:= $(TARGET).xMAP
    SYMBOL_FILE	:= $(TARGET).xSYM

    CFLAGS += -g -r

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

    LDFLAGS += -sym full -map $(MAP_FILE) -export pragma \
               -init _init_routine_ -term _term_routine_ 

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
	$(XRES) -o $@ $(MIMS) $(RSRC_FULL)
	$(MIMESET) -f $@
	setversion $@ $(SETVERFLAGS)
ifneq ($(CPU), $(NATIVE))
	[ -f $(OBJ_NATIVE)/$(IMG_NAME) ] && copyattr -n 'BEOS:M:STD_ICON' $(OBJ_NATIVE)/$(IMG_NAME) $@
	[ -f $(OBJ_NATIVE)/$(IMG_NAME) ] && copyattr -n 'BEOS:L:STD_ICON' $(OBJ_NATIVE)/$(IMG_NAME) $@
endif

clean :: FORCE
	-rm -f $(YACCTAB)h $(YACCTAB)c make_func.c $(OBJ)/mkfunc
	-rm -f $(OBJ)/dftables pcre/chartables.c
	-rm -f *~ efun_defs.c instrs.h lang.y lang.h lang.c y.output tags TAGS
	-rm -f stdstrings.c stdstrings.h
	-rm -f hosts/*~ hosts/*/*~ bugs/*~ done/*~ $(OBJ)/*.o
	-rm -f pcre/*~ wk/*~ settings/*~

cleanall :: clean
	-rm -f $(OBJ_CROSS)/*.o

clobber :: clean FORCE
	-rm -rf $(OBJ) config.cache config.status config.log

clobberall :: clobber cleanall
	-rm -rf $(OBJ_CROSS)

depend: $(SRCS) $(GENSRCS) $(OBJ)
	@$(SHELL) -ec "if type mkdepend > /dev/null 2>&1; then \
          echo Updating dependencies.; \
          mkdepend $(SKELETON) -I. $(EXCEPT) -m -p .c:$$\(OBJ\)/%n.o -fMakefile; \
          mkdepend $(SKELETON) -I. $(EXCEPT) $(SELECT) -m -p .c:$$\(OBJ\)/%n.o -fMakefile; \
          mkdepend $(SKELETON) -I. $(EXCEPT) -m -p .c:$$\(OBJ\)/%n.o -fhosts/be/Makefile; \
          mkdepend $(SKELETON) -I. $(EXCEPT) $(SELECT) -m -p .c:$$\(OBJ\)/%n.o -fhosts/be/Makefile; \
        else\
          echo mkdepend utility not available.; \
        fi"

.PHONY dependall: depend depend-generic

depend-generic: $(SRCS) $(GENSRCS) $(OBJ)
	@$(SHELL) -ec "if type mkdepend > /dev/null 2>&1; then \
          echo Updating dependencies in Makefile.in.; \
          mkdepend $(SKELETON) -I. $(EXCEPT) -m -p .c:%n.o -fMakefile.in; \
          mkdepend $(SKELETON) $(SELECT) -I. $(EXCEPT) -m -p .c:%n.o -fMakefile.in; \
          echo Updating dependencies in hosts/os2/Makefile.; \
          mkdepend $(SKELETON) -I. $(EXCEPT) -m -p .c:%n.o -fhosts/os2/Makefile; \
          mkdepend $(SKELETON) $(SELECT) -I. $(EXCEPT) -m -p .c:%n.o -fhosts/os2/Makefile; \
        else\
          echo mkdepend utility not available.; \
        fi"

AMIGASRCS:= 

# Special rules for making mkfunc and dftables, depending on whether we're
# crosscompiling or not.

make_func.c : make_func.y
	$(YACC) make_func.y
	mv $(YACCTAB)c make_func.c

$(OBJ)/mkfunc.o : mkfunc.c make_func.c driver.h config.h machine.h port.h
	$(CC) $(INCLUDES) $(CFLAGS) -DYACC='"$(YACC)"' -c $< -o $@

$(OBJ)/mkfunc : $(OBJ)/mkfunc.o $(OBJ)/hash.o
	$(LD) -o $@ $^ $(LDFLAGS)
	$(MIMESET) -f $@

ifneq ($(CPU), $(NATIVE))
$(OBJ_NATIVE)/mkfunc :
	@[ -d $(OBJ_NATIVE) ] || mkdir $(OBJ_NATIVE) > /dev/null 2>&1
	make CPU=$(NATIVE) $@
endif

$(OBJ)/dftables.o : pcre/dftables.c pcre/maketables.c pcre/config.h
	$(CC) $(INCLUDES) $(CFLAGS) -c $< -o $@

$(OBJ)/dftables : $(OBJ)/dftables.o
	$(LD) -o $@ $^ $(LDFLAGS)
	$(MIMESET) -f $@

ifneq ($(CPU), $(NATIVE))
$(OBJ_NATIVE)/dftables :
	@[ -d $(OBJ_NATIVE) ] || mkdir $(OBJ_NATIVE) > /dev/null 2>&1
	make CPU=$(NATIVE) $@
endif

# The making of the compiler and associated files.

efun_defs.c instrs.h : func_spec config.h $(OBJ_NATIVE)/mkfunc
	-rm -f efun_defs.c instrs.h
	$(OBJ_NATIVE)/mkfunc instrs

stdstrings.c stdstrings.h: string_spec $(OBJ_NATIVE)/mkfunc config.h
	rm -f stdstrings.c stdstrings.h
	$(OBJ_NATIVE)/mkfunc strings

lang.y : prolang.y config.h $(OBJ_NATIVE)/mkfunc
	-rm -f lang.y
	$(OBJ_NATIVE)/mkfunc lang

lang.c lang.h : lang.y
	$(YACC) -d -v lang.y
	mv $(YACCTAB)c lang.c
	mv $(YACCTAB)h lang.h

pcre/chartables.c : $(OBJ_NATIVE)/dftables
	$(OBJ_NATIVE)/dftables > pcre/chartables.c

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

# --- DO NOT MODIFY THIS LINE -- AUTO-DEPENDS FOLLOW ---
$(OBJ)/access_check.o : xalloc.h filestat.h comm.h access_check.h driver.h \
    svalue.h simulate.h typedefs.h config.h port.h strfuns.h sent.h exec.h \
    hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/actions.o : ../mudlib/sys/driver_hook.h ../mudlib/sys/commands.h \
    xalloc.h wiz_list.h svalue.h simulate.h sent.h stdstrings.h object.h \
    mstrings.h mapping.h interpret.h exec.h efuns.h dumpstat.h comm.h \
    closure.h backend.h array.h actions.h typedefs.h driver.h strfuns.h \
    instrs.h main.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/array.o : xalloc.h wiz_list.h swap.h svalue.h simulate.h \
    stdstrings.h rxcache.h regexp.h object.h mstrings.h mempools.h \
    mapping.h main.h interpret.h closure.h backend.h array.h my-alloca.h \
    typedefs.h driver.h strfuns.h sent.h exec.h instrs.h port.h config.h \
    hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/backend.o : ../mudlib/sys/debug_message.h \
    ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h swap.h svalue.h \
    stdstrings.h simulate.h rxcache.h regexp.h random.h otable.h object.h \
    mstrings.h mapping.h main.h lex.h interpret.h heartbeat.h gcollect.h \
    filestat.h exec.h ed.h comm.h closure.h call_out.h array.h actions.h \
    backend.h my-alloca.h typedefs.h driver.h strfuns.h sent.h instrs.h \
    port.h config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/bitstrings.o : xalloc.h svalue.h simulate.h mstrings.h main.h lex.h \
    interpret.h bitstrings.h typedefs.h driver.h strfuns.h sent.h exec.h \
    instrs.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/call_out.o : ../mudlib/sys/debug_info.h xalloc.h wiz_list.h swap.h \
    svalue.h strfuns.h stdstrings.h simulate.h object.h mstrings.h main.h \
    interpret.h gcollect.h exec.h comm.h closure.h backend.h array.h \
    actions.h call_out.h typedefs.h driver.h sent.h instrs.h port.h \
    config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/closure.o : xalloc.h switch.h swap.h svalue.h stdstrings.h \
    simul_efun.h simulate.h prolang.h object.h mstrings.h main.h lex.h \
    lang.h interpret.h instrs.h exec.h backend.h array.h closure.h \
    my-alloca.h typedefs.h driver.h ptrtable.h strfuns.h sent.h port.h \
    config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/comm.o : util/erq/erq.h ../mudlib/sys/input_to.h \
    ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h swap.h svalue.h \
    stdstrings.h simulate.h sent.h object.h mstrings.h main.h interpret.h \
    gcollect.h filestat.h exec.h ed.h closure.h backend.h array.h actions.h \
    access_check.h comm.h telnet.h my-alloca.h typedefs.h driver.h \
    strfuns.h instrs.h config.h port.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/dumpstat.o : xalloc.h svalue.h smalloc.h stdstrings.h simulate.h \
    ptrtable.h object.h mstrings.h mapping.h filestat.h exec.h closure.h \
    array.h dumpstat.h typedefs.h driver.h strfuns.h sent.h instrs.h port.h \
    config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/ed.o : xalloc.h svalue.h stdstrings.h simulate.h rxcache.h object.h \
    mstrings.h main.h interpret.h gcollect.h filestat.h comm.h closure.h \
    actions.h ed.h typedefs.h driver.h strfuns.h sent.h exec.h regexp.h \
    instrs.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/efuns.o : ../mudlib/sys/time.h ../mudlib/sys/strings.h \
    ../mudlib/sys/objectinfo.h ../mudlib/sys/debug_info.h xalloc.h \
    wiz_list.h svalue.h swap.h strfuns.h smalloc.h simulate.h stdstrings.h \
    rxcache.h random.h ptrtable.h otable.h object.h mstrings.h md5.h \
    mapping.h main.h interpret.h instrs.h heartbeat.h dumpstat.h comm.h \
    closure.h call_out.h backend.h array.h actions.h efuns.h my-rusage.h \
    my-alloca.h typedefs.h driver.h sent.h exec.h regexp.h port.h config.h \
    hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/files.o : ../mudlib/sys/files.h xalloc.h svalue.h stdstrings.h \
    simulate.h mstrings.h main.h lex.h interpret.h filestat.h comm.h \
    array.h files.h my-alloca.h typedefs.h driver.h strfuns.h sent.h exec.h \
    instrs.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/gcollect.o : ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h swap.h \
    stdstrings.h smalloc.h simul_efun.h simulate.h sent.h rxcache.h \
    prolang.h parse.h otable.h object.h mstrings.h mapping.h main.h lex.h \
    interpret.h instrs.h heartbeat.h filestat.h exec.h ed.h comm.h \
    closure.h call_out.h backend.h array.h actions.h gcollect.h typedefs.h \
    driver.h svalue.h ptrtable.h strfuns.h regexp.h port.h config.h \
    hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/hash.o : hash.h

$(OBJ)/heartbeat.o : ../mudlib/sys/debug_info.h xalloc.h wiz_list.h \
    svalue.h strfuns.h simulate.h sent.h object.h mstrings.h interpret.h \
    gcollect.h comm.h backend.h array.h actions.h heartbeat.h typedefs.h \
    driver.h exec.h instrs.h main.h port.h config.h hosts/unix.h \
    hosts/be/be.h machine.h

$(OBJ)/interpret.o : ../mudlib/sys/trace.h ../mudlib/sys/debug_info.h \
    ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h switch.h swap.h \
    svalue.h sprintf.h smalloc.h stdstrings.h simul_efun.h simulate.h \
    sent.h ptrtable.h prolang.h parse.h otable.h object.h mstrings.h \
    mapping.h main.h lex.h instrs.h heartbeat.h gcollect.h filestat.h \
    exec.h efuns.h ed.h comm.h closure.h call_out.h backend.h array.h \
    actions.h interpret.h my-alloca.h typedefs.h driver.h strfuns.h port.h \
    config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/lex.o : efun_defs.c ../mudlib/sys/driver_hook.h xalloc.h wiz_list.h \
    svalue.h strfuns.h stdstrings.h simul_efun.h simulate.h prolang.h \
    patchlevel.h object.h mstrings.h mempools.h main.h lang.h interpret.h \
    instrs.h hash.h gcollect.h filestat.h exec.h comm.h closure.h backend.h \
    array.h lex.h my-alloca.h typedefs.h driver.h ptrtable.h sent.h port.h \
    config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/main.o : xalloc.h wiz_list.h swap.h svalue.h stdstrings.h \
    simul_efun.h simulate.h rxcache.h random.h pcre/pcre.h patchlevel.h \
    otable.h object.h mstrings.h mapping.h lex.h interpret.h gcollect.h \
    filestat.h comm.h array.h backend.h main.h my-alloca.h typedefs.h \
    driver.h ptrtable.h exec.h strfuns.h sent.h regexp.h instrs.h port.h \
    config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/mapping.o : xalloc.h wiz_list.h svalue.h smalloc.h simulate.h \
    object.h mstrings.h main.h interpret.h gcollect.h closure.h backend.h \
    array.h mapping.h my-alloca.h typedefs.h driver.h strfuns.h sent.h \
    exec.h instrs.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/md5.o : strfuns.h md5.h machine.h typedefs.h driver.h port.h \
    config.h hosts/unix.h hosts/be/be.h

$(OBJ)/mempools.o : xalloc.h gcollect.h mempools.h driver.h typedefs.h \
    port.h config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/mkfunc.o : make_func.c ../mudlib/sys/driver_hook.h hash.h exec.h \
    my-alloca.h driver.h typedefs.h port.h config.h hosts/unix.h \
    hosts/be/be.h machine.h

$(OBJ)/mstrings.o : ../mudlib/sys/debug_info.h xalloc.h svalue.h strfuns.h \
    stdstrings.h simulate.h main.h hash.h gcollect.h mstrings.h driver.h \
    typedefs.h sent.h exec.h port.h config.h hosts/unix.h hosts/be/be.h \
    machine.h

$(OBJ)/object.o : ../mudlib/sys/inherit_list.h ../mudlib/sys/include_list.h \
    ../mudlib/sys/functionlist.h ../mudlib/sys/driver_hook.h xalloc.h \
    wiz_list.h svalue.h swap.h strfuns.h stdstrings.h simul_efun.h \
    simulate.h sent.h random.h ptrtable.h prolang.h otable.h mstrings.h \
    mempools.h mapping.h main.h instrs.h interpret.h filestat.h exec.h \
    comm.h closure.h backend.h array.h actions.h object.h my-alloca.h \
    typedefs.h driver.h ../mudlib/sys/lpctypes.h port.h config.h \
    hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/otable.o : ../mudlib/sys/debug_info.h xalloc.h svalue.h simulate.h \
    strfuns.h object.h mstrings.h hash.h gcollect.h backend.h otable.h \
    typedefs.h driver.h sent.h exec.h instrs.h main.h port.h config.h \
    hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/parse.o : xalloc.h wiz_list.h svalue.h stdstrings.h simulate.h \
    object.h mstrings.h main.h interpret.h instrs.h gcollect.h closure.h \
    array.h actions.h parse.h typedefs.h driver.h strfuns.h sent.h exec.h \
    port.h config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/parse_old.o : xalloc.h wiz_list.h svalue.h stdstrings.h simulate.h \
    random.h object.h mstrings.h main.h gcollect.h interpret.h closure.h \
    array.h actions.h parse.h my-alloca.h typedefs.h driver.h strfuns.h \
    sent.h exec.h instrs.h port.h config.h hosts/unix.h hosts/be/be.h \
    machine.h

$(OBJ)/parser.o : lang.c pkg-alists.h ../mudlib/sys/driver_hook.h xalloc.h \
    wiz_list.h switch.h swap.h svalue.h stdstrings.h simul_efun.h \
    simulate.h object.h mstrings.h mapping.h main.h lex.h instrs.h \
    interpret.h gcollect.h exec.h closure.h backend.h array.h prolang.h \
    my-alloca.h typedefs.h driver.h ptrtable.h strfuns.h sent.h port.h \
    config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/pcre-get.o : pcre/get.c driver.h pcre/internal.h port.h config.h \
    pcre/pcre.h pcre/config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/pcre-maketables.o : pcre/maketables.c driver.h pcre/internal.h \
    port.h config.h pcre/pcre.h pcre/config.h hosts/unix.h hosts/be/be.h \
    machine.h

$(OBJ)/pcre-pcre.o : pcre/pcre.c driver.h pcre/chartables.c pcre/internal.h \
    port.h config.h pcre/pcre.h pcre/config.h hosts/unix.h hosts/be/be.h \
    machine.h

$(OBJ)/pcre-study.o : pcre/study.c driver.h pcre/internal.h port.h config.h \
    pcre/pcre.h pcre/config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/dftables.o : pcre/maketables.c pcre/internal.h pcre/pcre.h \
    pcre/config.h

$(OBJ)/pkg-alists.o : xalloc.h svalue.h simulate.h mstrings.h main.h \
    interpret.h array.h my-alloca.h pkg-alists.h typedefs.h driver.h \
    strfuns.h sent.h exec.h instrs.h port.h config.h hosts/unix.h \
    hosts/be/be.h machine.h

$(OBJ)/pkg-mysql.o : xalloc.h svalue.h simulate.h mstrings.h interpret.h \
    array.h pkg-mysql.h my-alloca.h typedefs.h driver.h strfuns.h sent.h \
    exec.h instrs.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/port.o : hosts/crypt.c main.h backend.h my-rusage.h driver.h \
    typedefs.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/ptrtable.o : simulate.h mempools.h ptrtable.h driver.h svalue.h \
    strfuns.h sent.h exec.h typedefs.h port.h config.h hosts/unix.h \
    hosts/be/be.h machine.h

$(OBJ)/random.o : random.h driver.h port.h config.h hosts/unix.h \
    hosts/be/be.h machine.h

$(OBJ)/regexp.o : main.h xalloc.h simulate.h comm.h regexp.h driver.h \
    typedefs.h svalue.h strfuns.h sent.h exec.h port.h config.h \
    hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/rxcache.o : ../mudlib/sys/debug_info.h xalloc.h svalue.h strfuns.h \
    simulate.h regexp.h mstrings.h hash.h gcollect.h rxcache.h driver.h \
    typedefs.h sent.h exec.h port.h config.h hosts/unix.h hosts/be/be.h \
    machine.h

$(OBJ)/simul_efun.o : xalloc.h swap.h svalue.h stdstrings.h simulate.h \
    ptrtable.h object.h mstrings.h lex.h interpret.h instrs.h gcollect.h \
    exec.h array.h simul_efun.h my-alloca.h typedefs.h driver.h strfuns.h \
    sent.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/simulate.o : ../mudlib/sys/rtlimits.h ../mudlib/sys/files.h \
    ../mudlib/sys/driver_hook.h ../mudlib/sys/debug_info.h xalloc.h \
    wiz_list.h svalue.h swap.h strfuns.h stdstrings.h smalloc.h \
    simul_efun.h sent.h rxcache.h prolang.h otable.h object.h mstrings.h \
    mapping.h main.h lex.h instrs.h interpret.h heartbeat.h gcollect.h \
    filestat.h exec.h ed.h dumpstat.h comm.h closure.h call_out.h backend.h \
    array.h actions.h simulate.h my-alloca.h typedefs.h driver.h ptrtable.h \
    regexp.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/sprintf.o : xalloc.h swap.h svalue.h stdstrings.h simul_efun.h \
    simulate.h sent.h random.h ptrtable.h object.h mstrings.h mapping.h \
    main.h instrs.h interpret.h exec.h comm.h closure.h array.h actions.h \
    sprintf.h my-alloca.h typedefs.h driver.h strfuns.h port.h config.h \
    hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/stdstrings.o : mstrings.h stdstrings.h typedefs.h driver.h port.h \
    config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/strfuns.o : xalloc.h svalue.h stdstrings.h simulate.h mstrings.h \
    comm.h strfuns.h my-alloca.h typedefs.h driver.h sent.h exec.h port.h \
    config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/swap.o : ../mudlib/sys/debug_info.h xalloc.h wiz_list.h svalue.h \
    strfuns.h stdstrings.h simul_efun.h simulate.h random.h prolang.h \
    otable.h object.h mstrings.h mapping.h main.h interpret.h gcollect.h \
    exec.h comm.h backend.h array.h swap.h typedefs.h driver.h ptrtable.h \
    sent.h instrs.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

$(OBJ)/wiz_list.o : xalloc.h svalue.h stdstrings.h simulate.h object.h \
    mstrings.h mapping.h main.h interpret.h gcollect.h backend.h array.h \
    ../mudlib/sys/wizlist.h wiz_list.h my-alloca.h typedefs.h driver.h \
    strfuns.h sent.h exec.h instrs.h port.h config.h hosts/unix.h \
    hosts/be/be.h machine.h

$(OBJ)/xalloc.o : smalloc.c main.h simulate.h interpret.h xalloc.h driver.h \
    ../mudlib/sys/debug_info.h object.h exec.h svalue.h stdstrings.h \
    mstrings.h gcollect.h backend.h smalloc.h typedefs.h strfuns.h sent.h \
    instrs.h port.h config.h hosts/unix.h hosts/be/be.h machine.h

# --- DO NOT MODIFY THIS LINE -- AUTO-DEPENDS PRECEDE ---
# --- DO NOT MODIFY THIS LINE -- SELECTED AUTO-DEPENDS FOLLOW ---
$(OBJ)/actions.o : stdstrings.h instrs.h

$(OBJ)/array.o : stdstrings.h instrs.h

$(OBJ)/backend.o : stdstrings.h instrs.h

$(OBJ)/bitstrings.o : instrs.h

$(OBJ)/call_out.o : stdstrings.h instrs.h

$(OBJ)/closure.o : stdstrings.h lang.h instrs.h

$(OBJ)/comm.o : stdstrings.h instrs.h

$(OBJ)/dumpstat.o : stdstrings.h instrs.h

$(OBJ)/ed.o : stdstrings.h instrs.h

$(OBJ)/efuns.o : stdstrings.h instrs.h

$(OBJ)/files.o : stdstrings.h instrs.h

$(OBJ)/gcollect.o : stdstrings.h instrs.h

$(OBJ)/heartbeat.o : instrs.h

$(OBJ)/interpret.o : stdstrings.h instrs.h

$(OBJ)/lex.o : efun_defs.c stdstrings.h lang.h instrs.h

$(OBJ)/main.o : stdstrings.h instrs.h

$(OBJ)/mapping.o : instrs.h

$(OBJ)/mkfunc.o : make_func.c

$(OBJ)/mstrings.o : stdstrings.h

$(OBJ)/object.o : stdstrings.h instrs.h

$(OBJ)/otable.o : instrs.h

$(OBJ)/parse.o : stdstrings.h instrs.h

$(OBJ)/parse_old.o : stdstrings.h instrs.h

$(OBJ)/parser.o : lang.c stdstrings.h instrs.h

$(OBJ)/pkg-alists.o : instrs.h

$(OBJ)/pkg-mysql.o : instrs.h

$(OBJ)/simul_efun.o : stdstrings.h instrs.h

$(OBJ)/simulate.o : stdstrings.h instrs.h

$(OBJ)/sprintf.o : stdstrings.h instrs.h

$(OBJ)/stdstrings.o : stdstrings.h

$(OBJ)/strfuns.o : stdstrings.h

$(OBJ)/swap.o : stdstrings.h instrs.h

$(OBJ)/wiz_list.o : stdstrings.h instrs.h

$(OBJ)/xalloc.o : stdstrings.h instrs.h

# --- DO NOT MODIFY THIS LINE -- SELECTED AUTO-DEPENDS PRECEDE ---
