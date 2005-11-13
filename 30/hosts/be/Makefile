#-------------------------------------------------------
# Amylaar LPMud - Makefile for BeOS
#-------------------------------------------------------
#
# Usage: make [CPU=ppc|x86] [O=<opt-spec>] [<target>]
#
# The variable CPU determines the platform for which
# the program is compiled. If left unspecified, the
# executing platform is targeted. The created objects
# and binaries are located in subdirectories name
# 'obj.<CPU>'.
# The variable O determines the optimization, the value
# <opt-spec> is passed directly to the compiler. If the
# variable is not specified, '1' is used as opt-spec.
#
# At the moment, cross-compiling is not supported.
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

# The directory the mudlib usually is in.
    MUDLIB = /boot/home/mud/lib

# The directory where the executables are installed. The default
# erq path starts there.
    BINDIR = /boot/home/mud/bin

# Debugging options:
#   -DDEBUG: Enable run time debugging. It will use more time and space.
#            Nevertheless you are strongly encouraged to keep it defined.
#   -DYYDEBUG[=1]: Debug output for the LPC compiler.
#   -DTRACE_CODE: Enable the LPC code tracing.
    DEBUG = -DDEBUG -DTRACE_CODE

# Typical profiling, warning and optimizing options.
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
    SRCS = lex.c main.c interpret.c simulate.c object.c backend.c array.c\
           comm.c ed.c regexp.c mapping.c wiz_list.c swap.c $(MALLOC).c\
           call_out.c otable.c dumpstat.c stralloc.c hash.c port.c\
           access_check.c parse_old.c parse.c lang.c rxcache.c\
           simul_efun.c sprintf.c gcollect.c closure.c random.c \
           filestat.c

# Initialize the CFLAGS with the values above
    CFLAGS += $(OPTIMIZE) $(DEBUG) -DMALLOC_$(MALLOC) $(WARN) $(PROFIL)
    CFLAGS += -DMUD_LIB='"$(MUDLIB)"' -DBINDIR='"$(BINDIR)"'

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

# The mimeset tool
    MIMESET = mimeset

# platform specific settings
ifeq ($(CPU), x86)
    CC = gcc
    LD = gcc
    LDFLAGS += -Xlinker -soname=_APP_
endif

ifeq ($(CPU), ppc)
    CC = mwcc$(CPU)
    CFLAGS += -g -relax_pointers # -ansi strict # -w9
    # -ansi strict disallows 'inline'
    LD = mwld$(CPU)
    LDFLAGS += -sym full -xma  -map $(MAP_FILE) -export pragma \
               -init _init_routine_ -term _term_routine_

    # generate mapfiles for metrowerks.  
    MAP_FILE	:= $(TARGET).xMAP
    SYMBOL_FILE	:= $(TARGET).xSYM
endif

ifneq ($(CPU), $(NATIVE))
    CC = echo Cross-compilation is not supported!; exit 2;
    LD = $(CC)
endif

# pseudo-function for converting a list of source files in SRCS variable
# to a corresponding list of object files in $(OBJ_DIR)/xxx.o
# The "function" strips off the src file suffix (.ccp or .c or whatever)
# and then strips of the directory name, leaving just the root file name.
# It then appends the .o suffix and prepends the $(OBJ_DIR)/ path
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
	@echo Cross-compilation is not supported!
#	@make CPU=$(CROSS) $(IMG_NAME)

$(TARGET): $(OBJ) depend $(OBJS)
	$(LD) -o $@ $(OBJS) $(LDFLAGS)
	$(MIMESET) -f $@

clean :: FORCE
	-rm -f $(YACCTAB)h $(YACCTAB)c make_func.c $(OBJ)/make_func
	-rm -f *~ efun_defs.c instrs.h lang.y lang.h lang.c y.output tags TAGS
	-rm -f hosts/*~ hosts/*/*~ bugs/*~ done/*~ mudlib/*~ $(OBJ)/*.o

cleanall :: clean
	-rm -f obj.$(CROSS)/*.o

clobber :: clean FORCE
	-rm -rf $(OBJ)

clobberall :: clobber cleanall
	-rm -rf obj.$(CROSS)

depend: $(SRCS) make_func.c $(OBJ)
	@$(SHELL) -ec "if type mkdepend > /dev/null 2>&1; then \
          echo Updating dependencies.; \
          mkdepend $(SRCS) make_func.c -x efun_defs.c -p .c:$$\(OBJ_DIR\)/%n.o -fMakefile; \
          mkdepend $(SRCS) make_func.c -x efun_defs.c -p .c:$$\(OBJ_DIR\)/%n.o -fhosts/be/Makefile; \
        else\
          echo mkdepend utility not available.; \
        fi"

.PHONY dependall: depend depend-generic depend-amiga

depend-generic: $(SRCS) make_func.c $(OBJ)
	@$(SHELL) -ec "if type mkdepend > /dev/null 2>&1; then \
          echo Updating dependencies in Makefile.in.; \
          mkdepend $(SRCS) make_func.c -x efun_defs.c -p .c:%n.o -fMakefile.in; \
          echo Updating dependencies in hosts/os2/Makefile.; \
          mkdepend $(SRCS) make_func.c -x efun_defs.c -p .c:%n.o -fhosts/os2/Makefile; \
        else\
          echo mkdepend utility not available.; \
        fi"

AMIGASRCS:= $(addprefix hosts/amiga/, amiga.c signal.c signal_rr.c socket.c ixfile.c)

depend-amiga: $(SRCS) make_func.c $(AMIGASRCS)  $(OBJ)
	@$(SHELL) -ec "if type mkdepend > /dev/null 2>&1; then \
          echo Updating dependencies in hosts/amiga/DMakefile.; \
          mkdepend $(SRCS) make_func.c $(AMIGASRCS) -x efun_defs.c -p .c+:%n.o -Ihosts/amiga -fhosts/amiga/DMakefile; \
          echo Updating dependencies in hosts/amiga/Makefile.; \
          mkdepend $(SRCS) make_func.c $(AMIGASRCS) -x efun_defs.c -p .c+:%n.o -Ihosts/amiga -fhosts/amiga/Makefile; \
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
	$(OBJ_NATIVE)/make_func > efun_defs.c

lang.c lang.h : lang.y
	$(YACC) -d -v lang.y
	mv $(YACCTAB)c lang.c
	mv $(YACCTAB)h lang.h

# The making of the mallocator.

$(OBJ)/$(MALLOC).o : $(MALLOC).c config.h driver.h machine.h port.h
	rm -f $(OBJ)/smalloc.o $(OBJ)/sysmalloc.o
	$(CC) $(INCLUDES) $(CFLAGS) -c $< -o $@

#--------------------------------------------------------
#	Rules for the whole system
#--------------------------------------------------------

# rule to create the object file directory if needed
$(OBJ)::
	@[ -d $(OBJ) ] || mkdir $(OBJ) > /dev/null 2>&1

# Don't stumble over non-existing include files.
%.h :
	@echo "Warning: Can't find $@."

# default rule for take xxx.c files on compile into $(OBJ)/xxx.o
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
$(OBJ_DIR)/access_check.o : filestat.h comm.h access_check.h driver.h \
    sent.h object.h interpret.h config.h port.h machine.h exec.h \
    datatypes.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/array.o : wiz_list.h swap.h stralloc.h simulate.h rxcache.h \
    regexp.h prolang.h object.h mapping.h main.h interpret.h instrs.h \
    exec.h datatypes.h backend.h array.h my-alloca.h driver.h smalloc.h \
    sent.h lang.h port.h machine.h config.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/backend.o : wiz_list.h swap.h stralloc.h smalloc.h simulate.h \
    rxcache.h regexp.h object.h my-alloca.h mapping.h main.h lex.h \
    interpret.h gcollect.h filestat.h exec.h ed.h comm.h call_out.h array.h \
    backend.h hosts/amiga/nsignal.h driver.h datatypes.h sent.h instrs.h \
    port.h machine.h config.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/call_out.o : wiz_list.h stralloc.h simulate.h object.h main.h \
    interpret.h gcollect.h exec.h comm.h closure.h backend.h array.h \
    call_out.h driver.h smalloc.h sent.h instrs.h datatypes.h port.h \
    machine.h config.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/closure.o : switch.h simul_efun.h simulate.h stralloc.h \
    prolang.h object.h main.h lex.h instrs.h interpret.h exec.h array.h \
    closure.h my-alloca.h driver.h sent.h smalloc.h lang.h datatypes.h \
    port.h machine.h config.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/comm.o : util/erq.h wiz_list.h stralloc.h simulate.h sent.h \
    object.h main.h interpret.h gcollect.h filestat.h exec.h ed.h backend.h \
    array.h access_check.h comm.h hosts/amiga/nsignal.h telnet.h \
    my-alloca.h driver.h smalloc.h instrs.h datatypes.h config.h port.h \
    machine.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/dumpstat.o : smalloc.h simulate.h prolang.h object.h mapping.h \
    instrs.h interpret.h filestat.h exec.h comm.h array.h dumpstat.h \
    driver.h datatypes.h sent.h lang.h port.h machine.h config.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/ed.o : stralloc.h simulate.h rxcache.h regexp.h object.h main.h \
    interpret.h gcollect.h filestat.h comm.h closure.h ed.h driver.h \
    smalloc.h sent.h instrs.h exec.h datatypes.h port.h machine.h config.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/filestat.o : stralloc.h simulate.h hash.h gcollect.h comm.h \
    backend.h filestat.h driver.h smalloc.h sent.h object.h instrs.h \
    interpret.h exec.h port.h machine.h config.h datatypes.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/gcollect.o : wiz_list.h swap.h stralloc.h smalloc.h simul_efun.h \
    simulate.h sent.h rxcache.h prolang.h otable.h object.h mapping.h \
    main.h lex.h instrs.h interpret.h filestat.h exec.h ed.h comm.h \
    closure.h call_out.h backend.h array.h gcollect.h driver.h datatypes.h \
    regexp.h lang.h port.h machine.h config.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/hash.o : hash.h

$(OBJ_DIR)/interpret.o : wiz_list.h switch.h swap.h sprintf.h smalloc.h \
    stralloc.h simul_efun.h simulate.h sent.h random.h prolang.h parse.h \
    otable.h object.h mapping.h main.h lex.h instrs.h gcollect.h exec.h \
    ed.h comm.h closure.h call_out.h backend.h array.h interpret.h \
    my-rusage.h my-alloca.h driver.h datatypes.h lang.h port.h machine.h \
    config.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/lang.o : wiz_list.h switch.h swap.h stralloc.h simul_efun.h \
    simulate.h object.h mapping.h main.h lex.h instrs.h interpret.h \
    gcollect.h exec.h closure.h backend.h array.h prolang.h my-alloca.h \
    driver.h smalloc.h sent.h datatypes.h lang.h port.h machine.h config.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/lex.o : efun_defs.c hosts/amiga/socket.h stralloc.h simul_efun.h \
    simulate.h prolang.h patchlevel.h main.h instrs.h hash.h gcollect.h \
    filestat.h exec.h comm.h closure.h array.h lex.h my-alloca.h driver.h \
    hosts/amiga/socket_sim.h hosts/amiga/socket_tcp.h smalloc.h object.h \
    interpret.h sent.h lang.h datatypes.h port.h machine.h config.h \
    hosts/amiga/socket_sim_protos.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/main.o : wiz_list.h swap.h stralloc.h simul_efun.h simulate.h \
    rxcache.h random.h patchlevel.h otable.h object.h mapping.h lex.h \
    interpret.h gcollect.h filestat.h comm.h array.h backend.h main.h \
    hosts/amiga/socket.h my-alloca.h driver.h exec.h smalloc.h sent.h \
    instrs.h regexp.h datatypes.h hosts/amiga/socket_sim.h \
    hosts/amiga/socket_tcp.h port.h machine.h config.h \
    hosts/amiga/socket_sim_protos.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/make_func.o : interpret.h hash.h exec.h my-alloca.h driver.h \
    datatypes.h port.h machine.h config.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/mapping.o : wiz_list.h stralloc.h smalloc.h simulate.h regexp.h \
    object.h main.h interpret.h gcollect.h backend.h array.h mapping.h \
    my-alloca.h driver.h datatypes.h sent.h instrs.h exec.h port.h \
    machine.h config.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/object.o : wiz_list.h swap.h stralloc.h simulate.h sent.h \
    random.h prolang.h otable.h mapping.h main.h interpret.h filestat.h \
    exec.h comm.h closure.h backend.h array.h object.h my-alloca.h driver.h \
    smalloc.h instrs.h lang.h datatypes.h port.h machine.h config.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/otable.o : simulate.h object.h interpret.h hash.h gcollect.h \
    comm.h otable.h driver.h sent.h instrs.h exec.h datatypes.h port.h \
    machine.h config.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/parse.o : wiz_list.h stralloc.h simulate.h object.h main.h \
    instrs.h interpret.h closure.h array.h parse.h driver.h smalloc.h \
    sent.h exec.h datatypes.h port.h machine.h config.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/parse_old.o : wiz_list.h simulate.h random.h object.h main.h \
    interpret.h array.h parse.h driver.h sent.h instrs.h exec.h datatypes.h \
    smalloc.h port.h machine.h config.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/port.o : hosts/crypt.c main.h backend.h my-rusage.h driver.h \
    interpret.h object.h port.h machine.h config.h datatypes.h exec.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/random.o : random.h driver.h port.h machine.h config.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/regexp.o : regexp.h driver.h port.h machine.h config.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/rxcache.o : stralloc.h smalloc.h regexp.h hash.h gcollect.h \
    comm.h rxcache.h driver.h datatypes.h object.h interpret.h exec.h \
    sent.h port.h machine.h config.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/simul_efun.o : swap.h stralloc.h simulate.h prolang.h parse.h \
    object.h lex.h instrs.h interpret.h gcollect.h exec.h array.h \
    simul_efun.h my-alloca.h driver.h smalloc.h sent.h lang.h datatypes.h \
    port.h machine.h config.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/simulate.o : wiz_list.h swap.h stralloc.h simul_efun.h sent.h \
    rxcache.h prolang.h otable.h object.h mapping.h main.h lex.h instrs.h \
    interpret.h filestat.h exec.h ed.h dumpstat.h comm.h closure.h \
    call_out.h backend.h array.h simulate.h hosts/amiga/nsignal.h \
    my-alloca.h driver.h smalloc.h regexp.h lang.h datatypes.h port.h \
    machine.h config.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/smalloc.o : object.h exec.h simulate.h main.h gcollect.h \
    backend.h comm.h smalloc.h driver.h interpret.h sent.h instrs.h \
    datatypes.h port.h machine.h config.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/sprintf.o : swap.h stralloc.h simul_efun.h simulate.h sent.h \
    prolang.h object.h main.h instrs.h interpret.h exec.h array.h sprintf.h \
    driver.h smalloc.h lang.h datatypes.h port.h machine.h config.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/stralloc.o : simulate.h hash.h gcollect.h comm.h stralloc.h \
    driver.h sent.h object.h instrs.h interpret.h exec.h smalloc.h port.h \
    machine.h config.h datatypes.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/swap.o : hosts/amiga/socket.h wiz_list.h stralloc.h simul_efun.h \
    simulate.h prolang.h object.h mapping.h main.h interpret.h gcollect.h \
    exec.h comm.h array.h swap.h driver.h hosts/amiga/socket_sim.h \
    hosts/amiga/socket_tcp.h smalloc.h sent.h instrs.h lang.h datatypes.h \
    port.h machine.h config.h hosts/amiga/socket_sim_protos.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/wiz_list.o : stralloc.h object.h main.h interpret.h gcollect.h \
    backend.h array.h wiz_list.h driver.h smalloc.h exec.h datatypes.h \
    instrs.h port.h machine.h config.h hosts/amiga/patchfloat.h

# --- DO NOT MODIFY THIS LINE -- AUTO-DEPENDS PRECEDE ---
