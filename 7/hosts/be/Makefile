#-------------------------------------------------------
# Amylaar LPMud - Makefile for BeOS
#-------------------------------------------------------
#
# Usage: make [CPU=ppc|x86] [<target>]
#
# The variable CPU determines the platform for which
# the program is compiled. If left unspecified, the
# executing platform is targeted. The created objects
# and binaries are located in subdirectories name
# 'obj.<CPU>'.
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
#            You are strongly encouraged to keep it defined.
#   -DYYDEBUG[=1]: Debug output for the LPC compiler.
#   -DTRACE_CODE: Enable the LPC code tracing.
    DEBUG = -DDEBUG -DTRACE_CODE

# Typical profiling, warning and optimizing options for your compiler.
    PROFIL =
    WARN =
    OPTIMIZE = -O4

# The yacc used and the typically generated parser files
# Note that bison is not yacc compatible enough.
    YACC = byacc
    YACCTAB = y.tab.
    
# The C source files.
# Though lang.c is listed, it is in fact generated from prolang.y
    SRCS = lex.c main.c interpret.c simulate.c object.c backend.c array.c\
           comm1.c ed.c regexp.c mapping.c wiz_list.c swap.c $(MALLOC).c\
           call_out.c otable.c dumpstat.c stralloc.c hash.c port.c\
           access_check.c parse_old.c parse.c lang.c rxcache.c\
           simul_efun.c sprintf.c gcollect.c closure.c random.c alloca.c

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


# set the full directory variable id not specified
ifeq ($(FULL_DIR),)
    FULL_DIR := $(shell pwd)
endif

# set the object directory
    OBJ	    := obj.$(CPU)
    OBJ_DIR := obj.$(CPU)
    OBJ_NATIVE := obj.$(NATIVE)
    OBJ_CROSS := obj.$(CROSS)

# specify the directory for libraries
    BELIBRARIES = /boot/develop/lib/$(CPU)

# specify the default libraries
    DEFAULT_LIBS = \
        libnet.so \
        libbe.so \
        libroot.so

# specify the MIMESET tool
    MIMESET = mimeset

# specify the path to the headers
    BEHEADERS = /boot/develop/headers

# specify the compiler
    CC = mwcc$(CPU)

# set initial compiler flags
    CFLAGS += -g -relax_pointers

# specify the linker
    LD = mwld$(CPU)

# set the inital linker flags for an application
    LDFLAGS += -xma

# specify basic linker flags
    LDFLAGS += \
              -nodefaults \
              $(BELIBRARIES)/glue-noinit.a \
              $(BELIBRARIES)/init_term_dyn.o \
              $(BELIBRARIES)/start_dyn.o 


# platform specific settings
ifeq ($(CPU), x86)

#   specify the library extention
    LIB_EXTENTION = .LIB

#   specify additional compiler flags
    CFLAGS += -inline off

else

ifeq ($(CPU), ppc)
#   specify the library extention
    LIB_EXTENTION =

#  specify additional linker flags
    LDFLAGS +=  \
              -export pragma \
              -init _init_routine_ \
              -term _term_routine_

endif
endif

# create the final list of libraries to use
    LIBS_TO_USE = $(addsuffix $(LIB_EXTENTION), $(DEFAULT_LIBS) )
    
# additional common linker flags
    LDFLAGS += -L$(FULL_DIR) -L$(BELIBRARIES) $(LIBS_TO_USE)
	
# create the list of include paths
    INCLUDES = -i . 

# specify where to create the application binary
    TARGET :=$(OBJ_DIR)/$(IMG_NAME)

# psuedo-function for converting a list of source files in SRCS variable
# to a corresponding list of object files in $(OBJ_DIR)/xxx.o
# The "function" strips off the src file suffix (.ccp or .c or whatever)
# and then strips of the directory name, leaving just the root file name.
# It then appends the .o suffix and prepends the $(OBJ_DIR)/ path
define SRCS_LIST_TO_OBJS
	$(addprefix $(OBJ_DIR)/, $(addsuffix .o, $(foreach file, $(SRCS), $(basename $(notdir $(file))))))
endef


# specify the list of objects
    OBJS := $(SRCS_LIST_TO_OBJS)

# generate mapfiles for metrowerks.  
    MAP_FILE	:= $(TARGET).xMAP
    SYMBOL_FILE	:= $(TARGET).xSYM

#-------------------------------------------------------
# Targets and their rules
#-------------------------------------------------------

# define the actual work to be done	
default: $(TARGET)
$(IMG_NAME): $(TARGET)

all: $(TARGET)
	@make CPU=$(CROSS) $(IMG_NAME)

$(TARGET): $(OBJ_DIR) depend $(OBJS)
	$(LD) -o $@ $(OBJS) $(LDFLAGS) -map $(MAP_FILE)
	$(MIMESET) -f $@

clean :: FORCE
	-rm -f $(YACCTAB)h $(YACCTAB)c make_func.c $(OBJ_DIR)/make_func
	-rm -f efun_defs.c instrs.h lang.y lang.h lang.c y.output tags TAGS
	-rm -f $(OBJ_DIR)/*.o

cleanall :: clean
	-rm -f obj.$(CROSS)/*.o

clobber :: FORCE
	-rm -rf $(OBJ_DIR)

clobberall :: clobber
	-rm -rf obj.$(CROSS)

depend: $(SRCS) make_func.c
	@$(SHELL) -ec "if type mkdepend > /dev/null 2>&1; then \
          echo Updating dependencies.; \
          mkdepend $(SRCS) make_func.c -x efun_defs.c -p .c:$$\(OBJ_DIR\)/%n.o -fMakefile; \
        else\
          echo mkdepend utility not available.; \
        fi"

# Special rules for making make_func, depending on whether we're
# crosscompiling or not.

make_func.c : make_func.y
	$(YACC) make_func.y
	mv $(YACCTAB)c make_func.c

$(OBJ_DIR)/make_func.o : make_func.c lint.h config.h machine.h
	$(CC) $(INCLUDES) $(CFLAGS) -DYACC='"$(YACC)"' -c $< -o $@

$(OBJ_DIR)/make_func : $(OBJ_DIR)/make_func.o $(OBJ_DIR)/hash.o
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

$(OBJ_DIR)/$(MALLOC).o : $(MALLOC).c lint.h config.h machine.h
	rm -f $(OBJ_DIR)/smalloc.o $(OBJ_DIR)/sysmalloc.o
	$(CC) $(INCLUDES) $(CFLAGS) -c $< -o $@

#--------------------------------------------------------
#	Rules for the whole system
#--------------------------------------------------------

# rule to create the object file directory if needed
$(OBJ_DIR)::
	@[ -d $(OBJ_DIR) ] || mkdir $(OBJ_DIR) > /dev/null 2>&1

# default rule for take xxx.c files on compile into $(OBJ_DIR)/xxx.o
$(OBJ_DIR)/%.o : %.c
	$(CC) $(INCLUDES) $(CFLAGS) -c $< -o $@

# empty rule. Things that depend on this rule will always get triggered
FORCE:

# remove just the application from the object folder
rmapp ::
	-rm -f $(TARGET)

#--------------------------------------------------------
# Dependencies, manual and automatic.

$(OBJ_DIR)/array.o : instrs.h lang.h

$(OBJ_DIR)/closure.o : instrs.h lang.h

$(OBJ_DIR)/dumpstat.o : instrs.h lang.h

$(OBJ_DIR)/gcollect.o : instrs.h lang.h

$(OBJ_DIR)/interpret.o : instrs.h lang.h

$(OBJ_DIR)/lang.o : lint.h config.h machine.h lex.h interpret.h object.h \
    instrs.h incralloc.h switch.h stralloc.h

$(OBJ_DIR)/lex.o : instrs.h lang.h efun_defs.c

$(OBJ_DIR)/simul_efun.o : instrs.h lang.h

$(OBJ_DIR)/simulate.o : instrs.h

$(OBJ_DIR)/sprintf.o : instrs.h

# --- DO NOT MODIFY THIS LINE -- AUTO-DEPENDS FOLLOW ---
$(OBJ_DIR)/access_check.o : comm.h lint.h config.h interpret.h sent.h \
    hosts/amiga/patchfloat.h port.h smalloc.h machine.h

$(OBJ_DIR)/alloca.o : config.h port.h machine.h

$(OBJ_DIR)/array.o : stralloc.h instrs.h lang.h exec.h rxcache.h regexp.h \
    wiz_list.h object.h interpret.h lint.h config.h smalloc.h machine.h \
    port.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/backend.o : smalloc.h regexp.h stralloc.h rxcache.h comm.h \
    exec.h wiz_list.h object.h interpret.h lint.h hosts/amiga/nsignal.h \
    config.h port.h sent.h machine.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/call_out.o : wiz_list.h exec.h stralloc.h comm.h object.h \
    interpret.h lint.h machine.h smalloc.h config.h sent.h port.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/closure.o : stralloc.h switch.h instrs.h lang.h object.h \
    interpret.h exec.h lex.h lint.h smalloc.h config.h port.h machine.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/comm1.o : hosts/amiga/nsignal.h util/erq.h exec.h wiz_list.h \
    sent.h object.h comm.h interpret.h lint.h telnet.h stralloc.h config.h \
    machine.h smalloc.h port.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/dumpstat.o : instrs.h lang.h exec.h object.h smalloc.h \
    interpret.h lint.h machine.h config.h port.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/ed.o : stralloc.h comm.h object.h interpret.h rxcache.h regexp.h \
    lint.h config.h smalloc.h sent.h port.h hosts/amiga/patchfloat.h \
    machine.h

$(OBJ_DIR)/gcollect.o : stralloc.h rxcache.h wiz_list.h lang.h instrs.h \
    smalloc.h comm.h sent.h exec.h object.h interpret.h config.h lint.h \
    regexp.h machine.h port.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/hash.o :

$(OBJ_DIR)/interpret.o : stralloc.h switch.h sent.h comm.h instrs.h \
    wiz_list.h object.h exec.h lang.h smalloc.h interpret.h lint.h config.h \
    machine.h port.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/lang.o : stralloc.h switch.h incralloc.h instrs.h config.h \
    exec.h object.h interpret.h lex.h lint.h smalloc.h port.h machine.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/lex.o : efun_defs.c hosts/amiga/socket.h stralloc.h patchlevel.h \
    instrs.h lex.h exec.h config.h lang.h interpret.h lint.h \
    hosts/amiga/socket_sim.h hosts/amiga/socket_tcp.h smalloc.h machine.h \
    port.h hosts/amiga/patchfloat.h hosts/amiga/socket_sim_protos.h

$(OBJ_DIR)/main.o : hosts/amiga/socket.h stralloc.h rxcache.h wiz_list.h \
    patchlevel.h lex.h config.h object.h interpret.h lint.h \
    hosts/amiga/socket_sim.h hosts/amiga/socket_tcp.h smalloc.h regexp.h \
    port.h hosts/amiga/patchfloat.h hosts/amiga/socket_sim_protos.h \
    machine.h

$(OBJ_DIR)/make_func.o : exec.h interpret.h config.h lint.h machine.h \
    smalloc.h port.h hosts/amiga/patchfloat.h

$(OBJ_DIR)/mapping.o : smalloc.h stralloc.h regexp.h wiz_list.h object.h \
    interpret.h lint.h config.h port.h hosts/amiga/patchfloat.h machine.h

$(OBJ_DIR)/object.o : stralloc.h exec.h wiz_list.h sent.h object.h \
    interpret.h comm.h lint.h config.h smalloc.h machine.h port.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/otable.o : object.h interpret.h config.h lint.h smalloc.h port.h \
    hosts/amiga/patchfloat.h machine.h

$(OBJ_DIR)/parse.o : stralloc.h instrs.h wiz_list.h object.h config.h \
    interpret.h lint.h smalloc.h exec.h port.h hosts/amiga/patchfloat.h \
    machine.h

$(OBJ_DIR)/parse_old.o : wiz_list.h object.h config.h interpret.h lint.h \
    port.h smalloc.h hosts/amiga/patchfloat.h machine.h

$(OBJ_DIR)/port.o : hosts/crypt.c lint.h machine.h config.h \
    hosts/amiga/patchfloat.h port.h

$(OBJ_DIR)/random.o : lint.h hosts/amiga/patchfloat.h config.h port.h \
    machine.h

$(OBJ_DIR)/regexp.o : port.h lint.h regexp.h machine.h \
    hosts/amiga/patchfloat.h config.h

$(OBJ_DIR)/rxcache.o : stralloc.h smalloc.h rxcache.h regexp.h lint.h \
    config.h port.h hosts/amiga/patchfloat.h machine.h

$(OBJ_DIR)/simul_efun.o : stralloc.h instrs.h lang.h lex.h exec.h object.h \
    interpret.h lint.h smalloc.h config.h machine.h port.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/simulate.o : rxcache.h stralloc.h comm.h exec.h wiz_list.h \
    sent.h object.h instrs.h interpret.h lint.h port.h \
    hosts/amiga/nsignal.h config.h regexp.h smalloc.h machine.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/smalloc.o : object.h exec.h lint.h interpret.h machine.h \
    hosts/amiga/patchfloat.h config.h smalloc.h port.h

$(OBJ_DIR)/sprintf.o : stralloc.h sent.h object.h exec.h instrs.h lang.h \
    interpret.h lint.h config.h smalloc.h machine.h port.h \
    hosts/amiga/patchfloat.h

$(OBJ_DIR)/stralloc.o : stralloc.h smalloc.h lint.h config.h \
    hosts/amiga/patchfloat.h port.h machine.h

$(OBJ_DIR)/swap.o : hosts/amiga/socket.h wiz_list.h exec.h object.h \
    interpret.h lint.h config.h hosts/amiga/socket_sim.h \
    hosts/amiga/socket_tcp.h machine.h smalloc.h port.h \
    hosts/amiga/patchfloat.h hosts/amiga/socket_sim_protos.h

$(OBJ_DIR)/wiz_list.o : stralloc.h wiz_list.h object.h interpret.h lint.h \
    config.h smalloc.h port.h hosts/amiga/patchfloat.h machine.h

# --- DO NOT MODIFY THIS LINE -- AUTO-DEPENDS PRECEDE ---
