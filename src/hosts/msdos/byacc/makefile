DEST          = .

HDRS          = defs.h

#CFLAGS        = -DNDEBUG -DMSDOS -A -mc
CFLAGS        = -DNDEBUG -DMSDOS

LDFLAGS       =

LIBS          =

LINKER        = gcc


OBJS          = closure.o \
		error.o \
		lalr.o \
		lr0.o \
		main.o \
		mkpar.o \
		output.o \
		reader.o \
		skeleton.o \
		symtab.o \
		verbose.o \
		warshall.o

PRINT         = pr -f -l88

PROGRAM       = byacc.exe

SRCS          = closure.c \
		error.c \
		lalr.c \
		lr0.c \
		main.c \
		mkpar.c \
		output.c \
		reader.c \
		skeleton.c \
		symtab.c \
		verbose.c \
		warshall.c

all:            $(PROGRAM)

$(PROGRAM):     $(OBJS) $(LIBS)
		$(LINKER) $(LDFLAGS) -o $(PROGRAM) $(OBJS)

clean:;         @rm -f $(OBJS)

clobber:;       @rm -f $(OBJS) $(PROGRAM)

depend:;        @mkmf -f $(MAKEFILE) PROGRAM=$(PROGRAM) DEST=$(DEST)

index:;         @ctags -wx $(HDRS) $(SRCS)

install:        $(PROGRAM)
		@echo Installing $(PROGRAM) in $(DEST)
		@install -s $(PROGRAM) $(DEST)

listing:;       @$(PRINT) Makefile $(HDRS) $(SRCS) | lpr

lint:;          @lint $(SRCS)

program:        $(PROGRAM)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

###
closure.obj: defs.h
error.obj: defs.h
lalr.obj: defs.h
lr0.obj: defs.h
main.obj: defs.h
mkpar.obj: defs.h
output.obj: defs.h
reader.obj: defs.h
skeleton.obj: defs.h
symtab.obj: defs.h
verbose.obj: defs.h
warshall.obj: defs.h
