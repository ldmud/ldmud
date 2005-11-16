#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.9 (Berkeley) 9/15/88
#
CFLAGS=	-g
#LDFLAGS=	-g
#LDFLAGS=
#LIBC=	/lib/libc.a
#LIBC=	/usr/lib/libc.a
SRCS=	indent.c io.c lexi.c parse.c pr_comment.c args.c globs.c
OBJS=	indent.o io.o lexi.o parse.o pr_comment.o args.o globs.o
HEADERS= indent_globs.h version.h
MAN=	indent.texinfo ChangeLog Projects
# -g is here rather than in CFLAGS since many versions of cc do not
# allow -g with -O
CC=gcc

all: indent

indent: ${OBJS} ${LIBC}
	${CC} -o $@ ${LDFLAGS} ${OBJS}
	cp indent $(BINDIR)/indent

indent.tar.Z: ${SRCS} Makefile ${MAN} ${HEADERS}
	tar -c -z -f indent.tar.Z ${SRCS} Makefile ${MAN} ${HEADERS}

${OBJS}: indent_globs.h
args.o: version.h

clean:
	rm -f ${OBJS} core indent

install:	indent
	install -c -s indent $(BINDIR)

