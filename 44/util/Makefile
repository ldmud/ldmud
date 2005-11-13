SUBDIRS = indent make_docs xerq
SED = sed
#CFLAGS = -O
CFLAGS = -g
BINDIR=/user/mud/bin

all:	$(SUBDIRS) erq fundesc

$(SUBDIRS): 
	cd $@; $(MAKE)

FORCE: install

erq:	erq.c
#	$(CC) erq.c -lresolv -o erq
	$(CC) $(CFLAGS) erq.c -o erq

fundesc:
	$(CC) fundesc.c -o fundesc

install: erq fundesc
	cp erq $(BINDIR)/erq
	cp fundesc $(BINDIR)/fundesc
#	$(SED) -e "s;MUD_LIB;$(MUD_LIB);" \
#	-e "s;BINDIR;$(BINDIR);" restart_mud > $(BINDIR)/restart_mud
#	-chmod 775 $(BINDIR)/restart_mud
	for i in $(SUBDIRS) ;\
	do \
	(cd $$i ; echo "Installing in $$i." ;\
	$(MAKE) $(MFLAGS) BINDIR=$(BINDIR) install);\
	done

clean:
	-rm -f *.o fundesc erq *~
	for i in $(SUBDIRS) ;\
	do \
	(cd $$i ; echo "Cleaning in $$i." ;\
	$(MAKE) clean);\
	done
