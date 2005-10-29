SUBDIRS = indent make_docs

all:	$(SUBDIRS) fundesc

$(SUBDIRS): FORCE
	cd $@; make

FORCE: install

hname:	hname.c
	$(CC) hname.c -lresolv -o hname

fundesc:
	$(CC) fundesc.c -o fundesc

install: hname fundesc
	cp hname $(BINDIR)/hname
	cp fundesc $(BINDIR)/fundesc
	/bin/sed -e "s;MUD_LIB;$(MUD_LIB);" \
	-e "s;BINDIR;$(BINDIR);" restart_mud > $(BINDIR)/restart_mud
	-chmod 775 $(BINDIR)/restart_mud
	for i in $(SUBDIRS) ;\
	do \
	(cd $$i ; echo "Installing in $$i." ;\
	$(MAKE) $(MFLAGS) install);\
	done

clean:
	-rm *.o fundesc hname
	for i in $(SUBDIRS) ;\
	do \
	(cd $$i ; echo "Cleaning in $$i." ;\
	$(MAKE) clean);\
	done
