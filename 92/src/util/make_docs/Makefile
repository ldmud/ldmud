CFLAGS = -O -DMUD_LIB='"$(MUD_LIB)"'

make_docs:

clean:
	-rm -f *.o make_docs

install: make_docs
	cp $? $(BINDIR)
#	$(INSTALL) -c -s $? $(BINDIR)
