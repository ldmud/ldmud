CFLAGS = -O -DMUD_LIB='"$(MUD_LIB)"'

make_docs:

clean:
	-rm *.o make_docs

install: make_docs
	install -c -s $? $(BINDIR)
