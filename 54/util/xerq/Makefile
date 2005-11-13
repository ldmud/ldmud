CFLAGS = -g -O4
CC = gcc

OBJ = erq.o execute.o socket.o lookup.o

erq:	$(OBJ) 
	$(CC) $(CFLAGS) $(OBJ) -o $@

%.o : %.c
	$(CC) -I../.. $(CFLAGS) -c $< -o $@

*.o: erq.h erq-config.h defs.h ../../config.h ../../machine.h ../../port.h

clean:
	rm -f erq *.o *~

realclean: clean
	rm -f erq-config.h
