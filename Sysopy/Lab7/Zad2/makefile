CC=gcc
CFLAGS += -Wall -lpthread -lrt


.PHONY: all
all: golibroda klient


golibroda.o: golibroda.c forall.h
	$(CC) -c -o $@ $< $(CFLAGS)

golibroda: golibroda.o
	$(CC) -o $@ $^ $(CFLAGS)


klient.o: klient.c forall.h
	$(CC) -c -o $@ $< $(CFLAGS)

klient: klient.o
	$(CC) -o $@ $^ $(CFLAGS)


.PHONY: clean
clean:
	rm -f klient klient.o golibroda golibroda.o
