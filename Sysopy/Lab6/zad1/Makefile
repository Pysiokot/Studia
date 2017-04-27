CFLAGS=-std=gnu99 -Wall

all: client server

client: client.c
	gcc $(CFLAGS) -o client client.c
server: server.c
	gcc $(CFLAGS) -o server server.c

clean:
	rm -f client server

.PHONY: all client server clean
