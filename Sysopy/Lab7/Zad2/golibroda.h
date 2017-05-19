#include "forall.h"
#include <semaphore.h>
#include <unistd.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <signal.h>


#define SEMAPHOREQTY 5
void *lobbyAddress;
size_t lobbySize;
sem_t *semaphores[SEMAPHOREQTY];
struct timespec ts;


void closeAllSemaphores();
void closeLobby();
void unmapLobby();
void sigintHandler(int signo);