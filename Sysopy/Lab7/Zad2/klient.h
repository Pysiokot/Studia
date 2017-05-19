#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <semaphore.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include "forall.h"
#include <time.h>

#define SEMAPHOREQTY 5
sem_t *semaphores[SEMAPHOREQTY];
void *queueAddress;
size_t lobbySize;

void closeSemaphores();
void closeQueue();
void unmapQueue();