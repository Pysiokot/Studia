#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include "forall.h"
#include <time.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>

int semaphores;
int queue;
pid_t *client;
int *firstClient;
int *queuen;
pid_t *clientsPIDs;
struct sembuf semaphoreAction;
struct timespec ts;


void *queueAddress;
void unmapQueue();
void nextClientSleep();
void nextClientAwaken();
void golibrodaAwaken();
void jobPerformed();
void workshopOpen();
void queueSleep();
void queueAwaken();