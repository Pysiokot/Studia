#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>

void *queueAddress;
size_t queuesize;
int queue;
int semaphores;
int numberOfPlaces;
struct timespec ts;
struct sembuf semaphoreAction;
union semun sunion;
pid_t *client;
int *firstClient;
int *queuePlaces;
pid_t *clientsPIDs;

void closeSemaphores();
void closeQueue();
void unmapQueue();
void sigintHandler(int signo);
void createSemaphores();
void dayTime();
void queueAwaken();
void queueSleep();
void worshopClosed();
void golibrodaNotBusy();
void golibrodaSleep();
void nextClientAwaken();
void isOk();