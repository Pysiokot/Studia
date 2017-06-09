#include <pthread.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <semaphore.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>

struct library {
  int reading;
  int writing;
  int gonnaWrite;
  int gonnaRead;
  sem_t *semState;
  sem_t *readSem;
  sem_t *writeSem;
  sem_t *writersSem;
};

int *litterbox;
int litterboxSize;
int debugFlag = false;
sem_t readSem;
sem_t writeSem;
sem_t semState;
sem_t writersSem;
struct library l;
struct library *lib;
pthread_t *readers;
pthread_t *writers;
int *dividers;

void clear();
void initSemaphores();
void readerJob(void *args);
void writerJob(void *arg);
void stopWriting();
void startWriting();
void stopReading();
void startReading();
void fillLitterbox();
void sigintHandler(int signo);
