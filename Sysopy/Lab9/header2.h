#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <stdbool.h>

struct library{
    int reading;
    int writing;
    pthread_cond_t write;
    pthread_cond_t read;
};

int *litterbox;
int litterboxSize;
int debugFlag = false;
int *dividers;
struct library *lib;
struct library l;
pthread_t *readers;
pthread_t *writers;
pthread_mutex_t mudegz;

void clear();
void initLibrary();
void readerJob(void *args);
void writerJob(void *arg);
void stopWriting();
void startWriting();
void stopReading();
void startReading();
void fillLitterbox();
void sigintHandler(int signo);