#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdbool.h>

typedef struct threadProperties{
  int records;
  int file;
  const char *search;
  void(*whatToDo)(pthread_t*);
  pthread_t *threads;
} threadProperties;


bool isFileReaded = false;
int taskType;


void jobToDo(void *arg);
void killAll(pthread_t *thread);
void nothing(pthread_t *thread);