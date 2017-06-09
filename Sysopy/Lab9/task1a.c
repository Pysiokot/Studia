#include "header.h"

int main(int argc, char **argv){
  if(argc != 4 && argc != 5){
    printf("Bad number of args\n");
    return 1;
  }

  if(argc == 5 && strcmp(argv[4], "-i") != 0){
    printf("%s\n", "Wrong option on the end. Try '-i'");
    return 1;
  }
  else{
    debugFlag = true;
  }
  signal(SIGINT, sigintHandler);
  int catWritersNo = atoi(argv[1]);
  int catReadersNo = atoi(argv[2]);
  litterboxSize = atoi(argv[3]);
  srand((unsigned int)time(NULL));
  initSemaphores();
  fillLitterbox();
  
  readers = calloc((size_t)catReadersNo, sizeof(pthread_t));
  writers = calloc((size_t)catWritersNo, sizeof(pthread_t));
  dividers = calloc((size_t)catReadersNo, sizeof(int));

  for (size_t kot = 0; kot < catReadersNo; kot++) {
    dividers[kot] = rand() % 20 + 1;
    pthread_create(&readers[kot], NULL, (void *(*)(void *)) & readerJob, &dividers[kot]);
  }

  for (size_t kot = 0; kot < catWritersNo; kot++) {
    pthread_create(&writers[kot], NULL, (void *(*)(void *)) & writerJob, NULL);
  }

  for (int kot = 0; kot < catReadersNo; kot++) {
    pthread_join(readers[kot], NULL);
  }

  for (int kot = 0; kot < catWritersNo; kot++) {
    pthread_join(writers[kot], NULL);
  }

  return 0;
}

void sigintHandler(int signo){
  clear();
  printf("\n\n\nEND\n\n\n");
  exit(0);
}

void clear(){
  free(readers);
  free(dividers);
  free(writers);
  free(litterbox);
}

void initSemaphores(){
  sem_init(&readSem, 0, 0);
  sem_init(&writeSem, 0, 0);
  sem_init(&semState, 0, 1);
  sem_init(&writersSem, 0, 1);

  l.readSem = &readSem;
  l.writeSem = &writeSem;
  l.semState = &semState;
  l.writersSem = &writersSem;
  l.reading = 0;
  l.writing = 0;
  l.gonnaWrite = 0;
  l.gonnaRead = 0;

  lib = &l;
}

void fillLitterbox(){
  litterbox = (int *)malloc(litterboxSize * sizeof(int));
  for(int kot = 0; kot < litterboxSize; kot++){
    litterbox[kot] = rand();
  }
}

void readerJob(void *args){
  int divider = *((int *)args);

  while (true) {
    startReading();
    int divisible = 0;

    for (int kot = 0; kot < litterboxSize; kot++) {
      if (litterbox[kot] % divider == 0) {
        divisible++;
        if (debugFlag) printf("%i Number: %i is divisible by: %i\n", kot, litterbox[kot], divider);
      }
    }
    printf("%i numbers are divisible by %i\n", divisible, divider);
    stopReading();
    usleep(1000);
  }
}

void writerJob(void *args){
  while (true) {
    startWriting();
    int modificationsNo = rand() % litterboxSize;

    printf("Starting modifications\n");
    for (int kot = 0; kot < modificationsNo; kot++) {
      int toChange = rand() % litterboxSize;
      int tmp = litterbox[toChange];
      litterbox[toChange] = rand();
      if (debugFlag)
        printf("Changing %i value from: %i to %i\n", toChange, tmp, litterbox[toChange]);
    }
    printf("End of modifications\n");
    stopWriting();
    usleep(1000);
  }
}

void startReading() {
  sem_wait(lib->semState);
  lib->gonnaRead++;
  if (lib->writing == 0) {
    lib->gonnaRead--;
    lib->reading++;
    sem_post(lib->semState);
  } else {
    sem_post(lib->semState);
    sem_wait(lib->readSem);
  }
}

void stopReading() {
  sem_wait(lib->semState);
  lib->reading--;
  if (lib->reading == 0) {
    while (lib->gonnaWrite > 0) {
      lib->gonnaWrite--;
      lib->writing++;
      sem_post(lib->writeSem);
    }
  }
  sem_post(lib->semState);
}

void startWriting() {
  sem_wait(lib->semState);
  lib->gonnaWrite++;
  if (lib->reading == 0) {
    lib->writing++;
    lib->gonnaWrite--;
    sem_post(lib->semState);
  } else {
    sem_post(lib->semState);
    sem_wait(lib->writeSem);
  }
  sem_wait(lib->writersSem);
}

void stopWriting() {
  sem_post(lib->writersSem);
  sem_wait(lib->semState);
  lib->writing--;
  if (lib->writing == 0) {
    while (lib->gonnaRead > 0) {
      lib->gonnaRead--;
      lib->reading++;
      sem_post(lib->readSem);
    }
  }
  sem_post(lib->semState);
}
