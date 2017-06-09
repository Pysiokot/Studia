#include "header2.h"

int main(int argc, char** argv) {
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

    srand((unsigned int) time(NULL));
    signal(SIGINT, sigintHandler);
    int readersNo = atoi(argv[1]);
    int writersNo = atoi(argv[2]);
    litterboxSize = atoi(argv[3]);
    initLibrary();
    fillLitterbox();
    dividers = calloc((size_t) readersNo, sizeof(int));
    readers = calloc((size_t) readersNo, sizeof(pthread_t));
    writers = calloc((size_t) writersNo, sizeof(pthread_t));

    for (size_t kot = 0; kot < readersNo; kot++) {
        dividers[kot] = rand()%10 + 1;
        pthread_create(&readers[i], NULL, (void *(*)(void *)) & readerJob, &dividers[kot]);
    }

    for (size_t kot = 0; kot < writersNo; kot++) {
        pthread_create(&writers[kot], NULL, (void *(*)(void *)) & writerJob, NULL);
    }

    for (int kot = 0; kot < readersNo; kot++) {
        pthread_join(readers[kot], NULL);
    }

    for (int kot = 0; kot < writersNo; kot++) {
        pthread_join(writers[kot], NULL);
    }

    pthread_mutex_destroy(&mudegz);
    pthread_cond_destroy(&lib->write);
    pthread_cond_destroy(&lib->read);

    return 0;
}

void sigintHandler(int signo){
  clear();
  printf("\n\n\nEND\n\n\n");
  exit(0);
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
        if (debugFlag) printf("%i number at index:%i is divisible by: %i\n", litterbox[kot], kot, divider);
      }
    }
    printf("Divisible by %i -> %i\n", divider, divisible);
    stopReading();
    usleep(10000);
  }
}

void writerJob(void *args){
  while (true) {
    startWriting();
    int modificationsNo = rand() % litterboxSize;

    printf("Starting modifications\n");
    for (int kot = 0; kot < modificationsNo; kot++) {
      int toChange = rand() % litterboxSize;
      int temp = litterbox[toChange];
      litterbox[toChange] = rand();
      if (debugFlag)    printf("Changing %i value from: %i to %i\n", toChange+1, temp, litterbox[toChange]);
    }
    printf("End of modifications\n");
    stopWriting();
    usleep(10000);
  }
}

void startReading() {
    pthread_mutex_lock(&mudegz);
    lib->reading++;
    if(lib->writing > 0)  pthread_cond_wait(&lib->read, &mudegz);
    pthread_mutex_unlock(&mudegz);
}

void stopReading() {
    pthread_mutex_lock(&mudegz);
    lib->reading--;
    if(lib->reading == 0) pthread_cond_signal(&lib->write);
    pthread_mutex_unlock(&mudegz);
}

void startWriting() {
    pthread_mutex_lock(&mudegz);
    lib->writing++;
    if(lib->reading != 0 || m->writing != 0)  pthread_cond_wait(&lib->write, &mudegz);
    pthread_mutex_unlock(&mudegz);

}

void stopWriting() {
    pthread_mutex_lock(&mudegz);
    lib->writing--;
    pthread_cond_broadcast(&lib->read);
    pthread_mutex_unlock(&mudegz);
}

void clear(){
    free(readers);
    free(dividers);
    free(writers);
    free(litterbox);
}

void initLibrary(){
    l.reading = 0;
    l.writing = 0;

    pthread_mutex_init(&mudegz, NULL);
    pthread_cond_init(&l.read, NULL);
    pthread_cond_init(&l.write, NULL);
    lib = &l;
}