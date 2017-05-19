#include "divNull.h"

int main(int argc, char **argv){
    printf("Creating thread\n");
    pthread_t thread;
    pthread_create(&thread, NULL, &runThread, (void *)NULL);
    
    usleep(2000);

    printf("Creating killer\n");
    pthread_t myKiller;
    pthread_create(&thread, NULL, &threadKiller, (void *)NULL);

    sleep(1);

    pthread_join(thread, NULL);
    pthread_join(myKiller, NULL);

    return 0;
}

void *runThread(void *arg){
    while(true) printf("on the run\n");
}

void *threadKiller(void *arg){
    printf("Lel %d\n", 2/0);
}