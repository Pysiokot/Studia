#include "task2.h"

int main(int argc, char **argv){
    if(argc != 2){
        printf("Gimmeh task type!!!!");
        return 1;
    }

    taskType = atoi(argv[1]);

    if(taskType == 2){
        maskSignal();
    }

    pthread_t thread;
	pthread_create(&thread, NULL, &runThread, (void*) NULL);
	printf("Thread created\n");

    sleep(10);

    if(taskType == 3){
        signal(SIGUSR1, sigusrHandler);
        kill(getpid(), SIGUSR1);
    }
    else if(taskType < 3){
        kill(getpid(), SIGUSR1);
    }
    else if(taskType >= 4){
        pthread_kill(thread, SIGUSR1);
    }

    pthread_join(thread, NULL);

    printf("END");
    return 0;
}

void maskSignal(){
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGUSR1);
    sigprocmask(SIG_SETMASK, &set, NULL);
    printf("Mask ok\n");
}

void sigusrHandler(int signum){
    printf("PID: %d, Thread ID: %d", getpid(), (int)pthread_self());
}

void * runThread(void *arg){
    if(taskType == 3 || taskType == 5)  signal(SIGUSR1, sigusrHandler);
    if(taskType == 4)   maskSignal();
    while(true){}

    return NULL;
}