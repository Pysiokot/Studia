#include "task2.h"

int main(int argc, char **argv){
    printf("in main\n");
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
        signal(SIGSTOP, sigusrHandler);
        kill(getpid(), SIGSTOP);
        printf("Signal sent\n");
    }
    else if(taskType < 3){
        kill(getpid(), SIGSTOP);
        printf("Signal sent\n");
    }
    else if(taskType >= 4){
        pthread_kill(thread, SIGSTOP);
        printf("Signal sent\n");
    }

    pthread_join(thread, NULL);

    printf("END");
    return 0;
}

void maskSignal(){
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGSTOP);
    sigprocmask(SIG_SETMASK, &set, NULL);
    printf("Mask ok\n");
}

void sigusrHandler(int signum){
    printf("It's my main handler\n");
}

void * runThread(void *arg){
    printf("In thread\n");
    if(taskType == 3 || taskType == 5)  signal(SIGSTOP, sigusrHandler);
    if(taskType == 4)   maskSignal();
    while(true){}

    return NULL;
}