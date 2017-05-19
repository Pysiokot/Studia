#include "klient.h"

int main(int argc, char **argv){
    if(argc != 3){
        printf("Bad number of cats");
        return -1;
    }

    int numberOfClients = atoi(argv[1]);
    int numberOfCats = atoi(argv[2]);
    pid_t child;

    while(--numberOfClients){
        child = fork();
        if(child == 0)  numberOfClients = 1;
    }

    semaphores = semget(ftok(SEMAPHORESNAME, SEMAPHORESKEY), 0, 0);

    queue = shmget(ftok(QUEUENAME, QUEUEKEY), 0, 0);

    queueAddress = shmat(queue, NULL, 0);
    atexit(unmapQueue);

    client = queueAddress;
    firstClient = queueAddress+sizeof(pid_t);
    queuen = queueAddress+sizeof(pid_t)+sizeof(int);
    clientsPIDs = queueAddress+sizeof(pid_t)+sizeof(int)*2;


    semaphoreAction.sem_flg = 0;

    while(numberOfCats){
        queueSleep();
        if(*client == 0){
                *client = getpid();
                clock_gettime(CLOCK_MONOTONIC, &ts);
                printf("%d.%.9d:\t%d wakes up Golibroda\n", (int)ts.tv_sec, (int)ts.tv_nsec, getpid());
                golibrodaAwaken();

                queueAwaken();

                workshopOpen();

                jobPerformed();
                numberOfCats--;
                clock_gettime(CLOCK_MONOTONIC, &ts);
                printf("%d.%.9d:\t%d cutted, %d cuts left\n", (int)ts.tv_sec, (int)ts.tv_nsec, getpid(), numberOfCats);
            }
        else{
            int i = *firstClient;
            do{
                if(clientsPIDs[i] == 0){
                    clientsPIDs[i] = getpid();
                    i = -1;
                    break;
                }
                i = (i+1)%(*queuen);
            }while(i != *firstClient);
            queueAwaken();

            if(i == -1){
                clock_gettime(CLOCK_MONOTONIC, &ts);
                printf("%d.%.9d:\t%d gonna wait.\n", (int)ts.tv_sec, (int)ts.tv_nsec, getpid());
                while(true){
                    nextClientSleep();

                    queueSleep();
                    if(*client == getpid()){
                        queueAwaken();

                        workshopOpen();

                        jobPerformed();
                        numberOfCats--;
                        clock_gettime(CLOCK_MONOTONIC, &ts);
                        printf("%d.%.9d:\t%d catted, %d cats to do\n", (int)ts.tv_sec, (int)ts.tv_nsec, getpid(), numberOfCats);
                        break;
                    }
                    else{
                        nextClientAwaken();

                        queueAwaken();
                        usleep(10);
                    }
                }
            }
            else{
                clock_gettime(CLOCK_MONOTONIC, &ts);
                printf("%d.%.9d:\t%d all places are occupied.\n", (int)ts.tv_sec, (int)ts.tv_nsec, getpid());
                usleep(30000);
            }
        }
    }
}

void queueAwaken(){
    semaphoreAction.sem_op = 1;
    semaphoreAction.sem_num = QUEUESEM;
    semop(semaphores, &semaphoreAction, 1);
}

void queueSleep(){
    semaphoreAction.sem_op = -1;
    semaphoreAction.sem_num = QUEUESEM;
    semop(semaphores, &semaphoreAction, 1);
}

void workshopOpen(){
    semaphoreAction.sem_op = 1;
    semaphoreAction.sem_num = WORKSHOP;
    semop(semaphores, &semaphoreAction, 1);
}

void jobPerformed(){
    semaphoreAction.sem_op = -1;
    semaphoreAction.sem_num = JOBSEM;
    semop(semaphores, &semaphoreAction, 1);
}

void golibrodaAwaken(){
    semaphoreAction.sem_op = 1;
    semaphoreAction.sem_num = GOLIBRODASEM;
    semop(semaphores, &semaphoreAction, 1);
}

void nextClientAwaken(){
    semaphoreAction.sem_num = NEXTCLIENT;
    semaphoreAction.sem_op = 1;
    semop(semaphores, &semaphoreAction, 1);
}

void nextClientSleep(){
    semaphoreAction.sem_num = NEXTCLIENT;
    semaphoreAction.sem_op = -1;
    semop(semaphores, &semaphoreAction, 1);
}

void unmapQueue(){
  shmdt(queueAddress);
}