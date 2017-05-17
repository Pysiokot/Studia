#include "golibroda.h"
#include "forall.h"

int main(int argc, char **argv){
    if(argc != 2){
        printf("Bad number of cats");
        return -1;
    }
    signal(SIGINT, sigintHandler);

    numberOfPlaces = atoi(argv[1]);
    createSemaphores();

    *client = queueAddress;
    *firstClient = queueAddress+sizeof(pid_t);
    *queuePlaces = queueAddress+sizeof(pid_t)+sizeof(int);
    *clientsPIDs = queueAddress+sizeof(pid_t)+sizeof(int)*2;
    *client = 0;
    *firstClient = 0;
    *queuePlaces = numberOfPlaces;
    for(int i = 0; i < numberOfPlaces; i++)
        clientsPIDs[i] = 0;

    semaphoreAction.sem_flg = 0;
    queueAwaken();

    dayTime();
}

void closeSemaphores(){
  semctl(semaphores, 0, IPC_RMID);
}

void closeQueue(){
  shmctl(queue, IPC_RMID, NULL);
}

void unmapQueue(){
  shmdt(queueAddress);
}

void sigintHandler(int signo){
  exit(0);
}

void createSemaphores(){
    semaphores = semget(ftok(SEMAPHORESNAME, SEMAPHORESKEY), SEMAPHORESNUM, IPC_CREAT | 0666);
    CHECK(semaphores, -1);
    atexit(closeSemaphores);

    union semun sunion;
    sunion.val = 0;
    for(int i = 0; i < SEMAPHORESNUM; i++) CHECK(semctl(semaphores, i, SETVAL, sunion), -1);

    queue = semget(ftok(QUEUENAME, QUEUEKEY), (numberOfPlaces+1)*sizeof(pid_t) + sizeof(int)*2, IPC_CREAT | 0666);
    CHECK(queue, -1);
    atexit(closeQueue);

    queueAddress = shmat(queue, NULL, 0);
    CHECK(queueAddress, (void*)-1);
    atexit(unmapQueue);
}

void dayTime(){
    while(1){
        queueSleep();
        if(*client != 0){
            pid_t currentClient = *client;
            queueAwaken();

            worshopClosed();
            clock_gettime(CLOCK_MONOTONIC, &ts);
            printf("%ld.%.9ld:\tKitki birzi si di prici %d\n", (long)ts.tv_sec, (long)ts.tv_nsec, currentClient);
            usleep(3000);

            queueSleep();
            *client = -1;
            queueAwaken();

            golibrodaNotBusy();
            clock_gettime(CLOCK_MONOTONIC, &ts);
            printf("%ld.%.9ld:\tKitki tni wlisi wszistkim %d\n", (long)ts.tv_sec, (long)ts.tv_nsec, currentClient);
        }
        if(clientsPIDs[*firstClient] == 0){
            *client = 0;

            queueAwaken();
            clock_gettime(CLOCK_MONOTONIC, &ts);
            printf("%ld.%.9ld:\tKitki idi spic <3.\n", (long)ts.tv_sec, (long)ts.tv_nsec);

            golibrodaSleep();
        }
        else{
            *client = clientsPIDs[*firstClient];
            clientsPIDs[*firstClient] = 0;
            *firstClient = ((*firstClient)+1)%(*queuePlaces);
            queueAwaken();
            nextClientAwaken();
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

void worshopClosed(){
    semaphoreAction.sem_op = -1;
    semaphoreAction.sem_num = WORKSHOP;
    semop(semaphores, &semaphoreAction, 1);
}

void golibrodaNotBusy(){
    semaphoreAction.sem_op = 1;
    semaphoreAction.sem_num = JOBSEM;
    semop(semaphores, &semaphoreAction, 1);
}

void golibrodaSleep(){
    semaphoreAction.sem_op = -1;
    semaphoreAction.sem_num = GOLIBRODASEM;
    semop(semaphores, &semaphoreAction, 1);
}

void nextClientAwaken(){
    semaphoreAction.sem_num = NEXTCLIENT;
    semaphoreAction.sem_op = 1;
    semop(semaphores, &semaphoreAction, 1);
}