#include "golibroda.h"

int main(int argc, char **argv){
    if(argc != 2){
        printf("Bad number of args");
        return -1;
    }
    signal(SIGINT, sigintHandler);
    int numberOfClients = atoi(argv[1]);

    int lobby = shm_open(LOBBY, O_CREAT|O_RDWR, 0666);
    lobbySize = (numberOfClients + 1)*sizeof(pid_t) + sizeof(int)*2;
    ftruncate(lobby, lobbySize);
    lobbyAddress = mmap(NULL, lobbySize, PROT_WRITE|PROT_READ, MAP_SHARED, lobby, 0);
    atexit(closeLobby);
    atexit(unmapLobby);

    pid_t *client = lobbyAddress;
    int *firstClient = lobbyAddress+sizeof(pid_t);
    int *lobbyCapacity = lobbyAddress+sizeof(pid_t)+sizeof(int);
    pid_t *clientsPIDs = lobbyAddress+sizeof(pid_t)+sizeof(int)*2;
    *client = 0;
    *firstClient = 0;
    *lobbyCapacity = numberOfClients;
    for(int i = 0; i < numberOfClients; i++)  clientsPIDs[i] = 0;

    sem_t **golibrodaSleep = semaphores;
    *golibrodaSleep = sem_open(GOLISEMAPHORE, O_CREAT, 0666, 0);

    sem_t **workshop = semaphores+sizeof(sem_t*);
    *workshop = sem_open(WORKSEMAPHORE, O_CREAT, 0666, 0);

    sem_t **lobbySemaphore = semaphores+sizeof(sem_t*)*2;
    *lobbySemaphore = sem_open(LOBBYSEMAPHORE, O_CREAT, 0666, 0);

    sem_t **workPerformedSemaphore = semaphores+sizeof(sem_t*)*3;
    *workPerformedSemaphore = sem_open(WORKPERFORMED, O_CREAT, 0666, 0);

    sem_t **nextClientSemaphore = semaphores+sizeof(sem_t*)*4;
    *nextClientSemaphore = sem_open(NEXTCLIENT, O_CREAT, 0666, 0);
    atexit(closeAllSemaphores);


    sem_post(*lobbySemaphore);

    while(true){
        sem_wait(*lobbySemaphore);
        if(clientsPIDs[*firstClient] == 0){
            *client = 0;
            sem_post(*lobbySemaphore);
            clock_gettime(CLOCK_MONOTONIC, &ts);
            printf("%d.%.9d:\tSpanko time.\n", (int)ts.tv_sec, (int)ts.tv_nsec);
            sem_wait(*golibrodaSleep);
        }
        else{
            *client = clientsPIDs[*firstClient];
            clientsPIDs[*firstClient] = 0;
            *firstClient = ((*firstClient)+1)%(*lobbyCapacity);
            sem_post(*lobbySemaphore);
            sem_post(*nextClientSemaphore);
        }
        if(*client != 0){
            pid_t currentClient = *client;
            sem_wait(*workshop);
            sem_post(*lobbySemaphore);
            clock_gettime(CLOCK_MONOTONIC, &ts);
            printf("%d.%.9d:\tCutting time! %d\n", (int)ts.tv_sec, (int)ts.tv_nsec, currentClient);
            usleep(10000);

            sem_wait(*lobbySemaphore);
            *client = -1;
            sem_post(*lobbySemaphore);

            sem_post(*workPerformedSemaphore);
            clock_gettime(CLOCK_MONOTONIC, &ts);
            printf("%d.%.9d:\tEnd %d\n", (int)ts.tv_sec, (int)ts.tv_nsec, currentClient);
        }
    }
    return 0;
}

void closeAllSemaphores(){
    for(int i = 0; i < SEMAPHOREQTY; ++i) sem_close(semaphores[i]);
}

void closeLobby(){
    shm_unlink(LOBBY);
}

void unmapLobby(){
    munmap(lobbyAddress, lobbySize);
}

void sigintHandler(int signo){
    exit(0);
}