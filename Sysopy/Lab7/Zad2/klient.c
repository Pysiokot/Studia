#include "klient.h"

int main(int argc, char **argv){
    if(argc != 3){
        printf("Bad number of argumentz");
        return -1;
    }

    int numberOfClients = atoi(argv[1]);
    int numberOfCats = atoi(argv[2]);
    pid_t child;
    while(--numberOfClients){
        child = fork();
        if(child == 0)  numberOfClients = 1;
    }

    sem_t **golibrodaSleep = semaphores;
    *golibrodaSleep = sem_open(GOLISEMAPHORE, 0);

    sem_t **workshop = semaphores+sizeof(sem_t*);
    *workshop = sem_open(WORKSEMAPHORE, 0);

    sem_t **lobbySemaphore = semaphores+sizeof(sem_t*)*2;
    *lobbySemaphore = sem_open(LOBBYSEMAPHORE, 0);

    sem_t **workPerformed = semaphores+sizeof(sem_t*)*3;
    *workPerformed = sem_open(WORKPERFORMED, 0);

    sem_t **nextClientSemaphore = semaphores+sizeof(sem_t*)*4;
    *nextClientSemaphore = sem_open(NEXTCLIENT, 0);
    atexit(closeSemaphores);


    int queue = shm_open(LOBBY, O_RDWR, 0666);

    lobbySize = sizeof(pid_t) + sizeof(int)*2;
    queueAddress = mmap(NULL, lobbySize, PROT_WRITE|PROT_READ, MAP_SHARED, queue, 0);
    atexit(unmapQueue);

    int *queuen = queueAddress+sizeof(pid_t)+sizeof(int);
    sem_wait(*lobbySemaphore);
    numberOfClients = *queuen;
    sem_post(*lobbySemaphore);
    unmapQueue();

    lobbySize = (numberOfClients+1)*sizeof(pid_t) + sizeof(int)*2;
    queueAddress = mmap(NULL, lobbySize, PROT_WRITE|PROT_READ, MAP_SHARED, queue, 0);

    pid_t *client = queueAddress;
    int *firstClient = queueAddress+sizeof(pid_t);
    pid_t *clientsPIDs = queueAddress+sizeof(pid_t)+sizeof(int)*2;

    struct timespec ts;
    while(numberOfCats){
        sem_wait(*lobbySemaphore);
        if(*client == 0){
            *client = getpid();
            clock_gettime(CLOCK_MONOTONIC, &ts);
            printf("%d.%.9d:\t%d wakes up Golibroda\n", (int)ts.tv_sec, (int)ts.tv_nsec, getpid());
            sem_post(*workshop);
            sem_post(*lobbySemaphore);
            sem_post(*golibrodaSleep);
            sem_wait(*workPerformed);
            clock_gettime(CLOCK_MONOTONIC, &ts);
            printf("%d.%.9d:\t%d cutted, %d cuts left\n", (int)ts.tv_sec, (int)ts.tv_nsec, getpid(), numberOfCats--);
        }
        else{
            int i = *firstClient;
            do{
                if(clientsPIDs[i] == 0){
                    clientsPIDs[i] = getpid();
                    i = -1;
                    break;
                }
                i = (i+1)%(numberOfClients);
            }while(i != *firstClient);
            sem_post(*lobbySemaphore);
            if(i != -1){
                clock_gettime(CLOCK_MONOTONIC, &ts);
                printf("%d.%.9d:\t%d no places in lobby.\n", (int)ts.tv_sec, (int)ts.tv_nsec, getpid());
                usleep(30000);
            }
            else{
                clock_gettime(CLOCK_MONOTONIC, &ts);
                printf("%d.%.9d:\t%d gonna wait.\n", (int)ts.tv_sec, (int)ts.tv_nsec, getpid());
                while(true){
                    sem_wait(*nextClientSemaphore);
                    sem_wait(*lobbySemaphore);
                    if(getpid() == *client){
                        sem_post(*workshop);
                        sem_post(*lobbySemaphore);
                        sem_wait(*workPerformed);
                        clock_gettime(CLOCK_MONOTONIC, &ts);
                        printf("%d.%.9d:\t%d done, %d to do\n", (int)ts.tv_sec, (int)ts.tv_nsec, getpid(), numberOfCats--);
                        break;
                    }
                    else{
                        sem_post(*lobbySemaphore);
                        sem_post(*nextClientSemaphore);
                    }
                }
            }
        }
    }


    return 0;
}

void closeSemaphores(){
  for(int i = 0; i < SEMAPHOREQTY; ++i)
    sem_close(semaphores[i]);
}

void closeQueue(){
  shm_unlink(LOBBY);
}

void unmapQueue(){
  munmap(queueAddress, lobbySize);
}