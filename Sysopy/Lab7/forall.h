#ifndef FORALL_H
#define FORALL_H

#define isOk(x, errval) \
    do { \
        if ((x) == (errval)) { \
            fprintf(stderr, "%s:%d: ", __func__, __LINE__); \
            perror(#x); \
            exit(-1); \
        } \
    } while (0) \

const int SEMAPHORESKEY = 1;
const char *SEMAPHORESNAME = "/GOLIBRODA";
const int SEMAPHORESNUM = 5;
const char *QUEUENAME = "/QUEUEGOLIBRODA";
const int QUEUEKEY = 1;

union semun {
    int              val;    /* Value for SETVAL */
    struct semid_ds *buf;    /* Buffer for IPC_STAT, IPC_SET */
    unsigned short  *array;  /* Array for GETALL, SETALL */
    struct seminfo  *__buf;  /* Buffer for IPC_INFO
                                (Linux-specific) */
};

enum semaphoresnames {
  QUEUESEM,
  WORKSHOP,
  JOBSEM,
  NEXTCLIENT,
  GOLIBRODASEM
};

#endif //FORALL_H
