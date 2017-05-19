#include "task1.h"

int main(int argc, char **argv){
    if(argc != 6){
        printf("Bad number of args");
        return -1;
    }

    int threadNum = atoi(argv[1]);
    char *fileName = argv[2];
    int recordsNum = atoi(argv[3]);
    char *myWord = argv[4];
    taskType = atoi(argv[5]);

    int myFile = open(fileName, O_RDONLY);
    if(myFile < 0){
        printf("File open error)");
        return -1;
    }

    pthread_t *threads;
    threads = malloc(sizeof(pthread_t)*(threadNum+1));
    pthread_attr_t threadAttr;
    pthread_attr_init(&threadAttr);
    if(taskType == 3)   pthread_attr_setdetachstate(&threadAttr, PTHREAD_CREATE_DETACHED);

    threadProperties threadProps;
    threadProps.records = recordsNum;
    threadProps.file = myFile;
    threadProps.search = myWord;
    threadProps.threads = threads;
    if(taskType == 3){
        threadProps.whatToDo = nothing;
    }
    else{
        threadProps.whatToDo = killAll;
    }
    int i;
    for(i = 0; i < threadNum; i++){
        int res = pthread_create(threads + i, &threadAttr, jobToDo, &threadProps);
        if(res != 0){
            printf("Erro while creating thread");
            return -1;
        }
    }
    threads[i] = -1;

    if(taskType == 3){
        do{
            usleep(1000);
        } while(!isFileReaded);
    }
    else{
        int j = 0;
        while(j < threadNum){
            pthread_join(threads[j++], NULL);
        }
    }

    close(myFile);
    return 0;
}

void nothing(pthread_t *thread){
    printf("Nic nie robię XD");
}

void killAll(pthread_t *thread){
    for(int i = 0; thread[i] != -1; i++){
        if(thread[i] != pthread_self())
            pthread_cancel(thread[i]);
    }
    pthread_exit(0);
}

void jobToDo(void *arg){
    if(taskType == 3)   pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
    if(taskType == 2)   pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, NULL);
    if(taskType == 1)   pthread_setcanceltype(PTHREAD_CANCEL_DISABLE, NULL);

    threadProperties *threadProps = arg;

    int length = strlen(threadProps->search);
    int myBufferSize = threadProps->records * 1024;
    char *myBuffer = malloc(myBufferSize);
    pthread_cleanup_push(free, myBuffer);
    bool canRead = true;
    int readed = read(threadProps->file, myBuffer, myBufferSize);
    if(readed <= 0) canRead = false;
    while(canRead){
        for(int i = 0; i < readed; i+=1024){
            for(int j = 4; j < 1024; j++){
                if(strncmp(threadProps->search, myBuffer+i+j, length) == 0){
                    printf("I found it!!!!! My num: %d, line: %d\n", (int)pthread_self(), (int)myBuffer[i]);
                }
            }
        }
        if(taskType == 2)
            pthread_testcancel();
        readed = read(threadProps->file, myBuffer, myBufferSize);
        if(readed <= 0) canRead = false;
    }
    isFileReaded = true;
    pthread_cleanup_pop(1);
    return;
}