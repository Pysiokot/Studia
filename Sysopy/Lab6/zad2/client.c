#include <limits.h>
#include <signal.h>
#include <mqueue.h>
#include "communication.h"
#include "client.h"

mqd_t privateQueueID;
mqd_t serverQueueID;
char name[NAME_MAX];
int logout = 0;
struct sigevent sig;

int main() {
    atexit(handleExit);
    privateQueueID = makeQueue();

    puts("Connecting to server..");
    serverQueueID = getServQueue();

    int IDrecieved = getIDFromServ();
    puts("Connected!");
    printf("My server-id: %d\n",IDrecieved);

    struct sigaction sigact;
    sigemptyset(&(sigact.sa_mask));
    sigact.sa_handler = handleSignal;
    if(sigaction(SIGRTMIN,&sigact,NULL) == -1) {
        exitProgram(EXIT_FAILURE,"Couldn't set handler for sgrtmin signal");
    }
    struct sigaction sigact2;
    sigemptyset(&(sigact2.sa_mask));
    sigact2.sa_handler = handleSigint;
    if(sigaction(SIGINT,&sigact2,NULL) == -1) {
        exitProgram(EXIT_FAILURE,"Couldn't set handler for sigint signal");
    }

    sig.sigev_notify = SIGEV_SIGNAL;
    sig.sigev_signo = SIGRTMIN;
    if(mq_notify(privateQueueID,&sig) == -1) { // prepare for getting responses from server
        exitProgram(EXIT_FAILURE,"Error while setting up mq_notify");
    }
    struct message msg;
    while(!logout) {
        if(readLine(&msg) == -1) // fulfill the msg struct from stdin
            continue;
        if(sendMessage(&msg) == -1) // send it to server
            continue;
    }
    return 0;
}

void exitProgram(int status, char *exitMessage) {
    if(status == EXIT_FAILURE) {
        perror(exitMessage);
    } else {
        fprintf(stderr,"%s\n",exitMessage);
    }
    exit(status);
}

void handleExit() {
    mq_close(privateQueueID);
    mq_unlink(name);
}

void handleSignal(int signum) {
    struct message msg;
    do { // at least one message is in queue, this which triggered this function, so for safety - do..while
        if (recieveMessage(&msg) == -1) {// receive response from server
            perror("Couldn't receive message from server");
        } else if (msg.type == LOGOUT || msg.type == TERMINATE) {
            logout = 1;
        }
    } while (!isQueueEmpty());
    if (mq_notify(privateQueueID, &sig) == -1) { // prepare for getting responses from server
        exitProgram(EXIT_FAILURE, "Error while setting up mq_notify");
    }
}

void handleSigint(int signum) {
    struct message msg;
    msg.type = LOGOUT;
    msg.value[0] = '\0';
    sendMessage(&msg);
}

mqd_t makeQueue() {
    struct mq_attr attr;
    attr.mq_maxmsg = MAX_QUEUE_SIZE;
    attr.mq_msgsize = MAX_MSG_SIZE;

    snprintf(name, NAME_MAX, "/client-%d", getpid()); // write at most NAME_MAX bytes

    mqd_t res = mq_open(name, O_CREAT | O_RDONLY, SERVER_QUEUE_PERM, &attr);
    if(res == -1)
        exitProgram(EXIT_FAILURE, "Couldn't create private queue");

    return res;
}

mqd_t getServQueue() {
    mqd_t res = mq_open(SERVER_NAME, O_WRONLY);
    if(res == -1)
        exitProgram(EXIT_SUCCESS,"Couldn't establish connection with server");
    return res;
}

int sendMessage(message *msg) {
    msg->pid = getpid();

    if(mq_send(serverQueueID,(char *) msg,MAX_MSG_LEN,1) == -1) {
        perror("Error while sending message to server");
        return -1;
    }
    return 0;
}

long isQueueEmpty() {
    struct mq_attr attr;
    mq_getattr(privateQueueID,&attr);
    return attr.mq_curmsgs == 0;
}

int recieveMessage(message *msg) {
    unsigned int priority = 1;
    ssize_t receivedBytes = mq_receive(privateQueueID,(char *) msg,MAX_MSG_LEN,&priority);
    if(receivedBytes == -1) {
        perror("Error while receiving message from server");
        return -1;
    }
    printf("Received from server: %s\n",msg->value);
    return 0;
}

int getIDFromServ() {
    message msg;
    msg.type = LOGIN;
    strcpy(msg.value,name);

    sendMessage(&msg);
    recieveMessage(&msg);

    if(msg.type == DECLINE) {
        exitProgram(EXIT_SUCCESS,"Couldn't establish connection with server");
    }

    return atoi(msg.value);
}

int readLine(message *msg) {
    char line[MAX_MSG_LEN];

    ssize_t buffor = read(0,line,MAX_MSG_LEN);
    if(buffor == -1) {
        if(errno == EINTR) { // if interrupted by income message - return
            return -1;
        }
        perror("Error while reading line from stdin");
    }
    if(buffor < MAX_MSG_LEN)
        line[buffor] = '\0'; // preventing from unwanted characters left in line variable after the last read (faster than using callock)

    char type[10]; // type string (eg. "ECHO", "TIME", etc.)
    char *p;
    if((p = strtok(line," ")) == NULL) {
        fprintf(stderr,"Not valid message. Please pass type (ECHO, UPPER, TIME, TERMINATE, LOGOUT) and after white-space character the message itself.\n");
        return -1;
    }
    if(p[strlen(p)-1]=='\n') // this is a case when TIME (or LOGOUT/TERMINATE) is used as a single command
        p[strlen(p)-1] = '\0';
    strcpy(type,p);

    msg->type = NOC;
    for(int i=0;i<sizeof(COMMANDS_STR)/sizeof(COMMANDS_STR[0]) - 1;++i) { // -1 because we do not want clients to pass LOGIN type
        if(strcmp(type,COMMANDS_STR[i])==0) {
            msg->type = COMMANDS_ENUM[i];
            break;
        }
    }

    if(msg->type == NOC) {
        fprintf(stderr,"Not valid message. Please pass type (ECHO, UPPER, TIME, TERMINATE, LOGOUT) and after white-space character the message itself.\n");
        return -1;
    }

    if((p = strtok(NULL,"\n")) == NULL) {
        msg->value[0] = '\0';
    } else {
        strcpy(msg->value,p);
    }
    return 0;
}