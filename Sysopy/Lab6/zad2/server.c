#include <ctype.h>
#include <time.h>
#include <signal.h>
#include <mqueue.h>
#include "communication.h"
#include "server.h"


int term = 0; // flag which defines whether the client sends termination request
mqd_t serverQueueID; // file descriptor of server queue
struct client clients[MAX_CLIENTS];
int clientAmount = 0;
int clientUniqID;

int main() {
    clientUniqID = 0;
    atexit(exitHandler);

    signal(SIGINT,sigint_handler);

    puts("Launching CAAAAAAAT...");
    serverQueueID = createQueue();
    puts("CAT RUNNING! Joke, just sleepin'");

    while(!term) {
        recieveMessage();
    }

    return 0;
}

void recieveMessage() {
    struct message msg;
    unsigned int priority = 1;
    if(mq_receive(serverQueueID, (char *) &msg, MAX_MSG_LEN, &priority) == -1) {
        perror("Error while receiving kitty");
    }
    printf("%d messages: %s %s\n",msg.pid,COMMANDS_STR[msg.type],msg.value);
    switch(msg.type) {
        case LOGIN:
            loginHandler(msg);
            break;
        case LOGOUT:
            logoutHandler(msg);
            break;
        case ECHO:
            handleECHO(msg);
            break;
        case UPPER:
            handleUPPER(msg);
            break;
        case TIME:
            handleTIME(msg);
            break;
        case term:
            term = 1;
            break;
        default:
            printf("%d unknown cat\n",msg.pid);
    }
}

void handleUPPER(message msg) {
    for(int i=0;msg.value[i];++i) {
        msg.value[i] = (char) toupper(msg.value[i]);
    }
    sendMessagePID(msg.pid,&msg);
}

void handleECHO(message msg) {
    sendMessagePID(msg.pid,&msg);
}

void handleTIME(message msg) {
    time_t rawtime;
    struct tm *timeinfo;
    time(&rawtime);
    timeinfo = localtime(&rawtime);
    strcpy(msg.value,asctime(timeinfo));
    msg.value[strlen(msg.value)-1] = '\0';
    sendMessagePID(msg.pid,&msg);
}

void exitProgram(int status, char *exitMsq) {
    perror(exitMsq);
    exit(status);
}

mqd_t createQueue() {
    struct mq_attr attr;
    attr.mq_maxmsg = MAX_QUEUE_SIZE;
    attr.mq_msgsize = MAX_MSG_SIZE;
    mqd_t res = mq_open(SERVER_NAME, O_CREAT | O_RDONLY, SERVER_QUEUE_PERM, &attr);
    if(res == -1) {
        exitProgram(EXIT_FAILURE,"Couldn't create server queue");
    }
    return res;
}

void sendMessageQID(mqd_t clientQueueID, message *msg) {
    msg->pid = getpid();
    if(mq_send(clientQueueID,(char *) msg,MAX_MSG_LEN,1) == -1) {
        fprintf(stderr,"%d error while sending kitty to client",msg->pid);
    }
}

mqd_t getClientQueue(pid_t clientPID) {
    mqd_t clientQueueID = -1;
    for(int i = 0; i < clientAmount; ++i) {
        if(clients[i].pid == clientPID) {
            return clients[i].qid;
        }
    }
    return clientQueueID;
}

void sendMessagePID(pid_t clientPID, message *msg) {
    mqd_t clientQueueID = getClientQueue(clientPID);
    if(clientQueueID == -1) {
        fprintf(stderr,"%d not connected, terminating action\n",clientPID);
        return;
    }
    sendMessageQID(clientQueueID,msg);
}

void loginHandler(message msg) {
    pid_t clientPID = msg.pid;
    mqd_t clientQueueID;
    char* clientName = msg.value; // by value we pass client's queue name

    printf("%d is trying to connect...\n",clientPID);
    sleep(1);

    if((clientQueueID = mq_open(clientName, O_WRONLY)) == -1) {
        fprintf(stderr,"%d couldn't open client queue\n", clientPID);
    }

    struct message response;

    if(clientAmount == MAX_CLIENTS) {
        response.type = DECLINE;
        strcpy(response.value,"-1");
        printf("%d couldn't connect: max cats number reached\n",clientPID);
    } else {
        struct client c1;
        c1.pid = clientPID;
        c1.qid = clientQueueID;
        clients[clientAmount++] = c1;

        response.type = ACCEPT;
        snprintf(response.value,MAX_MSG_LEN,"%d",clientUniqID++);
        printf("%d you're on!\n",clientPID);
    }
    sendMessageQID(clientQueueID, &response);
}

void logoutHandler(message msg) {
    mqd_t clientQueueID = getClientQueue(msg.pid);
    pid_t clientPID = msg.pid;
    if(clientQueueID == -1) {
        fprintf(stderr,"%d not connected, terminating action\n",clientPID);
        return;
    }
    int i = 0;
    while(clients[i].pid != clientPID) ++i;
    printf("%d disconnected\n",clientPID);

    --clientAmount;
    while(i<clientAmount) {
        clients[i] = clients[i+1];
        ++i;
    }

    struct message response;
    response.type = LOGOUT;
    strcpy(response.value, "You've successfully logged out");
    sendMessageQID(clientQueueID,&response);
}

long emptyQueue() {
    struct mq_attr attr;
    if(mq_getattr(serverQueueID,&attr) == -1) {
        exitProgram(EXIT_FAILURE,"Error while getting attributes from server queue");
    }
    return attr.mq_curmsgs == 0;
}

void killServer() {
    puts("Killing server.");
    while(!emptyQueue()) {
        recieveMessage();
    }
    struct message response;
    response.type = term;
    strcpy(response.value,"Server killed. You've been logged out.");

    for(int i = 0; i < clientAmount; ++i) {
        sendMessageQID(clients[i].qid,&response);
        mq_close(clients[i].qid);
    }
    mq_close(serverQueueID);
    mq_unlink(SERVER_NAME);
    term = 1;
    puts("Server killed.");
}

void exitHandler() {
    killServer();
}

void sigint_handler(int signum) {
    exit(EXIT_SUCCESS);
}