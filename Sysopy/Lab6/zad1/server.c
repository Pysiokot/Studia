#include <ctype.h>
#include <time.h>
#include <signal.h>
#include "communication.h"
#include "headers.h"

int serverQueue; // id of this server queue
struct client clients[MAX_CLIENTS]; // clients connected to server
int clientAmount;
int clientUniqID;

int main() {
    clientAmount = 0;
    clientUniqID = 0;
    atexit(exit_handler);
    signal(SIGINT,sigint_handler);

    key_t server_key = ftok(getenv(SERVER_PATH), SERVER_ID); // create the special server key
    serverQueue = msgget(server_key, IPC_CREAT | 0600); // create the brand-new server queue

    printf("Server launched.\n");

    struct message msg;
    while(1) {
        if(msgrcv(serverQueue, &msg, MAX_MSG_SIZE, 0, 0) < 0) {
            exit_program(EXIT_FAILURE,"Error while receiving cats from kitties");
            break;
        }
        printf("%d messages: %s %s\n", msg.pid,COMMANDS_STR[msg.type],msg.value);
        handle_message(msg);
        sleep(1);
    }
    return 0;
}

void exit_program(int status, char *exit_message) {
    perror(exit_message);
    exit(status);
}

void exit_handler() {
    msgctl(serverQueue, IPC_RMID, NULL);
}

void loginHandler(struct message msg) {
    key_t client_key;
    int clientQueue;
    pid_t clientPID;

    clientPID = msg.pid;
    client_key = atoi(msg.value);
    printf("%d connecting\n",clientPID);
    sleep(1);

    if((clientQueue = msgget(client_key,0)) == -1) {
        fprintf(stderr,"Couldn't open queue from client with PID: %d: %s",msg.pid,strerror(errno));
        return;
    }
    struct message response;
    response.pid = getpid();

    if(clientAmount == MAX_CLIENTS) {
        response.type = DECLINE;
        strcpy(response.value,"-1");
        printf("%d couldn't connect: max kitties number reached",clientPID);
    } else {
        struct client c1;
        c1.pid = clientPID;
        c1.qid = clientQueue;
        clients[clientAmount] = c1;

        response.type = ACCEPT;
        sprintf(response.value,"%d",clientUniqID++);
        printf("%d connected!\n",clientPID);
        ++clientAmount;
    }
    if(msgsnd(clientQueue,&response,MAX_MSG_SIZE,0) != 0) {
        fprintf(stderr,"Couldn't send cat to %d: %s\n",clientPID,strerror(errno));
    }
}

int getClientQueue(pid_t pid) {
    for(int i = 0; i < clientAmount; ++i) {
        if(pid == clients[i].pid) {
            return clients[i].qid;
        }
    }
    return -1; // error, can't find qid
}

void logoutHandler(struct message msg) {
    pid_t clientPID = msg.pid;
    int clientQueue;
    if((clientQueue = getClientQueue(clientPID)) == -1) {
        fprintf(stderr,"Not connected client %d sent message, terminating action",msg.pid);
        return;
    }
    int i;
    for(i = 0; i < clientAmount; ++i) {
        if(clients[i].pid == clientPID) {
            printf("%d disconnected\n",clientPID);
            break;
        }
    }
    --clientAmount;
    while(i<clientAmount) {
        clients[i] = clients[i+1];
        ++i;
    }
    struct message response;
    response.pid = getpid();
    strcpy(response.value,"You're out");
    response.type = LOGOUT;
    if(msgsnd(clientQueue,&response,MAX_MSG_SIZE,0) != 0) {
        fprintf(stderr,"Couldn't send cat to %d: %s\n",clientPID,strerror(errno));
    }
}
void defaultHandler(struct message msg) {
    int clientQueue;
    if((clientQueue = getClientQueue(msg.pid)) == -1) {
        fprintf(stderr,"Not connected client %d sent message, terminating action",msg.pid);
        return;
    }

    struct message response;
    response.value[0] = '\0';
    response.pid = getpid();
    response.type = NOC;
    if(msgsnd(clientQueue,&response,MAX_MSG_SIZE,0) != 0) {
        fprintf(stderr,"Couldn't send cat to %d: %s\n",msg.pid,strerror(errno));
    }
}
void echoHandler(struct message msg) {
    int clientQueue;
    if((clientQueue = getClientQueue(msg.pid)) == -1) {
        fprintf(stderr,"Not connected client %d sent message, terminating action",msg.pid);
        return;
    }

    struct message response;
    strcpy(response.value,msg.value);
    response.pid = getpid();
    response.type = ECHO;
    if(msgsnd(clientQueue,&response,MAX_MSG_SIZE,0) != 0) {
        fprintf(stderr,"Couldn't send message to %d: %s\n",msg.pid,strerror(errno));
    }
}
void upperHandler(struct message msg) {
    int clientQueue;
    if((clientQueue = getClientQueue(msg.pid)) == -1) {
        fprintf(stderr,"Can't find clientQueueID %d",msg.pid);
        return;
    }

    struct message response;
    strcpy(response.value,msg.value);

    for(int i = 0; response.value[i]; ++i) {
        response.value[i] = (char) toupper(response.value[i]);
    }

    response.pid = getpid();
    response.type = UPPER;
    if(msgsnd(clientQueue,&response,MAX_MSG_SIZE,0) != 0) {
        fprintf(stderr,"Couldn't send cat to %d: %s\n",msg.pid,strerror(errno));
    }
}

void timeHandler(struct message msg) {
    int clientQueue;
    if((clientQueue = getClientQueue(msg.pid)) == -1) {
        fprintf(stderr,"Can't find clientQueueID %d",msg.pid);
        return;
    }

    time_t rawtime;
    struct tm *timeinfo;
    time(&rawtime); // seconds since epoch
    timeinfo = localtime(&rawtime); // local time-zone

    struct message response;
    strcpy(response.value,asctime(timeinfo)); // pretty-formatted string
    response.pid = getpid();
    response.type = TIME;
    if(msgsnd(clientQueue, &response, MAX_MSG_SIZE, 0) != 0) {
        fprintf(stderr,"Couldn't send cat to %d: %s\n",msg.pid,strerror(errno));
    }
}

void termHandler(struct message trigger_msg) {
    printf("Killing serv\n");
    while(1) {
        struct msqid_ds stats;
        struct message msg;
        msgctl(serverQueue, IPC_STAT, &stats);

        printf("Number of cats to proccess: %zu\n",stats.msg_qnum);
        if(msgrcv(serverQueue, &msg, MAX_MSG_SIZE, 0, IPC_NOWAIT) < 0) {
            break;
        }
        printf("%d messages: %s\n",msg.pid,msg.value);
        handle_message(msg);
    }
    struct message response;
    response.pid = getpid();
    response.type = TERMINATE;
    strcpy(response.value,"Server killed.");

    for(int i = 0; i < clientAmount; ++i) {
        if(msgsnd(clients[i].qid, &response, MAX_MSG_SIZE, 0) != 0) {
            fprintf(stderr,"Couldn't send cat to %d: %s\n",clients[i].pid,strerror(errno));
        }
        kill(clients[i].pid,SIGRTMIN);
    }

    printf("Killing server\n");
    exit(EXIT_SUCCESS);
}

void handle_message(struct message msg) {
    switch(msg.type) {
        case TERMINATE:
            termHandler(msg);
            break;
        case TIME:
            timeHandler(msg);
            break;
        case ECHO:
            echoHandler(msg);
            break;
        case LOGIN:
            loginHandler(msg);
            break;
        case LOGOUT:
            logoutHandler(msg);
            break;
        case UPPER:
            upperHandler(msg);
            break;
        default:
            defaultHandler(msg);
            break;
    }
}

void sigint_handler(int signum) {
    for(int i = 0; i < clientAmount; ++i) {
        kill(clients[i].pid,SIGRTMIN);
    }
    printf("Server killed :c\n");
    exit(EXIT_SUCCESS);
}