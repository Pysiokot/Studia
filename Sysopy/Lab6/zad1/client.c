#include <signal.h>
#include "communication.h"
#include "clientHeaders.h"
key_t privateQueue; // client's private queue for receiving messages

int main() {
    atexit(exitHandler);
    signal(SIGRTMIN,signalHandler);

    int id_from_server;

    key_t servID;
    key_t privID;

    if((servID = ftok(getenv(SERVER_PATH),SERVER_ID)) == -1) {
        endProgram(EXIT_FAILURE,"Error while generating the server key");
    }
    if((privID = ftok(".",getpid())) == -1) {
        endProgram(EXIT_FAILURE,"Error while generating the private key");
    }

    int servQueue;

    if((servQueue = msgget(servID,0)) == -1) {
        endProgram(EXIT_FAILURE,"Error while retrieving queue from server");
    }
    if((privateQueue = msgget(privID, IPC_CREAT | 0600)) == -1) {
        endProgram(EXIT_FAILURE,"Error while creating private queue");
    }


    // send private key to server
    struct message msg;
    msg.type = LOGIN;
    msg.pid = getpid();
    printf("Sending request to server..\n");
    sprintf(msg.value,"%d",privID);
    if(msgsnd(servQueue, &msg, MAX_MSG_SIZE, 0) != 0) {
        fprintf(stderr,"Couldn't send message to server: %s\n",strerror(errno));
    }
    // receive id from server
    if(msgrcv(privateQueue, &msg, MAX_MSG_SIZE, 0, 0) < 0) {
        endProgram(EXIT_FAILURE,"Error while receiving id from server");
    }

    if(msg.type == DECLINE) {
        endProgram(EXIT_FAILURE, "Couldn't connect to server");
    }

    id_from_server = atoi(msg.value);
    printf("Received id from server: %d\n",id_from_server);

    printf("\nEnter one of these commands: [ECHO, UPPER, TIME, TERMINATE, LOGOUT]\n\n");

    goAndPlay(msg, servQueue);

    return 0;
}

void goAndPlay(struct message msg, int servQueue){
     while(1) {
        char line[MAX_MSG_LEN];
        char type[10];
        msg.type = NOC;
        msg.pid = getpid();
        fgets(line,MAX_MSG_LEN,stdin);
        char *p = NULL;
        if((p = strtok(line," ")) == NULL) {
            fprintf(stderr,"Not valid message. Please pass type (ECHO, UPPER, TIME, TERMINATE, LOGOUT) and after white-space character the message itself.\n");
            continue;
        }
        if(p[strlen(p)-1]=='\n') p[strlen(p)-1] = '\0';  // this is a case when TIME (or LOGOUT/TERMINATE) is used as a single command
        strcpy(type,p);

        for(int i=0;i<sizeof(COMMANDS_STR)/sizeof(COMMANDS_STR[0]) - 1;++i) { // -1 because we do not want clients to pass LOGIN type
            if(strcmp(type,COMMANDS_STR[i])==0) {
                msg.type = COMMANDS_ENUM[i];
            }
        }

        if(msg.type == NOC) {
            fprintf(stderr,"Not valid message. Please pass type (ECHO, UPPER, TIME, TERMINATE, LOGOUT) and after white-space character the message itself.\n");
            continue;
        }

        if((p = strtok(NULL,"\n")) == NULL) {
            msg.value[0] = '\0';
        } else {
            strcpy(msg.value,p);
        }

        if(msgsnd(servQueue, &msg, MAX_MSG_SIZE, 0)!=0) {
            fprintf(stderr,"Couldn't send message to server: %s\n",strerror(errno));
        }

        if(msgrcv(privateQueue, &msg, MAX_MSG_SIZE, 0, 0) < 0) {
            fprintf(stderr,"Error while receiving message from server: %s\n",strerror(errno));
        }

        printf("Received from server: %s\n",msg.value);

        if(msg.type == LOGOUT || msg.type == TERMINATE) {
            break;
        }
    }
}

void endProgram(int status, char *exit_message) {
    perror(exit_message);
    exit(status);
}

void exitHandler() {
    msgctl(privateQueue, IPC_RMID, NULL);
}

// I use this signal, because can't interrupt waiting fgets() otherway
void signalHandler(int signum) {
    fprintf(stdout,"Server terminated. You've succesfully logged out.\n");
    exit(EXIT_SUCCESS);
}