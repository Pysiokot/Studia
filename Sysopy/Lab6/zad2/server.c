//
// Created by Mrz355 on 19.04.17.
//

#include <ctype.h>
#include <time.h>
#include <signal.h>
#include <mqueue.h>
#include "communication.h"

struct client {
    pid_t pid;
    mqd_t qid;
};

int terminate = 0; // flag which defines whether the client sends termination request
mqd_t server_qid; // file descriptor of server queue
struct client clients[MAX_CLIENTS];

void receive_message();

int client_count = 0;
int client_unique_id;

void exit_program(int status, char *exit_message) {
    perror(exit_message);
    exit(status);
}

mqd_t create_queue() {
    struct mq_attr attr;
    attr.mq_maxmsg = MAX_QUEUE_SIZE;
    attr.mq_msgsize = MAX_MSG_SIZE;

    mqd_t res = mq_open(SERVER_NAME, O_CREAT | O_RDONLY, SERVER_QUEUE_PERM, &attr);
    if(res == -1) {
        exit_program(EXIT_FAILURE,"Couldn't create server queue");
    }

    return res;
}

void send_message_qid(mqd_t client_qid, message *msg) {
    msg->pid = getpid();
    if(mq_send(client_qid,(char *) msg,MAX_MSG_LEN,1) == -1) {
        fprintf(stderr,"%d error while sending message to client",msg->pid);
    }
}
mqd_t get_client_qid(pid_t client_pid) {
    mqd_t client_qid = -1;
    for(int i=0;i<client_count;++i) {
        if(clients[i].pid == client_pid) {
            return clients[i].qid;
        }
    }
    return client_qid;
}
void send_message_pid(pid_t client_pid, message *msg) {
    mqd_t client_qid = get_client_qid(client_pid);
    if(client_qid == -1) {
        fprintf(stderr,"%d not connected, terminating action\n",client_pid);
        return;
    }
    send_message_qid(client_qid,msg);
}

void login_handler(message msg) {
    pid_t client_pid = msg.pid;
    mqd_t client_qid;
    char* client_name = msg.value; // by value we pass client's queue name

    printf("%d is trying to connect...\n",client_pid);
    sleep(1);

    if((client_qid = mq_open(client_name, O_WRONLY)) == -1) {
        fprintf(stderr,"%d couldn't open client queue\n", client_pid);
    }

    struct message response;

    if(client_count == MAX_CLIENTS) {
        response.type = DECLINE;
        strcpy(response.value,"-1");
        printf("%d couldn't connect: max clients number reached\n",client_pid);
    } else {
        struct client c1;
        c1.pid = client_pid;
        c1.qid = client_qid;
        clients[client_count++] = c1;

        response.type = ACCEPT;
        snprintf(response.value,MAX_MSG_LEN,"%d",client_unique_id++);
        printf("%d connected!\n",client_pid);
    }
    send_message_qid(client_qid, &response);
}

void logout_handler(message msg) {
    mqd_t client_qid = get_client_qid(msg.pid);
    pid_t client_pid = msg.pid;
    if(client_qid == -1) {
        fprintf(stderr,"%d not connected, terminating action\n",client_pid);
        return;
    }
    int i = 0;
    while(clients[i].pid != client_pid) ++i;
    printf("%d disconnected\n",client_pid);

    --client_count;
    while(i<client_count) {
        clients[i] = clients[i+1];
        ++i;
    }

    struct message response;
    response.type = LOGOUT;
    strcpy(response.value, "You've successfully logged out");
    send_message_qid(client_qid,&response);
}

void echo_handler(message msg) {
    send_message_pid(msg.pid,&msg);
}
void upper_handler(message msg) {
    for(int i=0;msg.value[i];++i) {
        msg.value[i] = (char) toupper(msg.value[i]);
    }
    send_message_pid(msg.pid,&msg);
}
void time_handler(message msg) {
    time_t rawtime;
    struct tm *timeinfo;
    time(&rawtime); // seconds since epoch
    timeinfo = localtime(&rawtime); // converting to local time-zone seconds

    strcpy(msg.value,asctime(timeinfo)); // pretty-formatted string

    msg.value[strlen(msg.value)-1] = '\0'; // getting rid of additional not needed /n (asctime adds one)

    send_message_pid(msg.pid,&msg);
}
long queue_empty() {
    struct mq_attr attr;
    if(mq_getattr(server_qid,&attr) == -1) {
        exit_program(EXIT_FAILURE,"Error while getting attributes from server queue");
    }
    return attr.mq_curmsgs == 0;
}
void terminate_server() {
    puts("Terminating server. Responding to queued messages..");
    while(!queue_empty()) {
        receive_message();
    }
    struct message response;
    response.type = TERMINATE;
    strcpy(response.value,"Server terminated. You've been logged out.");

    for(int i=0;i<client_count;++i) {
        send_message_qid(clients[i].qid,&response);
        mq_close(clients[i].qid);
    }
    mq_close(server_qid);
    mq_unlink(SERVER_NAME);
    terminate = 1;
    puts("Server terminated.");
}

void exit_handler() {
    terminate_server();
}

void receive_message() {
    unsigned int priority = 1;
    struct message msg;
    ssize_t received_bytes = mq_receive(server_qid, (char *) &msg, MAX_MSG_LEN, &priority);
    if(received_bytes == -1) {
        perror("Error while receiving message");
    }
    printf("%d messages: %s %s\n",msg.pid,COMMANDS_STR[msg.type],msg.value);
    switch(msg.type) {
        case LOGIN:
            login_handler(msg);
            break;
        case LOGOUT:
            logout_handler(msg);
            break;
        case ECHO:
            echo_handler(msg);
            break;
        case UPPER:
            upper_handler(msg);
            break;
        case TIME:
            time_handler(msg);
            break;
        case TERMINATE:
            terminate = 1;
            break;
        default:
            printf("%d unknown command\n",msg.pid);
    }
}

void sigint_handler(int signum) {
    exit(EXIT_SUCCESS);
}

// every (I hope) possible error handled inside functions create_queue and receive_message
int main() {
    client_unique_id = 0;
    atexit(exit_handler);

    signal(SIGINT,sigint_handler);

    puts("Launching server...");
    server_qid = create_queue();
    puts("Server launched!");

    while(!terminate) {
        receive_message();
    }

    return 0;
}