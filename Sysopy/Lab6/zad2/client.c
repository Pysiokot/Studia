//
// Created by Mrz355 on 20.04.17.
//

#include <limits.h>
#include <signal.h>
#include <mqueue.h>
#include "communication.h"

mqd_t private_qid;
mqd_t server_qid;
char name[NAME_MAX];
int logout = 0;
struct sigevent sig;

void exit_program(int status, char *exit_message) {
    if(status == EXIT_FAILURE) {
        perror(exit_message);
    } else {
        fprintf(stderr,"%s\n",exit_message);
    }
    exit(status);
}

void exit_handler() {
    mq_close(private_qid);
    mq_unlink(name);
}

mqd_t create_queue() {
    struct mq_attr attr;
    attr.mq_maxmsg = MAX_QUEUE_SIZE;
    attr.mq_msgsize = MAX_MSG_SIZE;

    snprintf(name, NAME_MAX, "/client-%d", getpid()); // write at most NAME_MAX bytes

    mqd_t res = mq_open(name, O_CREAT | O_RDONLY, SERVER_QUEUE_PERM, &attr);
    if(res == -1)
        exit_program(EXIT_FAILURE, "Couldn't create private queue");

    return res;
}

mqd_t get_server_queue() {
    mqd_t res = mq_open(SERVER_NAME, O_WRONLY);
    if(res == -1)
        exit_program(EXIT_SUCCESS,"Couldn't establish connection with server");
    return res;
}

int send_message(message *msg) {
    msg->pid = getpid();

    if(mq_send(server_qid,(char *) msg,MAX_MSG_LEN,1) == -1) {
        perror("Error while sending message to server");
        return -1;
    }
    return 0;
}

long queue_empty() {
    struct mq_attr attr;
    mq_getattr(private_qid,&attr);
    return attr.mq_curmsgs == 0;
}

int receive_message(message *msg) {
    unsigned int priority = 1;
    ssize_t bytes_received = mq_receive(private_qid,(char *) msg,MAX_MSG_LEN,&priority);
    if(bytes_received == -1) {
        perror("Error while receiving message from server");
        return -1;
    }
    printf("Received from server: %s\n",msg->value);
    return 0;
}

int get_id_from_server() {
    message msg;
    msg.type = LOGIN;
    strcpy(msg.value,name);

    send_message(&msg);
    receive_message(&msg);

    if(msg.type == DECLINE) {
        exit_program(EXIT_SUCCESS,"Couldn't establish connection with server");
    }

    return atoi(msg.value);
}

int read_line(message *msg) {
    char line[MAX_MSG_LEN];

    ssize_t bytes_read = read(0,line,MAX_MSG_LEN);
    if(bytes_read == -1) {
        if(errno == EINTR) { // if interrupted by income message - return
            return -1;
        }
        perror("Error while reading line from stdin");
    }
    if(bytes_read < MAX_MSG_LEN)
        line[bytes_read] = '\0'; // preventing from unwanted characters left in line variable after the last read (faster than using callock)

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

void sigrt_handler(int signum) {
    struct message msg;
    do { // at least one message is in queue, this which triggered this function, so for safety - do..while
        if (receive_message(&msg) == -1) {// receive response from server
            perror("Couldn't receive message from server");
        } else if (msg.type == LOGOUT || msg.type == TERMINATE) {
            logout = 1;
        }
    } while (!queue_empty());
    if (mq_notify(private_qid, &sig) == -1) { // prepare for getting responses from server
        exit_program(EXIT_FAILURE, "Error while setting up mq_notify");
    }
}

void sigint_handler(int signum) {
    struct message msg;
    msg.type = LOGOUT;
    msg.value[0] = '\0';
    send_message(&msg);
}

int main() {
    atexit(exit_handler);

    private_qid = create_queue();

    puts("Connecting to server..");
    server_qid = get_server_queue();

    int id_from_server = get_id_from_server();
    puts("Connected!");
    printf("My server-id: %d\n",id_from_server);

    struct sigaction sigact;
    sigemptyset(&(sigact.sa_mask));
    sigact.sa_handler = sigrt_handler;
    if(sigaction(SIGRTMIN,&sigact,NULL) == -1) {
        exit_program(EXIT_FAILURE,"Couldn't set handler for sgrtmin signal");
    }
    struct sigaction sigact2;
    sigemptyset(&(sigact2.sa_mask));
    sigact2.sa_handler = sigint_handler;
    if(sigaction(SIGINT,&sigact2,NULL) == -1) {
        exit_program(EXIT_FAILURE,"Couldn't set handler for sigint signal");
    }

    sig.sigev_notify = SIGEV_SIGNAL;
    sig.sigev_signo = SIGRTMIN;
    if(mq_notify(private_qid,&sig) == -1) { // prepare for getting responses from server
        exit_program(EXIT_FAILURE,"Error while setting up mq_notify");
    }
    struct message msg;
    while(!logout) {
        if(read_line(&msg) == -1) // fulfill the msg struct from stdin
            continue;
        if(send_message(&msg) == -1) // send it to server
            continue;
    }

    return 0;
}
