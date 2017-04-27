//
// Created by Mrz355 on 19.04.17.
//

#ifndef CW06_COMMUNICATION_H
#define CW06_COMMUNICATION_H


#include <sys/ipc.h>
#include <sys/msg.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>           /* For O_* constants */
#include <sys/stat.h>        /* For mode constants */

#define MAX_CLIENTS 4
#define MAX_MSG_LEN 64
#define SERVER_QUEUE_PERM 0600
#define MAX_QUEUE_SIZE 4

typedef enum m_type {
    LOGOUT,ECHO,UPPER,TIME,TERMINATE, LOGIN, ACCEPT, DECLINE, NOC // from 0 to 5;
} m_type;
static const enum m_type COMMANDS_ENUM[] = {LOGOUT,ECHO,UPPER,TIME,TERMINATE,LOGIN};
static const char *COMMANDS_STR[] = {"LOGOUT","ECHO", "UPPER", "TIME", "TERMINATE", "LOGIN"};


static const char* SERVER_NAME = "/server355";
static const size_t MAX_MSG_SIZE = sizeof(char) * MAX_MSG_LEN ;
typedef struct message {
    enum m_type type;
    pid_t pid;
    char value[MAX_MSG_LEN];
} message;

#endif //CW06_COMMUNICATION_H