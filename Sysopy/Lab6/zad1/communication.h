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

#define MAX_CLIENTS 4
#define MAX_MSG_LEN 64

typedef enum m_type {
    LOGOUT,ECHO,UPPER,TIME,TERMINATE, LOGIN, ACCEPT, DECLINE, NOC // from 0 to 5;
} m_type;
static const enum m_type COMMANDS_ENUM[] = {LOGOUT,ECHO,UPPER,TIME,TERMINATE,LOGIN};
static const char *COMMANDS_STR[] = {"LOGOUT","ECHO", "UPPER", "TIME", "TERMINATE", "LOGIN"};

typedef struct message {
    enum m_type type;
    pid_t pid;
    char value[MAX_MSG_LEN];
} message;

static const char* SERVER_PATH = "HOME";  // the key is generated through the $HOME env variable
static const int SERVER_ID = 'p';   // and this id
static const size_t MAX_MSG_SIZE = sizeof(struct message) - sizeof(long);


#endif //CW06_COMMUNICATION_H