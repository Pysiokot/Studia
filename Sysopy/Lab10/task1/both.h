#include <stdio.h>
#include <stdlib.h>

#include <netdb.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <sys/types.h>

#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>

enum calcType {MUL, DIV, ADD , SUB};

struct serverMsg{
    enum calcType ct;
    double arg1;
    double arg2;
    int ID;
};

struct clientMsg{
    int logout;
    double answer;
    int ID;
    char name[16];
};

struct client{
    char name[16];
    int clientFD;
    int occupied;
};
