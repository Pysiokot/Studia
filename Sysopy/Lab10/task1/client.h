#include <signal.h>
char* name;
char* connectType;
char* address;
int socketFD;
int port;

void closeUnix();
void closeInet();
void signalHandler(int signo);
void unixConnect();
void inetConnect();
void calculate(struct serverMsg msg);
void reciever();