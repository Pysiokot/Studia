#include <sys/socket.h>
#include <poll.h>
#include <pthread.h>

int ID = 0;
int socketUnixFD;
int socketInetFD;
char *path;
struct client *clients;
struct pollfd FDs[MAX_CLIENTS + 2];
int clientsNum = 0;
pthread_mutex_t mutex;


int getClient();
void clean();
int getName(char *name);
void logout(char *name);
void pingJob();
void parseInput();
void initSockets(char *path, int portno);
void acceptClient(struct clientMsg msg, int socketFD, struct sockaddr addr, socklen_t s);
void checkAnswer(struct clientMsg msg);
int checkAddress(struct sockaddr sockaddr);
void checkSocket(int socketFD);
void mainJob();