#include <sys/socket.h>
#include <poll.h>
#include <pthread.h>

int ID = 0;
int unixSocketFD;
int inetSocketFD;
char *path;
struct client *clients;
struct pollfd FDs[ALLCLIENTS + 2];

int clientsNum = 0;
pthread_mutex_t mutex;

int getClient();
void clean();
int getIndex(char *name);
void disconnectClient(char *name);
void pingTask();
void inputParser();
void initSocket(char *path, int portno);
void acceptClient(struct pollfd *FDs, int sockfd);
void checkAnswer(int fd, char *name);
void serverJob();