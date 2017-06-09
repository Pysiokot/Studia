#define ALLCLIENTS 7
#include "both.h"
#include "server.h"

int main(int argc, char **argv) {
    if (argc != 3) {
        printf("Bad arguments");
        exit(EXIT_FAILURE);
    }

    int portNum = atoi(argv[1]);
    path = argv[2];
    unlink(path);
    srand(time(NULL));


    clients = calloc(ALLCLIENTS, sizeof(struct client));
    initSocket(path, portNum);

    pthread_mutex_init(&mutex, NULL);

    pthread_t reciever;
    pthread_t pinger;
    pthread_create(&reciever, NULL,(void * (*)(void *)) & inputParser, NULL);
    pthread_create(&pinger, NULL,(void * (*)(void *)) & pingTask, NULL);


    atexit(&clean);
    serverJob();

    pthread_join(reciever, NULL);
    pthread_join(pinger, NULL);
}

int getClient() {
    return rand() % clientsNum;
}

int getIndex(char *name) {
    for (int i = 0; i < ALLCLIENTS; ++i) {
        if (strcmp(clients[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

void initSocket(char *path, int portNum) {
    unixSocketFD = socket(AF_UNIX, SOCK_STREAM, 0);
    if (unixSocketFD < 0) {
        perror("ERROR opening socket");
        exit(EXIT_FAILURE);
    }
    struct sockaddr_un unixAddress;
    unixAddress.sun_family = AF_UNIX;
    strcpy(unixAddress.sun_path, path);

    if (bind(unixSocketFD, (struct sockaddr *) &unixAddress, sizeof(struct sockaddr)) < 0) {
        perror("error binding unix");
        exit(EXIT_FAILURE);
    }

    inetSocketFD = socket(AF_INET, SOCK_STREAM, 0);

    if (inetSocketFD < 0) {
        perror("ERROR opening socket");
        exit(1);
    }

    struct sockaddr_in inetAddress;
    inetAddress.sin_family = AF_INET;
    inetAddress.sin_addr.s_addr = INADDR_ANY;
    inetAddress.sin_port = htons(portNum);


    if (bind(inetSocketFD, (struct sockaddr *) &inetAddress, sizeof(inetAddress)) < 0) {
        perror("error binding inet");
        exit(EXIT_FAILURE);
    }

    listen(unixSocketFD, 10);
    listen(inetSocketFD, 10);
}

void acceptClient(struct pollfd *FDs, int socketFD) {
    int clientFD = accept(socketFD, NULL, 0);
    if (clientFD < 0) {
        perror("accept failed");
        exit(EXIT_FAILURE);
    }
    struct client newClient;
    newClient.clientFD = clientFD;
    newClient.occupied=0;
    clients[clientsNum] = newClient;

    char buf[100];
    if (recv(clientFD, buf, 100, 0) < 0) {
        perror("error receiving name \n");
        exit(EXIT_FAILURE);
    }
    char name[100];
    sscanf(buf, "%s\n", name);

    if (getIndex(name) == -1) {
        FDs[2 + clientsNum].fd = clientFD;
        FDs[2 + clientsNum].events = POLLIN;
        strcpy(clients[clientsNum].name, name);
        clientsNum++;
        printf("client %s connected\n", name);
    } else {
        struct serverMsg tmp;
        tmp.ID = -1;
        if (send(newClient.clientFD, (char *) &tmp, sizeof(tmp), 0) < 0) {
            perror("Error sending serverMsg\n");
            exit(EXIT_FAILURE);
        }
        close(clientFD);
    }
}

void serverJob() {
    FDs[0].fd = unixSocketFD;
    FDs[0].events = POLLIN;


    FDs[1].fd = inetSocketFD;
    FDs[1].events = POLLIN;

    while (1) {
        pthread_mutex_lock(&mutex);
        int ret = poll(FDs, (nfds_t) (2 + clientsNum), 1000);
        if (ret == -1) {
            perror("polling error");
            exit(EXIT_FAILURE);
        } else if (ret != 0) {
//            Client waiting on unix sock for accept
            if (FDs[0].revents & POLLIN) {
                FDs[0].revents = 0;
                acceptClient(FDs, FDs[0].fd);
            }
//            Client waiting on inet for accept
            if (FDs[1].revents & POLLIN) {
                FDs[1].revents = 0;
                acceptClient(FDs, FDs[1].fd);
            }

            for (int i = 2; i < clientsNum + 2; ++i) {
                if (FDs[i].revents & POLLIN) {
                    FDs[i].revents = 0;
                    checkAnswer(FDs[i].fd, clients[i - 2].name);
                    clients[i - 2].occupied--;
                }
            }
        }
        pthread_mutex_unlock(&mutex);
        usleep(1000);
    }
}

void pingTask() {
    while (1) {
        pthread_mutex_lock(&mutex);

        struct serverMsg ping;
        ping.ID = -2;

        for (int i = 0; i < clientsNum; ++i) {
            if (!clients[i].occupied) {
                if (send(clients[i].clientFD, (char *) &ping, sizeof(ping), 0) < 0) {
                    perror("Error sending serverMsg to client");
                    exit(EXIT_FAILURE);
                }

                struct clientMsg resp;
                if (recv(clients[i].clientFD, (char *) &resp, sizeof(resp), 0) <= 0) {
                    disconnectClient(clients[i].name);
                }
            }
        }

        pthread_mutex_unlock(&mutex);
        sleep(1);
    }
}

void inputParser() {
    while (1) {
        char *line;
        size_t len;
        getline(&line, &len, stdin);
        if (len != 0) {
            char operation[5];
            double arg1 = 0;
            double arg2 = 0;
            sscanf(line, "%s %lf %lf\n", operation, &arg1, &arg2);
            struct serverMsg calc;
            calc.arg1 = arg1;
            calc.arg2 = arg2;
            calc.ID = ID++;

            if (strcmp(operation, "ADD") == 0) {
                calc.ct = ADD;
            } else if (strcmp(operation, "SUB") == 0) {
                calc.ct = SUB;
            } else if (strcmp(operation, "MUL") == 0) {
                calc.ct = MUL;
            } else if (strcmp(operation, "DIV") == 0) {
                calc.ct = DIV;
            }
            pthread_mutex_lock(&mutex);
            if(clientsNum>0) {
                int i = getClient();
                struct client newClient = clients[i];
                if (send(newClient.clientFD, (char *) &calc, sizeof(calc), 0) < 0) {
                    perror("error sending serverMsg to client");
                    exit(EXIT_FAILURE);
                }
                clients[i].occupied++;
            } else {
                printf("No clients connected\n");
            }
            pthread_mutex_unlock(&mutex);

        }
    }
}

void checkAnswer(int fd, char *name) {
    struct clientMsg a1;
    ssize_t read = recv(fd, (char *) &a1, sizeof(struct clientMsg), 0);
    if (read < 0) {
        perror("error receiving clientMsg \n");
        exit(EXIT_FAILURE);
    } else if (a1.logout || read == 0) {
        disconnectClient(name);
    } else {
        printf("The anwsear to calculation with ID: %i is %lf calculation was done by: %s \n", a1.ID, a1.answer,
               a1.name);
    }
}

void disconnectClient(char *name) {
    int index = getIndex(name);
    if (index == -1)
        return;

    clients[index] = clients[clientsNum - 1];
    FDs[index] = FDs[clientsNum - 1];

    clientsNum--;
    printf("Client %s disconnected\n", name);
}

void clean() {
    pthread_mutex_destroy(&mutex);
    shutdown(unixSocketFD, SHUT_RDWR);
    close(unixSocketFD);
    unlink(path);
    shutdown(inetSocketFD, SHUT_RDWR);
    close(inetSocketFD);
}