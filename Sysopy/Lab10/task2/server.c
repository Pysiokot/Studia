#define MAX_CLIENTS 20

#include "both.h"
#include "server.h"

int main(int argc, char **argv) {
    if (argc != 3) {
        printf("Bad number of arguments\n");
        exit(EXIT_FAILURE);
    }

    int portNum = atoi(argv[1]);
    path = argv[2];
    unlink(path);
    srand(time(NULL));


    clients = calloc(MAX_CLIENTS, sizeof(struct client));
    initSockets(path, portNum);

    pthread_mutex_init(&mutex, NULL);

    pthread_t reciever;
    pthread_t pinger;
    pthread_create(&reciever, NULL, (void *(*)(void *)) &parseInput, NULL);
    pthread_create(&pinger, NULL, (void *(*)(void *)) &pingJob, NULL);


    atexit(&clean);
    mainJob();

    pthread_join(reciever, NULL);
    pthread_join(pinger, NULL);
}

int getClient() {
    return rand() % clientsNum;
}

int getName(char *name) {
    for (int i = 0; i < MAX_CLIENTS; ++i) {
        if (strcmp(clients[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

void initSockets(char *path, int portno) {

    socketUnixFD = socket(AF_UNIX, SOCK_DGRAM, 0);
    if (socketUnixFD < 0) {
        perror("ERROR opening socket");
        exit(EXIT_FAILURE);
    }

    struct sockaddr_un unixAddress;
    unixAddress.sun_family = AF_UNIX;
    strcpy(unixAddress.sun_path, path);


    if (bind(socketUnixFD, (struct sockaddr *) &unixAddress, sizeof(struct sockaddr)) < 0) {
        perror("error binding unix");
        exit(EXIT_FAILURE);
    }


    socketInetFD = socket(AF_INET, SOCK_DGRAM, 0);

    if (socketInetFD < 0) {
        perror("ERROR opening socket");
        exit(1);
    }


    struct sockaddr_in inetAddress;
    inetAddress.sin_family = AF_INET;
    inetAddress.sin_addr.s_addr = INADDR_ANY;
    inetAddress.sin_port = htons(portno);


    if (bind(socketInetFD, (struct sockaddr *) &inetAddress, sizeof(inetAddress)) < 0) {
        perror("error binding inet");
        exit(EXIT_FAILURE);
    }

}

void checkSocket(int socketFD) {
    struct sockaddr addr;
    struct clientMsg msg;
    socklen_t s;
    if (recvfrom(socketFD, (char *) &msg, sizeof(msg), 0, &addr, &s) < 0) {
        perror("error receiving name \n");
        exit(EXIT_FAILURE);
    }

    if (checkAddress(addr)) {
        checkAnswer(msg);
        clients[getName(msg.name)].occupied--;
    } else {
        acceptClient(msg, socketFD, addr, s);
    }
}

void acceptClient(struct clientMsg msg, int socketFD, struct sockaddr addr, socklen_t s) {
    struct client myClient;
    myClient.clientFD = socketFD;
    myClient.clientAddress = addr;
    myClient.socklen = s;
    myClient.occupied = 0;
    clients[clientsNum] = myClient;


    if (getName(msg.name) == -1) {
        strcpy(clients[clientsNum].name, msg.name);
        clientsNum++;
        printf("client %s connected\n", clients[clientsNum-1].name);
    } else {
        struct serverMsg myMsg;
        myMsg.ID = -1;
        if (sendto(socketFD, (char *) &myMsg, sizeof(myMsg), 0, &myClient.clientAddress, myClient.socklen) < 0) {
            perror("Error sending serverMsg\n");
            exit(EXIT_FAILURE);
        }
    }
}


void mainJob() {
    FDs[0].fd = socketUnixFD;
    FDs[0].events = POLLIN;


    FDs[1].fd = socketInetFD;
    FDs[1].events = POLLIN;

    while (1) {
        pthread_mutex_lock(&mutex);
        int ret = poll(FDs, (nfds_t) (2), 1000);
        if (ret == -1) {
            perror("polling error");
            exit(EXIT_FAILURE);
        } else if (ret != 0) {
            if (FDs[0].revents & POLLIN) {
                FDs[0].revents = 0;
                checkSocket(FDs[0].fd);

            }
            if (FDs[1].revents & POLLIN) {
                FDs[1].revents = 0;
                checkSocket(FDs[1].fd);
            }
        }
        pthread_mutex_unlock(&mutex);
        usleep(1000);
    }
}

void pingJob() {
    while (1) {
        pthread_mutex_lock(&mutex);

        struct serverMsg ping;
        ping.ID = -2;

        for (int i = 0; i < clientsNum; ++i) {
            if (clients[i].occupied == 0) {
                if (sendto(clients[i].clientFD, (char *) &ping, sizeof(ping), 0, &clients[i].clientAddress,
                           clients[i].socklen) < 0) {
                    logout(clients[i].name);
                    break;
                }
                usleep(5000);
                struct clientMsg resp;
                if (recv(clients[i].clientFD, (char *) &resp, sizeof(resp), MSG_DONTWAIT) <= 0) {
                    logout(clients[i].name);
                }
            }
        }
        pthread_mutex_unlock(&mutex);
        sleep(1);
    }
}

void parseInput() {
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
            if (clientsNum > 0) {
                int i = getClient();
                struct client myClient = clients[i];
                if (sendto(myClient.clientFD, (char *) &calc, sizeof(calc), 0, &myClient.clientAddress, myClient.socklen) <
                    0) {
                    perror("error sending serverMsg to client");
                    exit(EXIT_FAILURE);
                }
                clients[i].occupied++;
            } else {
                printf("No clients \n");
            }
            pthread_mutex_unlock(&mutex);
        }
    }
}

void checkAnswer(struct clientMsg msg) {
    if (msg.logout) {
        logout(msg.name);
    } else {
        printf("The answear to calculation with ID: %i is %lf calculation was done by: %s \n", msg.ID, msg.answer,
               msg.name);
    }
}

int checkAddress(struct sockaddr sockaddr) {
    for (int i = 0; i < clientsNum; ++i) {
        if (strcmp(clients[i].clientAddress.sa_data, sockaddr.sa_data) == 0) {
            return 1;
        }
    }
    return 0;
}

void logout(char *name) {
    int index = getName(name);
    if (index == -1)
        return;

    clients[index] = clients[clientsNum - 1];
    FDs[index] = FDs[clientsNum - 1];

    clientsNum--;
    printf("client %s disconnected\n", name);
}

void clean() {
    pthread_mutex_destroy(&mutex);
    shutdown(socketUnixFD, SHUT_RDWR);
    close(socketUnixFD);
    unlink(path);

    shutdown(socketInetFD, SHUT_RDWR);
    close(socketInetFD);
}