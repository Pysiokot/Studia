#include "both.h"
#include "client.h"

int main(int argc, char **argv){
    if(argc != 4 && argc !=5){
        printf("Bad number of arguments\n");
        exit(EXIT_FAILURE);
    }

    name = argv[1];
    connectType = argv[2];
    address = argv[3];
    if(argc == 5)
        port=atoi(argv[4]);

    if(strcmp(connectType, "unix") == 0) {
        unixConnect();
    }
    else if (strcmp(connectType, "inet") == 0) {
        inetConnect();
    }
    else {
        printf("Bad 2nd argument. Choose unix or inet connection\n");
        exit(EXIT_FAILURE);
    }

    char buf[100];
    sprintf(buf, "%s\n", name);

    if(send(socketFD, buf, 100, 0) <0 ){
        perror("Error writing to socket\n");
        exit(EXIT_FAILURE);
    }

    signal(SIGINT, signalHandler);
    reciever();

    printf("Data was sent \n");
    return 0;
}

void unixConnect(){
    socketFD = socket(AF_UNIX, SOCK_STREAM, 0);
    if (socketFD < 0) {
        perror("Error opening socket\n");
        exit(1);
    }

    struct sockaddr_un addr;
    addr.sun_family = AF_UNIX;
    strcpy(addr.sun_path, address);

    if (connect(socketFD, (struct sockaddr *) &addr, sizeof(struct sockaddr)) == -1) {
        perror("Error connecting to server\n");
        exit(EXIT_FAILURE);
    }
    atexit(&closeUnix);
}

void inetConnect(){
    socketFD = socket(AF_INET, SOCK_STREAM, 0);

    if (socketFD < 0) {
        perror("Error opening socket\n");
        exit(1);
    }
    struct in_addr inaddr;
    if(inet_aton(address, &inaddr) ==0){
        perror("Address doesn't exist\n");
        exit(EXIT_FAILURE);
    }
    struct sockaddr_in addr;
    addr.sin_family=AF_INET;
    addr.sin_addr=inaddr;
    addr.sin_port=htons(port);

    if(connect(socketFD, (struct sockaddr*) &addr, sizeof(addr))==-1){
        perror("Error logging to server\n");
        exit(EXIT_FAILURE);
    }
    atexit(&closeInet);
}

void reciever() {
    ssize_t buffSize;
    struct serverMsg msg;
    while((buffSize = recv(socketFD , (char *) &msg , sizeof(struct serverMsg) , 0)) > 0 ){
        if(msg.ID == -1){
            printf("Name already taken\n");
            exit(EXIT_FAILURE);
        } else if (msg.ID == -2){
            struct clientMsg resp;
            if(send(socketFD, (char*) &resp, sizeof(resp), 0) <0 ){
                perror("Error sending pong\n");
                exit(EXIT_FAILURE);
            }
        } else {
            calculate(msg);
        }
    }

    if(buffSize == 0) {
        printf("Server disconnected\n");
        exit(0);
    }
    else if(buffSize == -1)
    {
        perror("recv failed!!!\n");
        exit(EXIT_FAILURE);
    }
}

void calculate(struct serverMsg msg) {
    double res;
    switch (msg.ct){
        case MUL:
            res = msg.arg1 * msg.arg2;
            break;
        case DIV:
            res = msg.arg1 / msg.arg2;
            break;
        case ADD:
            res = msg.arg1 + msg.arg2;
            break;
        case SUB:
            res = msg.arg1 - msg.arg2;
            break;
        default:
            res = 0;
            break;
    }

    struct clientMsg resp;
    resp.answer = res;
    resp.ID=msg.ID;
    resp.logout=0;
    strcpy(resp.name, name);

    if(send(socketFD, (char*) &resp, sizeof(resp), 0) <0 ){
        perror("Error sending answear\n");
        exit(EXIT_FAILURE);
    }
}

void closeUnix(){
    shutdown(socketFD, SHUT_RDWR);
    close(socketFD);
}

void closeInet(){
    shutdown(socketFD, SHUT_RDWR);
    close(socketFD);
}

void signalHandler(int signo){
    if (signo == SIGINT){
        struct clientMsg msg;
        msg.logout=1;
        strcpy(msg.name,name);

        if(send(socketFD, (char*) &msg, sizeof(msg), 0) <0 ){
            perror("Error sending logout req\n");
            exit(EXIT_FAILURE);
        }
        exit(EXIT_SUCCESS);
    }
}