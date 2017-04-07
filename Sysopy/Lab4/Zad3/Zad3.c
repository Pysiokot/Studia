    #include <stdlib.h>
#include <wait.h>
#include <values.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <sys/resource.h>


pid_t cat;
pid_t kitty;
int sigAmount, redLaser, greenLaser;
int type;
const union sigval value;
volatile int flag = 0;
volatile int signalsSentToKitty;
volatile int signalsRecievedByKitty;
volatile int signalsRecievedFromKitty;

void doWork();
int sendSig1(int signals);
void sendSig2();
void redLaserHandler(int sigID);
void greenLaserHandler(int sigID);

int main(int argc, char **argv){
    if(argc != 3){
        printf("Wrong number of parameters");
        return 1;
    }
    sigAmount = atoi(argv[1]);
    type = atoi(argv[2]);
    redLaser = SIGUSR1;
    greenLaser = SIGUSR2;

    if(type == 3) {
        redLaser = SIGRTMIN + 10;
        greenLaser = redLaser + 5;
    }

    if(sigAmount < 0)
        return 1;

    signal(redLaser, redLaserHandler);
    signal(greenLaser, greenLaserHandler);
    doWork();

    return 0;
}

void doWork(){
    cat = getpid();
    kitty = fork();
    if(kitty == 0) {
        while (flag == 0){
            pause();
        }
        printf("\nKitty recieved: %d\n", signalsRecievedByKitty);
        exit(0);
    } 
    else {
        int signals = 0;
        while(signals < sigAmount) {
            signals = sendSig1(signals);
            signalsSentToKitty++;
        }
        sendSig2();
        signalsSentToKitty++;
    }

    int status;
    wait(&status);

    printf("\nAll sent gifts: %d\n",signalsSentToKitty);
    printf("\nAll recieved gifts from kitty: %d\n", signalsRecievedFromKitty);
}

void redLaserHandler(int sigID) {
    if(kitty != 0) {
        signalsRecievedFromKitty++;
    } 
    else {
        signalsRecievedByKitty++;
        kill(cat, sigID);
    }
}

void greenLaserHandler(int sigID) {
    if(kitty == 0) {
        signalsRecievedByKitty++;
        flag = 1;
    }
}

int sendSig1(int signals){
            if (type == 2) {
                sigqueue(kitty, redLaser, value);
            } 
            else {
                kill(kitty, redLaser);
            }
            return ++signals;
}

void sendSig2(){
        if(type == 2) {
            sigqueue(kitty, greenLaser, value);
        } 
        else {
            kill(kitty, greenLaser);
        }
}