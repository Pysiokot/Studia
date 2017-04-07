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
volatile sig_atomic_t flag = 0;
volatile sig_atomic_t signals_sent_to_kitty;
volatile sig_atomic_t signals_received_by_kitty;
volatile sig_atomic_t signals_received_from_kitty;

void doWork();
int sendSig1(int signals);
void sendSig2();
void redLaserHandler(int signo);
void greenLaserHandler(int signo);

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
        printf("\nKitty recieved: %d\n", signals_received_by_kitty);
        exit(0);
    } 
    else {
        int signals = 0;
        while(signals < sigAmount) {
            signals = sendSig1(signals);
            signals_sent_to_kitty++;
        }
        sendSig2();
        signals_sent_to_kitty++;
    }

    int status;
    wait(&status);

    printf("\nAll sent gifts: %d\n",signals_sent_to_kitty);
    printf("\nAll recieved gifts from kitty: %d\n", signals_received_from_kitty);
}

void redLaserHandler(int signo) {
    if(kitty != 0) {
        signals_received_from_kitty++;
    } 
    else {
        signals_received_by_kitty++;
        kill(cat, signo);
    }
}

void greenLaserHandler(int signo) {
    if(kitty == 0) {
        signals_received_by_kitty++;
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