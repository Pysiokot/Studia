#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/resource.h>
#include <time.h>
#include <stdbool.h>

clock_t startTime;
clock_t endTime;
int numOfProcessess;
int limitOfRequests;
int greenLaser;
int redLaser;
int reqCount = 0;
pid_t *kittyPID;
pid_t catPID;
void clearTable();
void putAllCatsToSleep(int sigNummer);
void printTime();
void sigRT(int sigNummer, siginfo_t *val);
void redLaserHandler(int sigNummer, siginfo_t *val);
void greenLaserHandler(int sigNummer);
void napTimeAfterCatchingRedLaser(int sigNummer, siginfo_t *val);

int main(int argc, char **argv){
    if(argc > 3 || argc < 3){
        printf("Wrong number of parameters");
        return 0;
    }
    numOfProcessess = atoi(argv[1]);
    limitOfRequests = atoi(argv[2]);
    if(limitOfRequests > numOfProcessess || limitOfRequests < 0 || numOfProcessess < 0){
        printf("ERR");
        return 1;
    }
	redLaser = SIGUSR1;
	greenLaser = SIGUSR2;

    signal(SIGINT, putAllCatsToSleep);
	catPID = getpid();
	kittyPID = malloc(sizeof(pid_t)*numOfProcessess);
	clearTable();

	
	struct sigaction kitty;
	kitty.sa_flags = SA_SIGINFO;
	kitty.sa_sigaction = sigRT;
	sigemptyset(&kitty.sa_mask);

	struct sigaction cat;
	cat.sa_flags = SA_SIGINFO;
	cat.sa_sigaction = redLaserHandler;
	sigemptyset(&cat.sa_mask);
	struct tms now;

	//define cat signals
	int kitku = SIGRTMIN;
	while(kitku <= SIGRTMAX){
		if(sigaction(kitku, &kitty, NULL) == -1)
			printf("\nError while sending signal to %d\n", kitku);
        kitku++;
    }
	if(sigaction(redLaser, &cat, NULL) == -1)
        perror("\nCat 1 ERRORRRRRRR\n");

	//play game
    pid_t kitki;
	int c = 0;
	while(c++ < numOfProcessess) {
    	kitki = fork();
    	if(kitki < 0) {
       		printf("Error");
        	exit(1);
    	} 
		else if (kitki == 0) {
			if(signal(greenLaser, greenLaserHandler) == SIG_ERR)
				perror("receiving SIGUSR2 error");
			srand(time(NULL) + getpid());
			sleep(rand()%11);
			times(&now);
			startTime = clock();
			kill(catPID, redLaser);
			pause();
        	exit(0); 
    	}
	}
	int tmp;
    bool war = true;
	while(war){
		if (reqCount >= limitOfRequests){
            tmp = reqCount - 1;
			struct sigaction cat;
			cat.sa_flags = SA_SIGINFO;
			cat.sa_sigaction = napTimeAfterCatchingRedLaser;
			sigemptyset(&cat.sa_mask);
			if(sigaction(redLaser, &cat, NULL) == -1)
        		perror("\nMEOWWW SIGUSR1\n");
			war = false;
		}
	}
	while (tmp >= 0)
		kill(kittyPID[tmp--],greenLaser);

    while(true);
    return 0;
}

void printTime(){
	printf("\nTime needed: %ld",(endTime - startTime)/CLOCKS_PER_SEC);
}

void redLaserHandler(int sigNummer, siginfo_t *val){
    kittyPID[reqCount++] = val -> si_pid;
}

void greenLaserHandler(int sigNummer){
	kill(catPID, rand()%(SIGRTMAX-SIGRTMIN)+SIGRTMIN);
	endTime = clock();
	printTime();
	exit(0);
}

void sigRT(int sigNummer, siginfo_t *val){
	printf("\nSIGRT %d\tPID: %d\t EixtCode: %i \n",sigNummer, val -> si_pid, val -> si_status);
}

void napTimeAfterCatchingRedLaser(int sigNummer, siginfo_t *val){
    kittyPID[reqCount] = val -> si_pid;
	kill(kittyPID[reqCount++], greenLaser);
}

void putAllCatsToSleep(int sigNummer){
    int i = 0;
    while( i < numOfProcessess){
        kill(kittyPID[i++], SIGKILL);
    }
    kill(catPID, SIGKILL);
    exit(0);
}

void clearTable(){
	int amount = 0;
	while(amount < numOfProcessess)
		kittyPID[amount++] = 0;
}