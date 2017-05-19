#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <pthread.h>
#include <signal.h>
#include <unistd.h>
#include <stdbool.h>

int taskType;

void maskSignal();
void sigusrHandler(int signo);
void * runThread(void * args);