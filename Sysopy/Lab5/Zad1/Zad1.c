#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>
#define MAXCOMM 50
#define MAXARGS 10

void handleCommands(char *line, size_t size, ssize_t read);
void executeCommand(char *line);
void executeCommandWithArguments(char *command, int *descriptor, int from, int to);

int main(int argc, char **argv){ 
    char *line;
    size_t size = 0;
    ssize_t read = 0;
    char *result;
    printf("Enter commands.\n");
    read = getline(&line, &size, stdin);
    handleCommands(line, size, read);
    return 0;
}

void handleCommands(char *line, size_t size, ssize_t read){
    if(read != -1){
        executeCommand(line);
        printf("\n");
    }
    else{
        printf("No commands, exiting\n");
        if(line != NULL)    free(line);
        exit(0);
    }
}

void executeCommand(char *line){
    int iterator = 1;
    char **inputLine = malloc(MAXCOMM*sizeof(char*));
    char *command = strtok(line,"|");
    char *nextCommand = strtok(NULL,"|");
    inputLine[0] = command;
    while (iterator < MAXCOMM && nextCommand!=NULL){
        inputLine[iterator++] = nextCommand;
        nextCommand = strtok(NULL,"|");
    }
    int descriptor[2];
    int read = STDIN_FILENO;
    iterator = 0;
    while (iterator < MAXCOMM && inputLine[iterator] != NULL){
        pipe(descriptor);
        if(iterator + 1 == MAXCOMM || inputLine[iterator+1] == NULL)
            executeCommandWithArguments(inputLine[iterator++], descriptor, read, -1);
        else
            executeCommandWithArguments(inputLine[iterator++], descriptor, read, 1);
        read = descriptor[0];
        close(descriptor[1]);
    }
    close(descriptor[0]);
    free(inputLine);
}

void executeCommandWithArguments(char *line, int *descriptor, int from, int to){
    int i = 1;
    char **args = malloc(MAXARGS*sizeof(char*));
    char *command = strtok(line," \n\t\r");
    char *nextCommand = strtok(NULL," \n\t\r");
    args[0] = command;
    while (i < MAXARGS && nextCommand != NULL){
        args[i++] = nextCommand;
        nextCommand = strtok(NULL," \n\t\r");
    }
    int pid = fork();
    if(pid > 0){
        close(descriptor[1]);
        dup2(descriptor[0],STDIN_FILENO);
        wait(NULL);
        if(from != -1) close(from);
    }
    else if (pid == 0){
        if(from != -1) dup2(from,STDIN_FILENO);
        if(to != -1) dup2(descriptor[1],STDOUT_FILENO);
        if(execvp(args[0], args) == -1){
            printf("Exec error\n");
            exit(-1);
        }
    }
    else {
        printf("Error\n");
        exit(-1);
    }
    free(args);
}