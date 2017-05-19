#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdbool.h>

int main(int argc, char **argv){
    if(argc != 4){
        printf("Bad number of arguments");
        return -1;
    }
    int threadsNum = atoi(argv[1]);
    int myFile = creat(argv[2], 0666);
    int records = atoi(argv[3]);
    bool written = false;

    char buffer[1024];
    char buffer2[1024];
    for(int i = 0; i < 1024; i++){
        buffer[i] = '-';
        buffer2[i] = '-';
        if(!written && i <= 1020 && i*112321%571 == 474){
            written = true;
            buffer[i++] = 'k';
            buffer[i++] = 'o';
            buffer[i++] = 't';
        }
    }
    for(int j = 0; j < threadsNum; j++){
        if(j%4 == 0)    write(myFile, buffer, 1024);
        else    write(myFile, buffer2, 1024);
    }
    close(myFile);
    return 0;
}