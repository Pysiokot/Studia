#include <stdio.h>
#include <complex.h>
#include <time.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
double real();
double imaginary();

int main(int argc, char ** argv) {
    if(argc != 4) {
        printf("Wrong number of args\n");
        return -1;
    }
    srand((unsigned int) (time(NULL)));
    char *stream = argv[1];
    int N = atoi(argv[2]);
    int K = atoi(argv[3]);
    int descriptor = open(stream, S_IFIFO | O_WRONLY);
    int iterator = 0;
    double complex c;
    double complex z0;
    char line[51];

    for(int i = 0; i < N; i++){
        c = real() + imaginary()*I;
        z0 = 0;
        iterator=0;
        while(cabs(z0) <= 2 && iterator < K){
            z0 = z0*z0 + c;
            iterator++;
        }
        sprintf(line,"%lf %lf %d", creal(c), cimag(c), iterator);
        write(descriptor,line,51);
    }
    close(descriptor);
    return 0;
}

double real() {
    return -2*(double)rand()/(double)RAND_MAX + (double)rand()/(double)RAND_MAX;
}

double imaginary() {
    return -(double)rand()/(double)RAND_MAX + (double)rand()/(double)RAND_MAX;
}