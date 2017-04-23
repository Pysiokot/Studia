#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
int size;

void makePlot(int **T);
void scale(double *real, double *imaginary);

int main(int argc, char **argv) {
    if(argc!=3) {
        printf("Not enought or too much args\n");
        return -1;
    }
    char *stream = argv[1];
    size = atoi(argv[2]);
    mkfifo(stream,0666);
    int descriptor = open(stream, O_RDONLY);

    size_t line_size = 51;
    char buffer[51];

    int **T = malloc(sizeof(int*)*size);
    int iter=0;
    while(iter < size) {
        T[iter++] = calloc((size_t)size,sizeof(int));
    }
    double real, imaginary;
    int iter2;
    while(read(descriptor,buffer,line_size) > 0) {
        sscanf(buffer,"%lf %lf %d",&real,&imaginary,&iter2);
        scale(&real, &imaginary);
        T[(int)real][(int)imaginary] = iter2;
    }
    close(descriptor);
    unlink(stream);
    makePlot(T);
    
    return 0;
}

void scale(double *real, double *imaginary) {
    *real = size * fabs(-2 - *real) / 3.0;
    *imaginary = size * fabs(1 - *imaginary) / 2.0;
}

void makePlot(int **T) {
    FILE* file = fopen("data","w");
    for(int i = 0; i < size; ++i) {
        for(int j = 0; j < size; ++j) {
            fprintf(file,"%d %d %d\n",i,j,T[i][j]);
        }
    }
    fclose(file);

    FILE* graph = popen("gnuplot","w");
    fprintf(graph,"set view map\n");
    fprintf(graph, "set xrange [0:%d]\n", size-1);
    fprintf(graph, "set yrange [0:%d]\n", size-1);
    fprintf(graph, "plot 'data' with image\n");

    fflush(graph);
    getc(stdin);
    pclose(graph);
}