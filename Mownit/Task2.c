#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <gsl/gsl_blas.h>
#include <sys/resource.h>
#include <time.h>

struct timeval tv1, tv2;

void start_timer();
void do_for_vectors(FILE *fp);
void do_for_matrixes(FILE *fp);
void set_vector(gsl_vector *v1, int length);
void set_vectors(gsl_vector *v1, gsl_vector *v2, int length);
void set_matrix(gsl_matrix *M, int n);
void print_vector_results(gsl_vector *v1, gsl_vector *v2, int len, FILE *f);
void print_matrix_results(gsl_matrix *M, gsl_vector *v, FILE *f, int len);
void stop_and_print_timer(FILE *file);


int main(void){
    FILE * ffp = fopen("RexultVec.csv", "w+");
    FILE * sfp = fopen("ResultMat.csv", "w+");

    srand(time(NULL));

    do_for_vectors(ffp);
    do_for_matrixes(sfp);

    fclose(ffp);
    fclose(sfp);
    return 0;
}

void start_timer(){
    gettimeofday(&tv1, NULL);
}

void stop_and_print_timer(FILE *file){
    gettimeofday(&tv2, NULL);
    fprintf(file, "%f", (double)(tv2.tv_usec - tv1.tv_usec) / 1000000 +
        (double) (tv2.tv_sec - tv1.tv_sec));
}

void set_vector(gsl_vector *v1, int length){
    for(int i = 0; i < length; i++)
    {
      gsl_vector_set(v1, i, (int) rand() * i );
    }
}

void set_matrix(gsl_matrix *M, int n){
    for(int i =0; i<n; i++)
    {
        for(int j=0; j<n; j++)
            gsl_matrix_set(M, i, j, (int) i*j * rand());
    }
}

void set_two_vectors(gsl_vector *v1, gsl_vector *v2, int length){
    for(int i = 0; i<length; i++)
    {
        gsl_vector_set(v1, i, (int) (rand() * i) );
        gsl_vector_set(v2, i, (int) (rand() * 2*i%7) );       
    }
}

void print_vector_results(gsl_vector *v1, gsl_vector *v2, int len, FILE *f){
    double dummyResult;
    for(int i=0; i<10; i++) {
        start_timer();
        gsl_blas_ddot(v1, v2, &dummyResult);
        fprintf(f,"%d\t", len);
        stop_and_print_timer(f);
        fprintf(f,"\n");
    }
}

void print_matrix_results(gsl_matrix *M, gsl_vector *v, FILE *f, int len){
    gsl_vector *res =gsl_vector_calloc(len);    
    for(int i=0; i<10; i++)
    {     
        start_timer();    
        gsl_blas_dgemv(CblasNoTrans, 1.0,M, v, 0.0, res );
        fprintf(f,"%d\t", len);
        stop_and_print_timer(f);
        fprintf(f,"\n");
    } 
}

void do_for_vectors(FILE *fp){
    gsl_vector *v0 =gsl_vector_alloc(10000);
    gsl_vector *v1 =gsl_vector_alloc(10000);
    gsl_vector *v2 =gsl_vector_alloc(20000);
    gsl_vector *v3 =gsl_vector_alloc(20000);
    gsl_vector *v4 =gsl_vector_alloc(40000);
    gsl_vector *v5 =gsl_vector_alloc(40000);
    gsl_vector *v6 =gsl_vector_alloc(80000);
    gsl_vector *v7 =gsl_vector_alloc(80000);
    
    set_two_vectors(v0, v1, 10000);
    set_two_vectors(v2, v3, 20000);
    set_two_vectors(v4, v5, 40000);
    set_two_vectors(v6, v7, 80000);

    fprintf(fp,"Wielkosc,Czas\n");

    print_vector_results(v0, v1, 10000, fp);
    print_vector_results(v2, v3, 20000, fp);
    print_vector_results(v4, v5, 40000, fp);
    print_vector_results(v6, v7, 80000, fp);    
  
    gsl_vector_free(v0);
    gsl_vector_free(v1);
    gsl_vector_free(v2);
    gsl_vector_free(v3);
    gsl_vector_free(v4);
    gsl_vector_free(v5);
    gsl_vector_free(v6);
    gsl_vector_free(v7);
}

void do_for_matrixes(FILE *fp){
    gsl_vector *v0 =gsl_vector_alloc(100);
    gsl_vector *v1 =gsl_vector_alloc(200);
    gsl_vector *v2 =gsl_vector_alloc(400);
    gsl_vector *v3 =gsl_vector_alloc(800);


    gsl_matrix *A = gsl_matrix_calloc(100,100);
    gsl_matrix *B = gsl_matrix_calloc(200,200); 
    gsl_matrix *C = gsl_matrix_calloc(400,400); 
    gsl_matrix *D = gsl_matrix_calloc(800,800); 
    
    set_vector(v0, 100);
    set_vector(v1, 200);
    set_vector(v2, 400);
    set_vector(v3, 800);
    
    set_matrix(A, 100);
    set_matrix(B, 200);
    set_matrix(C, 400);
    set_matrix(D, 800);
    
    fprintf(fp,"Wielkosc\tCzas\n");
    
    print_matrix_results(A, v0, fp, 100);
    print_matrix_results(B, v1, fp, 200);
    print_matrix_results(C, v2, fp, 400);
    print_matrix_results(D, v3, fp, 800);
}