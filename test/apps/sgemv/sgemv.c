#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <math.h>

#include <ppu_intrinsics.h>

#include "tpc_common.h"
#include "tpc_ppe.h"
#include "sgemv.h"

 

# define CMP_WITH_REF

int M = 32768;
int N = 256;
int BLOCK_M = 64;
int SPES = 6;



void sgemv_ref(float* A, float* x, float* y, float alpha, float beta, unsigned int m, unsigned int n)
{
    unsigned int i,j;
    float tmp;
    for (i=0; i<m; i++) {
        tmp = 0.0f;
        for (j=0; j<n; j++) {
            tmp += *(A + (i*N + j)) * x[j];
        }
        y[i] = alpha*tmp + beta*y[i];
    }
}




int main(int argc, char * argv[]) {

    unsigned int i, j;
    unsigned long long end, start;
    float tpc_usec;
    int num_errors = 0;

    if(argc != 5) {
      fprintf(stderr, "Wrong synatx\n");
      fprintf(stderr, "%s <M> <N> <BLOCK> <SPES>\n", argv[0]);
      exit(1);
    }

    M = atoi(argv[1]);
    N = atoi(argv[2]);
    BLOCK_M = atoi(argv[3]);
    SPES = atoi(argv[4]);

    M=M*1024;
    //N=N*1024;
    BLOCK_M = BLOCK_M;

    if(M<=0 || N<=0 || BLOCK_M<=0 || SPES<=0) {
      fprintf(stderr, "Wrong arguments\n");
      exit(1);
    }

    tpc_init(SPES);


    float *alpha = tpc_malloc(128);
    float *A_init = (float*)tpc_malloc( sizeof(float) * N);
#ifdef CMP_WITH_REF
    float *A_ref = (float*)tpc_malloc( sizeof(float) * M*N);
    float *y_ret = (float*)tpc_malloc( sizeof(float) * M);
#endif
    float *x_ref = (float*)tpc_malloc( sizeof(float) * N);
    //float *y_ref = (float*)tpc_malloc( sizeof(float) * M);

    alpha[0] = 2.0;
    alpha[1] = (float)BLOCK_M;
    alpha[2] = (float)N;

    printf("Initialize arrays\n");
    printf("Array size: %d X %d\n", M, N);
    printf("Initialize A\n");
    for ( i=0; i < M; ++i )
    {
        for ( j=0; j < N; ++j )
        {
            A_init[j] = (float)((i*N+j)%17);
        }
        
#ifdef CMP_WITH_REF
        memcpy(&((float (*)[N])(A_ref))[i][0], A_init, N*sizeof(float));
#endif
    }


    // Warm up TPC runtime
    /*for(i=0; i<6; ++i) {
      tpc_call(0, 4,
	  &y_ret[0], BLOCK_M*sizeof(float), TPC_INOUT_ARG,
	  &A_ref[0], BLOCK_M*N*sizeof(float), TPC_IN_ARG,
	  x_ref, N*sizeof(float), TPC_IN_ARG,
	  alpha, 128, TPC_IN_ARG
      );
    }
    tpc_wait_all();
    tpc_reset_stats();*/

    
    printf("Initialize x\n");
    for (i=0; i<N; i++){
        x_ref[i] = (float)(i % 19);
    }
    
    printf("Initialize y\n");
    for (i=0; i<M; i++){
        //y_ref[i] = (float)(i % 13);
        y_ret[i] = (float)(i % 13);
    }
    


    printf("\n\nRunning TPC implementation:\n");

    start = __mftb();
    for(j=0; j<ITERS; ++j) {
      for(i=0; i<M; i+=BLOCK_M) {
	tpc_call(0, 4,
	    &y_ret[i], BLOCK_M*sizeof(float), TPC_INOUT_ARG,
	    &A_ref[i*N], BLOCK_M*N*sizeof(float), TPC_IN_ARG,
	    x_ref, N*sizeof(float), TPC_IN_ARG,
	    alpha, 128, TPC_IN_ARG
	);
      }
      tpc_wait_all();
    }
    end = __mftb();

    tpc_usec = ((end-start)/79.8)/(float)ITERS;

    float numGOps = (2.0 * M * N)/1000000000.0;
    float xferGbytes = (sizeof(float) * (M*N + (3.0*N)))/1000000000.0;
    
    printf("TPC total sgemv running time: %.0f usec -- %.4f GFLOPS  -- %.4f GB/sec\n",
           tpc_usec, numGOps/(tpc_usec/1000000.0),
            xferGbytes/(tpc_usec/1000000.0) );
    
#ifdef CMP_WITH_REF
/*    printf("Running reference implementation:\n"); 
    for(i=0; i<ITERS; i++){
        sgemv_ref(A_ref, x_ref, y_ref, alpha[0], 1.0, M, N);
    }
    
    // results verification
    num_errors = 0;
    for (i=0; i<M; i++) { 
        if ( fabs(y_ret[i]-y_ref[i]) > 1.0e-4 ) {                 
            if ( num_errors < 50 )
            {
                printf("element %d: got %.2f expected %.2f\n", i, 
                       y_ret[i], y_ref[i]); 
            }
            num_errors++;  
        } 
    }*/
#endif
    
    
#ifdef CMP_WITH_REF
    //printf("%d errors detected.\n", num_errors);
#endif
    
    

#ifdef CMP_WITH_REF
    free(A_ref);
#endif
    free(A_init);
    free(x_ref);
    //free(y_ref);

    tpc_print_stats(stdout);
    tpc_shutdown();
    return num_errors;
}


