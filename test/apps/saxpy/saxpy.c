#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ppu_intrinsics.h>
#include <math.h>

#include "tpc_common.h"
#include "tpc_ppe.h"
#include "saxpy.h"



//#define CMP_WITH_REF


void saxpy_ref(float* x, float* y, float a, unsigned int n)
{
    int i;
    for (i=0; i<n; i++)
        y[i] += a * x[i];    
}



int N;
int BLOCK;
int SPES;


int main(int argc, char **argv)
{
    unsigned int i, j;
    float tpc_usec;
    //unsigned int ref_msec;
    unsigned long long start, end;
    int num_errors = 0;
    
    float *a;

    if(argc != 4) {
      fprintf(stderr, "Wrong syntax\n");
      fprintf(stderr, "%s <N> <BLOCK> <SPES>\n", argv[0]);
      exit(1);
    }

    N     = atoi(argv[1]);
    BLOCK = atoi(argv[2]);
    SPES  = atoi(argv[3]);

    if(N<=0 || BLOCK<=0 || SPES<1 || SPES>16) {
      fprintf(stderr, "Wrong arguments %d,%d,%d\n", N, BLOCK, SPES);
      exit(1);
    }

    N = N*1024*1024;
    BLOCK = BLOCK*1024;



    tpc_init(SPES);


#ifdef CMP_WITH_REF
    float *Y_ref = (float*)tpc_malloc( sizeof(float) * N); 
    float *X_ref = (float*)tpc_malloc( sizeof(float) * N); 
#else
    float *Y_ref = NULL;
    float *X_ref = NULL; 
#endif    
    float *Y_ret = (float*)tpc_malloc( sizeof(float) * N); 
    float *X_ret = (float*)tpc_malloc( sizeof(float) * N); 
    
    a = tpc_malloc(128);
    assert(a!=NULL);
    a[0] = (float)2.0;
    a[1] = (float)BLOCK;

    printf("Using %d SPEs\n", SPES);
    printf("Creating dataset: size:%d , block:%d\n", N, BLOCK);
    for (i=0; i<N; i++) 
    {
#ifdef CMP_WITH_REF
        X_ref[i] = (float)(i % 99);
        Y_ref[i] = 10.0f + (i % 99);
#endif
        X_ret[i] = (float)(i % 99);
        Y_ret[i] = 10.0f + (i % 99);
    }    

    tpc_reset_stats();
    
    printf("\n\nRunning TPC implementation:\n");
    start = __mftb();
    for(i=0; i<ITERS; ++i) {
      for(j=0; j<N; j+=BLOCK) {
	tpc_call(0, 3,
	    &Y_ret[j], BLOCK*sizeof(float), TPC_INOUT_ARG,
	    &X_ret[j], BLOCK*sizeof(float), TPC_IN_ARG,
	    a,     128,                 TPC_IN_ARG );
      }
      tpc_wait_all();
    }
    end = __mftb();

    //tpc_usec = ((end-start)/14.318)/(float)ITERS;
    tpc_usec = ((end-start)/79.8000)/(float)ITERS;

    float numGOps = (2.0 * N)/1000000000;
    float xferGbytes = (3.0 * N * sizeof(float))/1000000000;
    
    printf("TPC total saxpy running time: %.0f usec -- %.4f GFLOPS  -- %.4f GB/sec\n",
           tpc_usec,
           numGOps/(tpc_usec/1000000),
           xferGbytes/(tpc_usec/1000000) );
    
#ifdef CMP_WITH_REF
    printf("Running reference implementation:\n");
    
    //start = __mftb();    
    for(i=0; i<ITERS; i++) 
    { 
        saxpy_ref(X_ref, Y_ref, *a, N); 
    }
    //end = __mftb();
    //ref_msec = get_timer_micros()/(float)ITERS; 
    
    // results verification
    num_errors = 0;
    
    for (i=0; i<N; i++) { 
        if ( fabs(Y_ret[i]-Y_ref[i]) > 1.0e-4 ) { 
            if ( num_errors < 50 )
                printf("element %d: got %f expected %f\n", i, Y_ret[i], Y_ref[i]); 
            num_errors++; 
        } 
    } 
    //printf("Reference running time: %d usec -- %.4f GFLOPS\n", ref_usec, compute_gflops_from_usec(2. * N, (float)ref_usec));
    printf("number of errors = %d\n", num_errors);
#endif
#ifndef CMP_WITH_REF
    //free(X_init);
    //free(Y_init);
#else
    free(X_ref);
    free(Y_ref);
    free(X_ret);
    free(Y_ret);
#endif
    
    tpc_print_stats(stdout);
    tpc_shutdown();
    return num_errors;
}
