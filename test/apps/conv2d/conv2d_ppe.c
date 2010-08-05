//
// conv2d
//
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ppu_intrinsics.h>
#include <math.h>
#include <assert.h>

#include "include/tpc_common.h"
#include "include/tpc_ppe.h"
#include "conv2d.h"

//#define CMP_WITH_REF


void ref_conv2d( float *bA, float *bH, float *bC, 
                 unsigned int sN, unsigned int sM,
                 unsigned int sU, unsigned int sV)
{
    unsigned int n, m, u, v;
    for (n = 0; n < sN; n++) {
        for (m = 0; m < sM; m++) {
            *(bC + n*sM + m) = 0;
            for (u = 0; u < sU; u++) {
                for (v = 0; v < sV; v++) {
                    *(bC + n*sM + m) += (*(bH + u*sV + v)) 
                        * (*(bA + (n+u)*(sM+sV-1) + (m+v)));
                }
            }
        }
    }
   
}

void conv2d_leafext(float *A, float *H, float *C);

int main(int argc, char **argv)
{
    int i,j;
    float tpc_msec;
    float ref_msec;
    int spes;
    unsigned long long start, end;

    if(argc != 2) {
      fprintf(stderr, "Wrong syntax\n");
      fprintf(stderr, "%s <SPES>\n", argv[0]);
      exit(1);
    }

    spes = atoi(argv[1]);
    if(spes<=0 || spes>16) {
      fprintf(stderr, "Wrong number of SPEs (%d)\n", spes);
      exit(1);
    }
      
    tpc_init(spes);
    
    float *A_init = (float *)tpc_malloc( sizeof(float) * (M+V-1) );
    float *C_init = (float *)tpc_malloc( sizeof(float) * M );
    float *A_ref = (float *)tpc_malloc( ceil128( (sizeof(float) * (N+U-1) * (M+V-1)) ) );
    float *C_ret = (float *)tpc_malloc( sizeof(float) * M 		* N       );
#ifdef CMP_WITH_REF
    float *C_ref = (float *)tpc_malloc( sizeof(float) * M 		* N       );
#endif
    float *H_ref = (float *)tpc_malloc( ceil128( (sizeof(float) * U	  * V     )  ) );
    float *H_ref_tpc = (float *)tpc_malloc( ceil128( (sizeof(float) *  9	  * 12     )  ) );
    
    printf("Running conv2d, UxV=%dx%d, N=%d, M=%d, S=%d, T=%d\n",U,V,N,M,S,T);
    printf("Initializing input arrays\n");
    srand(0);
    // initialize the input arrays
    printf("Initialize array A\n");
    for ( i=0; i < N+U-1; ++i )
    {
        for ( j=0; j < M+V-1; ++j )
        {
            A_init[j] = (float)rand() / RAND_MAX;
        }
        memcpy(&((float (*)[M+V-1])A_ref)[i][0], A_init, sizeof(float)*(M+V-1));
    }

    printf("Initialize array H\n");
    for ( i=0; i < U; ++i )
    {
        for ( j=0; j < V; ++j )
        {
            *(H_ref + i*V + j) = (float)rand() / RAND_MAX;
            *(H_ref_tpc + i*12 + j) = *(H_ref + i*V + j);
        }
    }

    // Initalize "C" to something, despite it being an OUT varirable.
    // On CELL, the memory should be touched once before the test runs to load
    // TLB entries.
    printf("Initialize array C\n");
    for ( i=0; i < N; ++i )
    {
        for ( j=0; j < M; ++j )
        {
            C_init[j] = (float)rand() / RAND_MAX;
        }
        memcpy(&((float (*)[M])C_ret)[i][0], C_init, sizeof(float)*M);
#ifdef CMP_WITH_REF
        memcpy(&((float (*)[M])C_ref)[i][0], C_init, sizeof(float)*M);
#endif
    }


    tpc_reset_stats();
    
    printf("\n\nRunning TPC implementation:\n");
    
    start = __mftb();
/*    for(i=0; i<(N+S-1)/S; ++i) {
      for(j=0; j<(M+T-1)/T; ++j) {
	//printf("%d, %d\n", i*S*M, j*T);
	tpc_call(0, 3,
	    &C_ret[(i*S*M)+(j*T)], TPC_BUILD_STRIDEARG(S, T*sizeof(float)),
		TPC_INOUT_ARG|TPC_STRIDE_ARG, M*sizeof(float),
	    
	    &A_ref[(i*S*M)+(j*T)], TPC_BUILD_STRIDEARG((S+U-1), (T+V-1)*sizeof(float)),
		TPC_IN_ARG|TPC_STRIDE_ARG, M*sizeof(float),
	    
	    H_ref, ceil128(sizeof(float)*U*V), TPC_IN_ARG
	);
      }
    }
    tpc_wait_all();*/

    float *rowC = C_ret;
    float *rowA = A_ref;
    float *arg1, *arg2;
    int arg3size = 512;
    int stride1 = M*sizeof(float);
    int stride2 = (M+V-1)*sizeof(float);
    int elems1 = S;
    int elemsz1 = T*sizeof(float);
    int elems2 = (S+U-1);
    int elemsz2 = (T+V-1)*sizeof(float);
    for(i=0; i<N; i+=S) {
      for(j=0; j<M; j+=T) {
	//printf("%d, %d\n", i*S*M, j*T);
        arg1 = &rowC[j];
        arg2 = &rowA[j];
        #pragma tpc(arg1(out, stride1, elems1, elemsz1), arg1(in, stride2, elems2, elemsz2), H_ref_tpc(in, arg3size))
        conv2d_leafext(arg1, arg2, H_ref_tpc);
// 	tpc_call(0, 3,
// 	    &rowC[j], TPC_BUILD_STRIDEARG(S, T*sizeof(float)),
// 		TPC_OUT_ARG|TPC_STRIDE_ARG, M*sizeof(float),
// 	    
// 	    &rowA[j], TPC_BUILD_STRIDEARG((S+U-1), (T+V-1)*sizeof(float)),
// 		TPC_IN_ARG|TPC_STRIDE_ARG, (M+V-1)*sizeof(float),
// 	    
// 	    //H_ref_tpc, ceil128(sizeof(float)*U*V), TPC_IN_ARG
// 	    H_ref_tpc, 512, TPC_IN_ARG
// 	);
      }
      rowC += M*S;
      rowA += (M+V-1)*S;
      //assert((int)rowA%16 == 0);
    }
    tpc_wait_all();

    end = __mftb();
    
    tpc_msec = (end-start)/79800.0;
    //tpc_msec = (end-start)/14318.0;
    
    // Number of ops: 2*N*M*U*V
    // Number of words transferred (COUNTING DUPLICATES!): 
		//		N*M + U*V + (N/S)*(M/T)*(S+U-1)*(T+V-1)
    float numGOps = (2.0 * (float)N * (float)M * (float)U * (float)V)/1000000000.0;
    float numGXfer = ((float)(sizeof(float)) * ( N*M + U*V + (N/S)*(M/T)*(S+U-1)*(T+V-1) ))/1000000000.0;
    
    printf("TPC total conv2d running time: %.3f msec -- %.4f GFLOPS  -- %.4f GB/sec\n",
           tpc_msec,
           numGOps/(tpc_msec/1000.0),
           numGXfer/(tpc_msec/1000.0) );
    
#ifdef CMP_WITH_REF
    printf("Running reference implementation:\n");
    start = __mftb();
    ref_conv2d( A_ref, H_ref, C_ref, N, M, U, V );
    end = __mftb();
    ref_msec = (end-start)/79800.0;
    //ref_msec = (end-start)/14318.0;
    // compare the results
    unsigned int numErrors = 0;
    for ( i=0; i < N; ++i )
    {
        for ( j=0; j < M; ++j )
        {
            float valSequoia = *(C_ret + i*M + j);
            float valRef	 = *(C_ref + i*M + j);

            //printf("[%d,%d]     %.1f    %.1f\n", i,j,valSequoia, valRef);
            if (((valSequoia - valRef > 0.001) || (valSequoia - valRef < -0.001)))
            {
                numErrors++;
                if ( numErrors < 10 )
                {
                    printf("tpc[%d][%d]:%f, reference[%d][%d]:%f\n", 
                           i, j, valSequoia, i, j, valRef);
                }
            }
        }
    }
    
    printf("Number of errors = %d\n", numErrors);
    printf("PPE reference time : %.3f msec\n", ref_msec);
    
#endif
#ifdef CMP_WITH_REF
    free(A_ref);
    free(C_ref);
    free(C_ret);
#endif
    free(A_init);
    free(C_init);
    free(H_ref);
    
    tpc_print_stats(stdout);
    tpc_shutdown();
    return 0;
}
