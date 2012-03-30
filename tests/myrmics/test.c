#include "myrmics_header.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//won't work like this just testing
//won't work like this just testing
//won't work like this just testing
//won't work like this just testing
//won't work like this just testing

static int DIM;
static int BDIM;
static int REALDIM;

// See charm++ jacobi reference implementation
// static void jacobi_block(double inMatrix[REALDIM][REALDIM], double outMatrix[REALDIM][REALDIM],
static void jacobi_block(double *inMatrix, double *outMatrix,
		  double *west_ghostblock, double *east_ghostblock,
		  double *north_ghostline, double *south_ghostline)
{
  int N=DIM;
  int BN=BDIM;

// Update all values in the block ("inner-loop")
  for (int x = 0; x < BN; x++) {
    ///	__builtin_prefetch((void*)&outMatrix[x][0], 1 , 2);
    //	__builtin_prefetch((void*)&outMatrix[x][8], 1 , 2);
    //	__builtin_prefetch((void*)&outMatrix[x][16], 1 , 2);
    for (int y = 0; y < BN; y++) {

      // Start with "self" value
      double sum;
      sum =   inMatrix[x*REALDIM+y] + 
        inMatrix[(x-1)*REALDIM+y] +
        inMatrix[(x+1)*REALDIM+y] +
        inMatrix[x*REALDIM+y-1] +
        inMatrix[x*REALDIM+y+1] ;

      // Calculate new value, difference, and update maxDiff
      outMatrix[x*REALDIM+y] = sum * (double)0.2;
    }
  }
}



void init_block(double *matrix)
{
    unsigned int i, j;
    unsigned int block_dim = BDIM;
    for(i=0; i<block_dim; i++) {
      for(j=0; j<block_dim; j++) {
        matrix[i*REALDIM+j] = (i*j) % 1345;
      }
    }
}



// void (*Task_table[])()={
//     jacobi_block,
//     init_block
// };


// void parallel_jacobi(unsigned int jacobi_iterations, unsigned int N, unsigned int BN, double inMatrix[N][N], double outMatrix[N][N])
void parallel_jacobi(unsigned int jacobi_iterations, unsigned int N, unsigned int BN, double *inMatrix, double *outMatrix)
{
    unsigned int row_length = N*sizeof(double);
    double *arg1, *arg2, *arg3, *arg4, *arg5, *arg6;
        #pragma myrmics task region out(arg1)
        init_block(arg1);
//         tpc_call(1, 1,
    for(int iters=0; iters<jacobi_iterations; iters++) {
      for (int x = BN; x < N-BN; x+=BN) {
        for (int y = BN; y < N-BN; y+=BN) {
          /*jacobi_block(&inMatrix[x][y], &outMatrix[x][y],
            &inMatrix[x][y-BN],
            &inMatrix[x][y+BN],
            &inMatrix[x-1][y],
            &inMatrix[x+BN][y]);*/
          arg1=&inMatrix[x*N+y];
          arg2=&outMatrix[x*N+y];
          arg3=&inMatrix[x*N+y-BN];
          arg4=&inMatrix[x*N+y+BN];
          arg5=&inMatrix[(x-1)*N+y];
          arg6=&inMatrix[(x+BN)*N+y];
          #pragma myrmics task in(\
                            arg1,\
                            arg3,\
                            arg4,\
                            arg5,\
                            arg6\
                          )\
                          out(arg2)
          jacobi_block(arg1, arg2, arg3, arg4, arg5, arg6);
//           tpc_call(0, 6,
//             &inMatrix[x][y], TPC_BUILD_STRIDEARG(BN, BN*sizeof(double)),
//               TPC_IN_ARG|TPC_STRIDE_ARG, row_length,
// 
//             &outMatrix[x][y], TPC_BUILD_STRIDEARG(BN, BN*sizeof(double)),
//               TPC_OUT_ARG|TPC_STRIDE_ARG, row_length,
// 
//             &inMatrix[x][y-BN],TPC_BUILD_STRIDEARG(BN, BN*sizeof(double)),
//               TPC_IN_ARG|TPC_STRIDE_ARG, row_length,
// 
//             &inMatrix[x][y+BN],TPC_BUILD_STRIDEARG(BN, BN*sizeof(double)),
//               TPC_IN_ARG|TPC_STRIDE_ARG, row_length,
// 
//             &inMatrix[x-1][y], BN*sizeof(double), TPC_IN_ARG,
// 
//             &inMatrix[x+BN][y], BN*sizeof(double), TPC_IN_ARG
//           );
        }
      }
      //tpc_wait_all();
      // Swap in and out
      double *tmp = inMatrix;
      inMatrix = outMatrix;
      outMatrix = tmp;
    }
		//won't work like this just testing
    #pragma myrmics wait on(arg1, arg2, arg3, arg4, arg5, arg6)
    ;
}




// void init_matrix(unsigned int matrix_dim, unsigned int block_dim, double matrix[matrix_dim][matrix_dim])
void init_matrix(unsigned int matrix_dim, unsigned int block_dim, double *matrix)
{
    int i,j;
    unsigned int row_length = matrix_dim*sizeof(double);
    double* matr;
    for(i=0; i<matrix_dim; i+=block_dim) {
      for(j=0; j<matrix_dim; j+=block_dim) {
        matr = &matrix[i*matrix_dim+j];
        #pragma myrmics task region out(matr)
        init_block(matr);
//         tpc_call(1, 1,
//           &matrix[i][j], TPC_BUILD_STRIDEARG(block_dim, block_dim*sizeof(double)),
//             TPC_IN_ARG|TPC_STRIDE_ARG, row_length
//           );
      }
    }
		//won't work like this just testing
		#pragma myrmics wait on(matr)
		;
}

void print_matrix(int matrix_dim, double *matrix)
{
    int i,j;
    for(i=0; i<matrix_dim; i++) {
      for(j=0; j<matrix_dim; j++) {
          printf("%lf ", matrix[i*matrix_dim+j]);
      }
    }

}





unsigned long ceil_to_pow2(unsigned long x)
{
    unsigned long leeding_zeros = __builtin_clzl(x);
    unsigned long next_pow2 = 1L <<(64-leeding_zeros) ;
//    printf("clzl: %lu\n", leeding_zeros );
//    printf("x-> %lx | R -> %lx\n", x, next_pow2 );
    return next_pow2;
}


int main(int argc, char *argv[])
{
    int nthreads;
    unsigned int jacobi_iterations;

    if(argc == 5) {
      DIM = atoi(argv[1]);
      BDIM = atoi(argv[2]);
      nthreads = atoi(argv[3]);
      jacobi_iterations = atoi(argv[4]);
    } else {
      fprintf(stderr, "Usage: %s <DIM> <BDIM> <threads> <jacobi_iterations>\n", argv[0]);
      return 1;
    }

    printf( "Jacobi: %4ux%4u matrix\n"
	    "        %4ux%4u blocks\n"
	    "        %4d threads\n"
	    "        %4u iterations\n", DIM,DIM, BDIM,BDIM, nthreads, jacobi_iterations);


    int matrix_dim = DIM + (2*BDIM);

    REALDIM = matrix_dim;
    
    unsigned long grid_size = ceil_to_pow2(matrix_dim*matrix_dim*sizeof(double)*3);

//     tpc_init(nthreads, BDIM*sizeof(double), grid_size);


//     double *inMatrix = (double*)  tpc_malloc(matrix_dim*matrix_dim*sizeof(double));
//     double *outMatrix = (double*) tpc_malloc(matrix_dim*matrix_dim*sizeof(double));
    double *inMatrix = (double*)  malloc(matrix_dim*matrix_dim*sizeof(double));
    double *outMatrix = (double*) malloc(matrix_dim*matrix_dim*sizeof(double));

    init_matrix(matrix_dim, BDIM, inMatrix);

    //print_matrix(matrix_dim, inMatrix);
    printf("\nInit done\n");

    START_TIME();

    parallel_jacobi(jacobi_iterations, matrix_dim, BDIM, inMatrix, outMatrix);

    STOP_TIME();

//     tpc_print_stats(stdout);
//     tpc_shutdown();

    printf("Application time: %.3lf ms \n", (double)GET_TIME()/1000.0);


//     print_matrix(matrix_dim, inMatrix);
    //  /tmp/jacobi_sequenstial_2176_64_1_64 (ray1)

    return 0;
}

