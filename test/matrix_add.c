#include <stdio.h>
#include <ppu_intrinsics.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <malloc.h>
#include <math.h>
#include <assert.h>

#define TIMEBASE 79800.0
#define CORE_CYCLES 40

#define ceil16(v)   (((v) + 15U) & ~15U)
#define ceil128(v)   (((v) + 127U) & ~127U)
#define ceil4096(v)   (((v) + 4095U) & ~4095U)

void printMatrix(float *matrix, int row_size);
float checksumMatrix(float *matrix, int row_size);
int countErrors(float *matrixA, float *matrixB, int row_size);


int matrix_add_row(float *a, float *b, int *row_size)
{
  int i;
  int rowsz = row_size[0];

#ifdef NO_UNROLL
  for(i=0; i<rowsz; ++i) {
    a[i] = a[i] + b[i];
  }
#else
  for(i=0; i<rowsz; i+=16) {
    a[i+ 0] = a[i+ 0] + b[i+ 0];
    a[i+ 1] = a[i+ 1] + b[i+ 1];
    a[i+ 2] = a[i+ 2] + b[i+ 2];
    a[i+ 3] = a[i+ 3] + b[i+ 3];
    a[i+ 4] = a[i+ 4] + b[i+ 4];
    a[i+ 5] = a[i+ 5] + b[i+ 5];
    a[i+ 6] = a[i+ 6] + b[i+ 6];
    a[i+ 7] = a[i+ 7] + b[i+ 7];
    a[i+ 8] = a[i+ 8] + b[i+ 8];
    a[i+ 9] = a[i+ 9] + b[i+ 9];
    a[i+10] = a[i+10] + b[i+10];
    a[i+11] = a[i+11] + b[i+11];
    a[i+12] = a[i+12] + b[i+12];
    a[i+13] = a[i+13] + b[i+13];
    a[i+14] = a[i+14] + b[i+14];
    a[i+15] = a[i+15] + b[i+15];
  }
#endif

  return 0;
}



int main(int argc, char **argv)
{
  float *matrixA, *matrixB, *matrixResult, *matrix_dummy;
  //int tpc_row_size[4] __attribute__ ((aligned (16))); // Big address (stack)
  int matrix_size;
  int verify=1;
  int print_matrix=0;
  int print_stats=1;
  int total_spes=1;
  int i, j;
  int *tpc_row_size, row_size, tpc_args, tmp;
  unsigned long long starttime=0, issuetime=0, finishtime=0;
  int total_tasks;

  if(argc < 2) {
    fprintf(stderr, "Give row size (multiple of 16) and max_spes\n");
    exit(1);
  } 
  tmp = atoi(argv[1]);
  if(tmp < 4) {
    fprintf(stderr, "Row size must be multiple of 16 \n");
    exit(1);
  }

  if(argc == 3) {
    total_spes = atoi(argv[2]);
  }

  tmp = ceil16(tmp);
  
  row_size=tmp;
  total_tasks=row_size;
  tpc_args=3;
  matrix_size = row_size*row_size;

  printf("\n");
  printf("Total SPEs   : %d\n", total_spes);
  printf("Row size     : %d (%d bytes)\n", row_size, row_size<<2);
  printf("Matrix size  : %d (%d byted)\n", matrix_size, matrix_size<<2);
  if(verify) {
    printf("Total memory : %d byted\n", (matrix_size<<2)*4 + 16);
  } else {
    printf("Total memory : %d byted\n", (matrix_size<<2)*3 + 16);
  }

  printf("\nAllocating memory and initializing matrices...\n");
  matrixA = (float *)tpc_malloc(matrix_size*sizeof(float));
  matrixB = (float *)tpc_malloc(matrix_size*sizeof(float));
  matrix_dummy = (float *)tpc_malloc(matrix_size*sizeof(float));
  tpc_row_size = (int *)tpc_malloc(4*sizeof(int));
  assert(tpc_row_size);
  assert(matrixA && matrixB && matrix_dummy);

  printf("MatrixA  : %p -> %p\n", matrixA, matrixA+matrix_size);
  printf("MatrixB  : %p -> %p\n", matrixB, matrixB+matrix_size);
  printf("Matrix_d : %p -> %p\n", matrix_dummy, matrix_dummy+matrix_size);
  printf("Row_size : %p -> %p\n", tpc_row_size, tpc_row_size+4);

  //row_size *= 2;
  *tpc_row_size = row_size;
  srand48(0);

  printf("Initializing arrays...\n");
  for(i=0; i<row_size; ++i) {
    for(j=0; j<row_size; ++j) {
      matrixA[i*row_size+j] = (float)drand48()+0.01;
      matrixB[i*row_size+j] = (float)drand48()+0.01;
    }
  }

  if(verify) {
    if(print_matrix) {
      printf("Initial matrixA:\n");
      printMatrix(matrixA, row_size);
      printf("Initial matrixB:\n");
      printMatrix(matrixB, row_size);
    }
    matrixResult = (float *)tpc_malloc(matrix_size*sizeof(float));
    assert(matrixResult);
    printf("Pre-computing expexted result for verification...\n");
    memcpy((void *)matrixResult, (void *)matrixA, matrix_size*sizeof(float));
    for(i=0; i<row_size; ++i) {
      matrix_add_row(&matrixResult[i*row_size], &matrixB[i*row_size], tpc_row_size);
    }
  }


  // Initializing TPC runtime
  tpc_init(total_spes);


/*  printf("\nPerforming warm up computation...\n");

  for(i=0; i<total_tasks; ++i) {
    //matrix_add_row(&matrixA[i*row_size], &matrixB[i*row_size], tpc_row_size);
    tpc_call( 0, tpc_args,
	      &matrix_dummy[i*row_size], row_size<<2, TPC_INOUT_ARG,
	      &matrixB[i*row_size], row_size<<2, TPC_IN_ARG,
	      tpc_row_size, 16, TPC_IN_ARG
	    );
  }
  tpc_wait_all();
  


  tpc_reset_stats();
*/
  int arg_size_in = row_size<<2;
  float* arg1;
  float* arg2;
  int arg_size3 = 16;
  
  printf("\nPerforming actual computation...\n");
  starttime = __mftb();
  for(i=0; i<total_tasks; ++i) {
    arg1=&matrixA[i*row_size];
    arg2=&matrixB[i*row_size];
    #pragma tpc(arg1(inout, arg_size_in), arg2(in, arg_size_in), tpc_row_size(in, arg_size3))
    matrix_add_row(arg1, arg2, tpc_row_size);
//     tpc_call( 0, tpc_args,
// 	      &matrixA[i*row_size], row_size<<2, TPC_INOUT_ARG,
// 	      &matrixB[i*row_size], row_size<<2, TPC_IN_ARG,
// 	      tpc_row_size, 16, TPC_IN_ARG
// 	    );
    //printf("PPU adder: %p(%f),  %p(%f), %p (%d)\n", matrixA, matrixA[4111],
	//matrixB, matrixB[4111], tpc_row_size, tpc_row_size[0]);
    //printf("PPU adder: {%d,%d,%d,%d} \n", tpc_row_size[0],
	//tpc_row_size[1], tpc_row_size[2], tpc_row_size[3]);
    //usleep(50000);
    //exit(1);
  }

  issuetime = __mftb();

  tpc_wait_all();
  
  finishtime = __mftb();

  printf("Computation completed.\n");

  printf("\n");
  printf("Timings\n");
  printf("-------\n");
  printf("starttime           : 0x%.16llx \n", starttime);
  printf("issuetime           : 0x%.16llx \n", issuetime);
  printf("finishtime          : 0x%.16llx \n", finishtime);
  printf("\n");
  printf("Total time   (ms)   : %lf \n", (double)(finishtime-starttime)/TIMEBASE);
  printf("Issue time   (ms)   : %lf \n", (double)(issuetime-starttime)/TIMEBASE);
  printf("Wait time    (ms)   : %lf \n", (double)(finishtime-issuetime)/TIMEBASE);
  printf("Average time (ms)   : %lf \n", ((double)(finishtime-starttime)/total_tasks)/TIMEBASE);
  printf("Total GFLOPS        : %lf \n", ((double)matrix_size/
      ((double)(finishtime-starttime)/(TIMEBASE*1000))/1000000000));
  printf("\n");
  printf("Total time   (core) : %llu \n", (finishtime-starttime)*CORE_CYCLES);
  printf("Issue time   (core) : %llu \n", (issuetime-starttime)*CORE_CYCLES);
  printf("Wait time    (core) : %llu \n", (finishtime-issuetime)*CORE_CYCLES);
  printf("Average time (core) : %llu \n", ((finishtime-starttime)/total_tasks)*CORE_CYCLES);
  printf("\n");
  printf("Total time   (TBR)  : %llu \n", (finishtime-starttime) );
  printf("Issue time   (TBR)  : %llu \n", (issuetime-starttime) );
  printf("Wait time    (TBR)  : %llu \n", (finishtime-issuetime) );
  printf("Average time (TBR)  : %llu \n", ( (finishtime-starttime)/total_tasks) );


  if(verify) {
    int errors;
    printf("\nVerifying result...\n");
    if( (errors=countErrors(matrixResult, matrixA, row_size)) == 0) {
      printf("    TEST PASSED \n" );
    } else {
      printf("    TEST FAILED (%d errors out of %d values %f%%)\n", errors,
	  row_size*row_size, ((float)errors/(row_size*row_size))*100);
      if(print_matrix) {
	printf("Expexted Matrix:\n");
	printMatrix(matrixResult, row_size);
	printf("Matrix A:\n");
	printMatrix(matrixA, row_size);
      }
    }
  }

  tpc_shutdown();

  if(print_stats) {
    tpc_print_stats(stdout);
  }
  return 0;
}



void printMatrix(float *matrix, int row_size)
{
  int i, j;

  for(i=0; i<row_size; ++i) {
    for(j=0; j<row_size; ++j) {
      printf("%f ", matrix[i*row_size+j]);
    }
    printf("\n");
  }
}



float checksumMatrix(float *matrix, int row_size)
{
  int i, j;
  float sum=0.0;

  for(i=0; i<row_size; ++i) {
    for(j=0; j<row_size; ++j) {
      sum += matrix[i*row_size+j];
    }
  }

  return sum;
}


int countErrors(float *matrixA, float *matrixB, int row_size)
{
  int i, j;
  float diff=0.000001;
  int errors=0;

  for(i=0; i<row_size; ++i) {
    for(j=0; j<row_size; ++j) {
      if( fabsf(matrixA[i*row_size+j] - matrixB[i*row_size+j]) > diff ) {
	/*printf("Error: matrixA[%d][%d]=%f != matrixB[%d][%d]=%f\n", i, j,
	    matrixA[i*row_size+j], i, j, matrixB[i*row_size+j]);*/
	++errors;
      }
    }
  }

  return errors;
}

