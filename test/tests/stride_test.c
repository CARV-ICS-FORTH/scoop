#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ppu_intrinsics.h>
#include <assert.h>
#include <malloc.h>

// #include <tpc_common.h>
// #include <tpc_ppe.h>

#include "stride_test.h"

#define TABLEDIM (16)

char table[TABLEDIM*TABLEDIM] __attribute__ ((aligned(16)));


void test2(void);

void printf_table()
{
  int i,j;

  for(i=0; i<TABLEDIM; ++i) {
    for(j=0; j<TABLEDIM; ++j) {
      putchar(table[i*TABLEDIM+j]);
    }
    putchar('\n');
  }
}

int put_choice(char *rows, struct cb_t *cb)
{
  int tmp;

  while(cb->rows--) {
    tmp = cb->row_size;
    while(tmp--) {
      *rows = (char)cb->choice;
      ++rows;
    }
    rows+= cb->stride;
  }

  return 0;
}


int spu_calculation(int *nums, struct cb_t *cb)
{
  int i, j;
  
//   assert(cb->rows == BLOCK_DIM);
//   assert(cb->row_size == BLOCK_DIM);

  for(i=0; i<cb->rows; ++i) {
    for(j=0; j<cb->row_size; ++j) {
      *nums += cb->choice;
      ++nums;
    }
    //nums += cb->stride;
  }

  return 0;
}

int main()
{
  struct cb_t *cbp = tpc_malloc(sizeof(struct cb_t));
  assert(cbp && (long)cbp%16==0);

  tpc_init(1);
  
/*  memset(table, 'X', TABLEDIM*TABLEDIM);
  
  cbp->row_size = TABLEDIM;
  cbp->rows = TABLEDIM/2;
  cbp->stride = TABLEDIM;
  cbp->choice = '*';
  //put_choice(table, cbp);
  tpc_call(0, 2,
      table, TPC_BUILD_STRIDEARG(TABLEDIM/2,TABLEDIM),
	  TPC_STRIDE_ARG|TPC_INOUT_ARG, TABLEDIM,
      cbp, sizeof(struct cb_t), TPC_IN_ARG
  );

  tpc_wait_all();

  //cbp->row_size = TABLEDIM;
  //cbp->rows = TABLEDIM/2;
  //cbp->stride = TABLEDIM;
  //cbp->choice = '+';
  //put_choice(table+TABLEDIM, cbp);

  printf("Hello world!\n");
  printf_table();

  int arg1, arg2, arg3;
  arg1 = TPC_BUILD_STRIDEARG(TABLEDIM/2, TABLEDIM);
  printf("arg = %x\n", arg1);
  arg2 = TPC_EXTRACT_STRIDEARG_ELEMS(arg1);
  arg3 = TPC_EXTRACT_STRIDEARG_ELEMSZ(arg1);
  printf("elems=%x, elemsz=%x\n", arg2,arg3);

  */

  /* Begin test 2 */
#if 0
  test2();
#else
  int *matrix = tpc_malloc(ALL_BLOCKS*BLOCK_SIZE);
  struct cb_t *cbp = tpc_malloc(2*sizeof(struct cb_t));
  int i,j, cbflag;
  unsigned long long starttime=0, finishtime=0;
  double utime=0.0;
  int *arg1;
  int stride;
  int elems;
  int elemsz;
  int arg2_size;
  struct cb_t *arg2;

  assert(matrix);
  assert(cbp);

  printf("Setting up initial matrix\n");
  memset(matrix, 0x03, ALL_BLOCKS*BLOCK_SIZE);

  cbp[0].row_size = BLOCK_DIM;
  cbp[0].rows = BLOCK_DIM;
  cbp[0].stride = (X_BLOCKS)*BLOCK_DIM;
  cbp[0].choice = 0x01010101;

  cbp[1].row_size = BLOCK_DIM;
  cbp[1].rows = BLOCK_DIM;
  cbp[1].stride = (X_BLOCKS)*BLOCK_DIM;
  cbp[1].choice = -0x01010101;


  
  //printf_table2(matrix);
  /*printf("Calculating on PPE\n");
  starttime = __mftb();
  cbflag=0;
  for(i=0; i<Y_BLOCKS; ++i) {
    for(j=0; j<X_BLOCKS; ++j) {
      spu_calculation(&matrix[(i*BLOCK_DIM*BLOCK_DIM*X_BLOCKS)+(j*BLOCK_DIM)],
          &cbp[cbflag]);
      cbflag ^= 1;
    }
    cbflag ^= 1;
  }
  finishtime = __mftb();
  printf("After calculation on PPE\n");*/
  //printf_table2(matrix);
  
  
  //printf_table2(matrix);
  printf("Calculating on SPE\n");
  starttime = __mftb();
  cbflag=0;
  for(i=0; i<Y_BLOCKS; ++i) {
    for(j=0; j<X_BLOCKS; ++j) {
      arg1 = &matrix[(i*BLOCK_DIM*BLOCK_DIM*X_BLOCKS)+(j*BLOCK_DIM)];
      arg2 = &cbp[cbflag];
      elems = BLOCK_DIM;
      elemsz = BLOCK_DIM*sizeof(int);
      stride = cbp[cbflag].stride*sizeof(int);
      arg2_size = sizeof(struct cb_t);
      
      #pragma tpc(arg1(inout, stride, elems, elemsz), arg2(in, arg2_size))
      spu_calculation(arg1, arg2);
//       tpc_call(1, 2,
//        &matrix[(i*BLOCK_DIM*BLOCK_DIM*X_BLOCKS)+(j*BLOCK_DIM)],
//            TPC_BUILD_STRIDEARG(BLOCK_DIM,BLOCK_DIM*sizeof(int)),
//            TPC_STRIDE_ARG|TPC_INOUT_ARG, cbp[cbflag].stride*sizeof(int),
//        &cbp[cbflag], sizeof(struct cb_t), TPC_IN_ARG
//       );
      //spu_calculation(&matrix[(i*BLOCK_DIM*BLOCK_DIM*X_BLOCKS)+(j*BLOCK_DIM)],
        //  &cbp[cbflag]);
      cbflag ^= 1;
    }
    cbflag ^= 1;
  }
  tpc_wait_all();
  finishtime = __mftb();
  printf("After calculation on SPE\n");
  //printf_table2(matrix);

  printf("Verifying result...\n");
  int errors=0;
  int tmp[] = {0x04040404, 0x02020202};
  int tmpflag=0, z;
  for(i=0; i<Y_BLOCKS*BLOCK_DIM; ++i) {
    if(i%(BLOCK_DIM) == 0 && i!=0) {
      tmpflag ^= 1;
      //printf("-\n");
    }
    for(j=0; j<X_BLOCKS*BLOCK_DIM; j+=BLOCK_DIM) {
      for(z=0; z<BLOCK_DIM; ++z) {
        if(matrix[(i*X_BLOCKS*BLOCK_DIM)+(j)+z] != tmp[tmpflag]) {
          ++errors;
        }
        //printf("x=%d == %x\n", (i*X_BLOCKS*BLOCK_DIM)+(j)+z, tmp[tmpflag]);
      }
      tmpflag ^= 1;
    }
    //printf("-\n");
  }

  printf("%d errors\n", errors);
  utime = (double)(finishtime-starttime)/79.8;
  printf("time = %lf us\n", utime);
  printf("time = %lf ms\n", utime/1000);
  //tpc_print_stats(stdout);
#endif


  tpc_shutdown();
  return 0;
}




/*****************************************************************************/


void printf_table2(int *numbers)
{
  int i,j;

  for(i=0; i<Y_BLOCKS*BLOCK_DIM; ++i) {
    for(j=0; j<X_BLOCKS*BLOCK_DIM; ++j) {
      printf("%8x ", numbers[i*(X_BLOCKS*BLOCK_DIM)+j]);
      if((j+1)%BLOCK_DIM ==0 ) {
	printf("| ");
      }
    }
    if((i+1)%BLOCK_DIM == 0 ) {
      printf("\n");
      for(j=0; j<(X_BLOCKS*BLOCK_DIM*9)+(X_BLOCKS*2); ++j) {
	printf("+");
      }
    }
    putchar('\n');
  }
}

#if 0
void test2(void)
{
  int *matrix = tpc_malloc(ALL_BLOCKS*BLOCK_SIZE);
  struct cb_t *cbp = tpc_malloc(2*sizeof(struct cb_t));
  int i,j, cbflag;
  unsigned long long starttime=0, finishtime=0;
  double utime=0.0;
  int *arg1;
  int stride;
  int elems;
  int elemsz;
  int arg2_size;
  struct cb_t *arg2;

  assert(matrix);
  assert(cbp);

  printf("Setting up initial matrix\n");
  memset(matrix, 0x03, ALL_BLOCKS*BLOCK_SIZE);

  cbp[0].row_size = BLOCK_DIM;
  cbp[0].rows = BLOCK_DIM;
  cbp[0].stride = (X_BLOCKS)*BLOCK_DIM;
  cbp[0].choice = 0x01010101;

  cbp[1].row_size = BLOCK_DIM;
  cbp[1].rows = BLOCK_DIM;
  cbp[1].stride = (X_BLOCKS)*BLOCK_DIM;
  cbp[1].choice = -0x01010101;


  
  //printf_table2(matrix);
  /*printf("Calculating on PPE\n");
  starttime = __mftb();
  cbflag=0;
  for(i=0; i<Y_BLOCKS; ++i) {
    for(j=0; j<X_BLOCKS; ++j) {
      spu_calculation(&matrix[(i*BLOCK_DIM*BLOCK_DIM*X_BLOCKS)+(j*BLOCK_DIM)],
	  &cbp[cbflag]);
      cbflag ^= 1;
    }
    cbflag ^= 1;
  }
  finishtime = __mftb();
  printf("After calculation on PPE\n");*/
  //printf_table2(matrix);
  
  
  //printf_table2(matrix);
  printf("Calculating on SPE\n");
  starttime = __mftb();
  cbflag=0;
  for(i=0; i<Y_BLOCKS; ++i) {
    for(j=0; j<X_BLOCKS; ++j) {
      arg1 = &matrix[(i*BLOCK_DIM*BLOCK_DIM*X_BLOCKS)+(j*BLOCK_DIM)];
      arg2 = &cbp[cbflag];
      elems = BLOCK_DIM;
      elemsz = BLOCK_DIM*sizeof(int);
      stride = cbp[cbflag].stride*sizeof(int);
      arg2_size = sizeof(struct cb_t);
      
      #pragma tpc(arg1(inout, stride, elems, elemsz), arg2(in, arg2_size))
      spu_calculation(arg1, arg2);
//       tpc_call(1, 2,
// 	  &matrix[(i*BLOCK_DIM*BLOCK_DIM*X_BLOCKS)+(j*BLOCK_DIM)],
// 	      TPC_BUILD_STRIDEARG(BLOCK_DIM,BLOCK_DIM*sizeof(int)),
// 	      TPC_STRIDE_ARG|TPC_INOUT_ARG, cbp[cbflag].stride*sizeof(int),
// 	  &cbp[cbflag], sizeof(struct cb_t), TPC_IN_ARG
//       );
      //spu_calculation(&matrix[(i*BLOCK_DIM*BLOCK_DIM*X_BLOCKS)+(j*BLOCK_DIM)],
	//  &cbp[cbflag]);
      cbflag ^= 1;
    }
    cbflag ^= 1;
  }
  tpc_wait_all();
  finishtime = __mftb();
  printf("After calculation on SPE\n");
  //printf_table2(matrix);

  printf("Verifying result...\n");
  int errors=0;
  int tmp[] = {0x04040404, 0x02020202};
  int tmpflag=0, z;
  for(i=0; i<Y_BLOCKS*BLOCK_DIM; ++i) {
    if(i%(BLOCK_DIM) == 0 && i!=0) {
      tmpflag ^= 1;
      //printf("-\n");
    }
    for(j=0; j<X_BLOCKS*BLOCK_DIM; j+=BLOCK_DIM) {
      for(z=0; z<BLOCK_DIM; ++z) {
	if(matrix[(i*X_BLOCKS*BLOCK_DIM)+(j)+z] != tmp[tmpflag]) {
	  ++errors;
	}
	//printf("x=%d == %x\n", (i*X_BLOCKS*BLOCK_DIM)+(j)+z, tmp[tmpflag]);
      }
      tmpflag ^= 1;
    }
    //printf("-\n");
  }

  printf("%d errors\n", errors);
  utime = (double)(finishtime-starttime)/79.8;
  printf("time = %lf us\n", utime);
  printf("time = %lf ms\n", utime/1000);
  //tpc_print_stats(stdout);
  return;
}
#endif
