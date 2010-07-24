#ifndef _STRIDE_TEST_
#define _STRIDE_TEST_

#define BLOCK_DIM 64
#define BLOCK_SIZE (BLOCK_DIM*BLOCK_DIM*sizeof(int))

#define X_BLOCKS 64
#define Y_BLOCKS 64
#define ALL_BLOCKS (X_BLOCKS*Y_BLOCKS)



struct cb_t {
  int row_size;
  int rows;
  int stride;
  int choice;
} __attribute__ ((aligned(16)));


int put_choice(char *rows, struct cb_t *cb);
int spu_calculation(int *nums, struct cb_t *cb);


#endif
