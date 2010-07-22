#ifndef _TRANSPOSE_H_
#define _TRANSPOSE_H_

struct transargs {
  int block_dim;
  int matrix_dim;
} __attribute__ ((aligned(128)));

void tpc_traspose(float *matrix, int matrix_dim, int block_dim);

#endif

