// ===========================================================================
//
//                              FORTH-ICS / CARV
//
//                        Proprietary and confidential
//                           Copyright (c) 2010-2012
//
//
// ==========================[ Static Information ]===========================
//
// Author        : Iakovos Mavroidis / Spyros Lyberis
// Abstract      : Matrix multiplication benchmark
//
// =============================[ CVS Variables ]=============================
//
// File name     : $RCSfile: matrix_mult_myrmics.c,v $
// CVS revision  : $Revision: 1.2 $
// Last modified : $Date: 2012/10/24 15:22:29 $
// Last author   : $Author: lyberis-spree $
//
// ===========================================================================

#include <myrmics_header.h>


// ===========================================================================
// ===========================================================================
static inline int tile_to_rank(int tile_size, int tile_row, int tile_col) {
  return (tile_row * tile_size + tile_col);
}

// ===========================================================================
// ===========================================================================
void matrix_mult_myrmics_init_a(float *m, int rank, int blk_size_sq) {
  int i;

  //printf("%d: init_a: m = %08X\r\n", sys_get_worker_id(), m);

  for (i = 0; i < blk_size_sq; i++) {
    m[i] = rank + i;
  }
}

// ===========================================================================
// ===========================================================================
void matrix_mult_myrmics_init_b(float *m, int rank, int blk_size_sq) {
  int i;

  //printf("%d: init_b: m = %08X\r\n", sys_get_worker_id(), m);

  for (i = 0; i < blk_size_sq; i++) {
    m[i] = rank - i;
  }
}

// ===========================================================================
// ===========================================================================
void matrix_mult_myrmics_init_c(float *m, int blk_size_sq) {
  int i;

  //printf("%d: init_c: m = %08X\r\n", sys_get_worker_id(), m);

  for (i = 0; i < blk_size_sq; i++) {
    m[i] = 0.0;
  }
}

// ===========================================================================
// ===========================================================================
void matrix_mult_myrmics_do_submatrix(float *a, float *b, float *c,
                                      int blk_size) {
  int i;
  int j;
  int k;

  //printf("%d: do_submatrix: %08X x %08X -> %08X\r\n", sys_get_worker_id(), a, b, c);

  for (i = 0; i < blk_size; i++) {
    for (j = 0; j < blk_size; j++) {
      for (k = 0; k < blk_size; k++) {
        c[i * blk_size + j] += a[i * blk_size + k] * b[k * blk_size + j];
      }
    }
  }
}

// ===========================================================================
// ===========================================================================
void matrix_mult_myrmics_verify(int* r, float **a, float **b, float **c,
                                int tile_size, int blk_size) {

  int   whole_size;
  float verify_val;
  int   i;
  int   i1;
  int   i2;
  int   j;
  int   j1;
  int   j2;
  int   k;
  int   k1;
  int   k2;
  int   k3;
  int   k4;

  printf("Multiplication finished, verifying...\r\n");

  whole_size = tile_size * blk_size;

  //for (i = 0; i < whole_size; i++) {
  //  for (j = 0; j < whole_size; j++) {
  //    verify_val = 0;
  //    for (k = 0; k < whole_size; k++) {
  //      verify_val += a[i / blk_size * tile_size + k / blk_size]
  //                     [i % blk_size * blk_size  + k % blk_size] *
  //                    b[k / blk_size * tile_size + j / blk_size]
  //                     [k % blk_size * blk_size  + j % blk_size];
  //    }
  //    if (c[i / blk_size * tile_size + j / blk_size]
  //         [i % blk_size * blk_size  + j % blk_size] != verify_val) {
  //      printf("Verification [1;31mFAILED[0m at C[%d, %d]\r\n", i, j);
  //      sys_abort();
  //    }
  //  }
  //}
  for (i = 0; i < whole_size; i++) {
    i1 = i / blk_size * tile_size;
    i2 = i % blk_size * blk_size;
    for (j = 0; j < whole_size; j++) {
      verify_val = 0;
      j1 = j / blk_size;
      j2 = j % blk_size;
      for (k = 0; k < whole_size; k++) {
        k1 = k / blk_size;
        k2 = k % blk_size;
        k3 = k1 * tile_size;
        k4 = k2 * blk_size;
        verify_val += a[i1 + k1][i2 + k2] * b[k3 + j1][k4  + j2];
      }
      if (c[i1 + j1][i2  + j2] != verify_val) {
        printf("Verification [1;31mFAILED[0m at C[%d, %d]\r\n", i, j);
        sys_abort();
      }
    }
  }
  printf("Verification [1;32mPASSED[0m\r\n");
}

// ===========================================================================
// ===========================================================================
#if 0
void matrix_mult_myrmics_print_matrix(float *matrix, int size) {
  int i, j;
  int integer;
  int comma;

  for (i = 0; i < size; i++) {
    for (j = 0; j < size; j++) {
      integer = matrix[i * size + j];
      comma = (matrix[i * size + j] - integer) * 1000;

      printf("%4d.%01d ", integer, comma);
    }
    printf("\r\n");
  }
}
#endif


// ===========================================================================
// matrix_mult_myrmics()        Myrmics version of dense matrix multiplication.
//                              Tiles are laid out in a 2D space
//                              tile_size * tile_size layout. Each tile
//                              has a portion of matrices A and B (each such
//                              portion blk_size * blk_size numbers) and
//                              computes a portion of matrix C = A * B.
// ===========================================================================
// * INPUTS
//   int tile_size              Number of tile rows and columns in the 2D layout
//   int blk_size               Number of rows and columns for each portion.
//                              The total size of A, B and C is
//                              tile_size ^ 2 * blk_size ^ 2 elements.
//   int verify                 0: don't verify the multiplication
//                              1: gathers all matrices back and do the
//                                 verification
// ===========================================================================
void matrix_mult_myrmics(int tile_size, int blk_size, int verify) {

  int*                 r;
  int                   num_tiles;
  int                   tile_row;
  int                   tile_col;
  int                   blk_size_sq = 0;
  int                   whole_size = 0;
  int                   whole_size_sq = 0;
  int                   phase;
  float                 **a;
  float                 **b;
  float                 **c;
  float                 *a_tile;
  float                 *b_tile;
  float                 *c_tile;
  int                   rank;
  int                   i;
  int                   j;
  int                   k;


  // Create holding region
  r = sys_ralloc(0, 99); // highest level

  // Create arrays
  num_tiles = tile_size * tile_size;
  blk_size_sq = blk_size * blk_size;
  whole_size = tile_size * blk_size;
  whole_size_sq = whole_size * whole_size;

  a = sys_alloc(num_tiles * sizeof(float *), r);
  b = sys_alloc(num_tiles * sizeof(float *), r);
  c = sys_alloc(num_tiles * sizeof(float *), r);

  sys_balloc(blk_size_sq * sizeof(float), r, num_tiles, a);
  sys_balloc(blk_size_sq * sizeof(float), r, num_tiles, b);
  sys_balloc(blk_size_sq * sizeof(float), r, num_tiles, c);


  // Initialize A and B with some values, zero out C
  for (tile_row = 0; tile_row < tile_size; tile_row++) {
    for (tile_col = 0; tile_col < tile_size; tile_col++) {

      rank = tile_to_rank(tile_size, tile_row, tile_col);

      a_tile = a[tile_row * tile_size + tile_col];
      //#pragma myrmics task inout(a_tile) in(rank) in(blk_size_sq)
      matrix_mult_myrmics_init_a(a_tile, rank, blk_size_sq);

      b_tile = b[tile_row * tile_size + tile_col];
      //#pragma myrmics task inout(b_tile) in(rank) in(blk_size_sq)
      matrix_mult_myrmics_init_b(b_tile, rank, blk_size_sq);

      c_tile = c[tile_row * tile_size + tile_col];
      //#pragma myrmics task inout(c_tile) in(blk_size_sq)
      matrix_mult_myrmics_init_c(c_tile, blk_size_sq);
    }
  }

  // Print we're starting
  printf("Matrix multiplication of %d x %d starting split into %d tile(s)\r\n",
         whole_size, whole_size, num_tiles);


  // For all the phases in the algorithm
  for (phase = 0; phase < tile_size; phase++) {

    // For all tiles
    for (tile_row = 0; tile_row < tile_size; tile_row++) {
      for (tile_col = 0; tile_col < tile_size; tile_col++) {

        a_tile = a[tile_row * tile_size + phase];
        b_tile = b[phase * tile_size + tile_col];
        c_tile = c[tile_row * tile_size + tile_col];

        #pragma myrmics task inout(a_tile) inout(b_tile) inout(c_tile) \
                             in(blk_size)
        matrix_mult_myrmics_do_submatrix(a_tile, b_tile, c_tile, blk_size);
      }
    }
  }

  // Gather A, B and C's and verify
  if (verify == 1) {

    // int a_byvalue = (int) a;
    // int b_byvalue = (int) b;
    // int c_byvalue = (int) c;

    // // FIXME: a b c should be passed by value (default is an "in" object,
    // //        which is a subset of r, which we do not support currently: they
    // //        are stuck in the r queue, blocked by the same task...)

    // #pragma myrmics task region inout(r) \
    //                      in(a_byvalue) in(b_byvalue) in(c_byvalue) \
    //                      in(tile_size) in(blk_size)
    // matrix_mult_myrmics_verify(r, a_byvalue, b_byvalue, c_byvalue,
    //                            tile_size, blk_size);

    #pragma myrmics task notransfer inout(r) \
                         safe(a) in(a) safe(b) in(b) safe(c) in(c) \
                         in(tile_size) in(blk_size)
    matrix_mult_myrmics_verify(r, a, b, c,
                               tile_size, blk_size);
  }

}
int main(){return 0;}
