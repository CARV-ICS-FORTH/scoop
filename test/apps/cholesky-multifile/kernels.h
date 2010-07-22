#ifndef KERNELS_H
#define KERNELS_H


// #pragma css task inout(A[64][64])
void spu_spotrf_tile(float *A);

// #pragma css task input(A[64][64], B[64][64]) inout(C[64][64])
void spu_sgemm_tile(float *A, float *B, float *C);

// #pragma css task input(T[64][64]) inout(B[64][64])
void spu_strsm_tile(float *T, float *B);

// #pragma css task input(A[64][64]) inout(C[64][64])
void spu_ssyrk_tile(float *A, float *C);


#endif /* KERNELS_H */

