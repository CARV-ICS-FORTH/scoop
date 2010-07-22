/* Cell conv2d::leaf implementation hand-specialized for a 5x5 convolution window size.
 * Written using Cell SPE intrinsics.
 */

#include <assert.h>
#include <stdio.h>
#include "sequoia.h"
#include "conv2d_leaf_cell.h"

void conv2d_leafext(sqArray_t *A, sqArray_t *H, sqArray_t *C)
{
    unsigned int N  = C->numElmts[0];
    unsigned int M  = C->numElmts[1];
    unsigned int U  = 5;
    unsigned int V  = 5;

    assert(H->numElmts[0] == 5);
    assert(H->numElmts[1] == 5);

    float *a = (float*)(((unsigned char*)A->ptr) + (A->offset[0]*A->pitch[1] + A->offset[1])*A->elmtSize);
    float *h = (float*)(((unsigned char*)H->ptr) + (H->offset[0]*H->pitch[1] + H->offset[1])*H->elmtSize);
    float *c = (float*)(((unsigned char*)C->ptr) + (C->offset[0]*C->pitch[1] + C->offset[1])*C->elmtSize);
    unsigned int APITCH = A->pitch[1];
    unsigned int HPITCH = H->pitch[1];
    unsigned int CPITCH = C->pitch[1];

    vector unsigned int mask000f = (vector unsigned int)spu_maskw(1);
    vector unsigned int mask00f0 = (vector unsigned int)spu_maskw(2);
    vector unsigned int mask0f00 = (vector unsigned int)spu_maskw(4);
    vector unsigned int maskf000 = (vector unsigned int)spu_maskw(8);
    
    vector float hrow0_0123 = *((vector float*)(h + 0*HPITCH));
    vector float hrow1_0123 = *((vector float*)(h + 1*HPITCH));
    vector float hrow2_0123 = *((vector float*)(h + 2*HPITCH));
    vector float hrow3_0123 = *((vector float*)(h + 3*HPITCH));
    vector float hrow4_0123 = *((vector float*)(h + 4*HPITCH));
    vector float hrow0_4 = spu_splats(*(h + 0*HPITCH + V-1));
    vector float hrow1_4 = spu_splats(*(h + 1*HPITCH + V-1));
    vector float hrow2_4 = spu_splats(*(h + 2*HPITCH + V-1));
    vector float hrow3_4 = spu_splats(*(h + 3*HPITCH + V-1));
    vector float hrow4_4 = spu_splats(*(h + 4*HPITCH + V-1));

    vector float zero = spu_splats((float)0.0);
    
    // The "horizontal" dimensions become the outer loops; these dimensions are vectorized.
    // Recall that the array is NxM (i.e. the dimensions aren't in alphabetical order).
    int n, m, u, v;
    for (m = 0; m < M; m +=4 ) {
    
        // For "a", create an aligned vector for each offset 0,1,2,3 in the row (i.e.) shift data across lanes.
        vector float arow0_0123 = *((vector float *)(a+ 0*APITCH + m));
        vector float arow0_4567 = *((vector float *)(a+ 0*APITCH + m+4));
        vector float arow0_off0 = arow0_0123;
        vector float arow0_off1 = load_misaligned_vector_float__(4, arow0_0123, arow0_4567);
        vector float arow0_off2 = load_misaligned_vector_float__(8, arow0_0123, arow0_4567);
        vector float arow0_off3 = load_misaligned_vector_float__(12, arow0_0123, arow0_4567);
        
        vector float arow1_0123 = *((vector float *)(a+ 1*APITCH + m));
        vector float arow1_4567 = *((vector float *)(a+ 1*APITCH + m+4));
        vector float arow1_off0 = arow1_0123;
        vector float arow1_off1 = load_misaligned_vector_float__(4, arow1_0123, arow1_4567);
        vector float arow1_off2 = load_misaligned_vector_float__(8, arow1_0123, arow1_4567);
        vector float arow1_off3 = load_misaligned_vector_float__(12, arow1_0123, arow1_4567);
        
        vector float arow2_0123 = *((vector float *)(a+ 2*APITCH + m));
        vector float arow2_4567 = *((vector float *)(a+ 2*APITCH + m+4));
        vector float arow2_off0 = arow2_0123;
        vector float arow2_off1 = load_misaligned_vector_float__(4, arow2_0123, arow2_4567);
        vector float arow2_off2 = load_misaligned_vector_float__(8, arow2_0123, arow2_4567);
        vector float arow2_off3 = load_misaligned_vector_float__(12, arow2_0123, arow2_4567);
        
        vector float arow3_0123 = *((vector float *)(a+ 3*APITCH + m));
        vector float arow3_4567 = *((vector float *)(a+ 3*APITCH + m+4));
        vector float arow3_off0 = arow3_0123;;
        vector float arow3_off1 = load_misaligned_vector_float__(4, arow3_0123, arow3_4567);
        vector float arow3_off2 = load_misaligned_vector_float__(8, arow3_0123, arow3_4567);
        vector float arow3_off3 = load_misaligned_vector_float__(12, arow3_0123, arow3_4567);
        
        for (n = 0; n < N; n++) {

            // Read in the 5th row of "a"; 4 are already ready.
            vector float *arow4_ptr = (vector float *)(a + (n+4)*APITCH + m);
            vector float arow4_0123 = *(arow4_ptr);
            vector float arow4_4567 = *(arow4_ptr + 1);
            vector float arow4_off0 = arow4_0123;
            vector float arow4_off1 = load_misaligned_vector_float__(4, arow4_0123, arow4_4567);
            vector float arow4_off2 = load_misaligned_vector_float__(8, arow4_0123, arow4_4567);
            vector float arow4_off3 = load_misaligned_vector_float__(12, arow4_0123, arow4_4567);
        
            vector float tmp_off0 = zero;
            tmp_off0 = spu_madd(hrow0_0123, arow0_off0, tmp_off0);
            tmp_off0 = spu_madd(hrow1_0123, arow1_off0, tmp_off0);
            tmp_off0 = spu_madd(hrow2_0123, arow2_off0, tmp_off0);
            tmp_off0 = spu_madd(hrow3_0123, arow3_off0, tmp_off0);
            tmp_off0 = spu_madd(hrow4_0123, arow4_off0, tmp_off0);
           
            vector float tmp_off1 = zero;
            tmp_off1 = spu_madd(hrow0_0123, arow0_off1, tmp_off1);
            tmp_off1 = spu_madd(hrow1_0123, arow1_off1, tmp_off1);
            tmp_off1 = spu_madd(hrow2_0123, arow2_off1, tmp_off1);
            tmp_off1 = spu_madd(hrow3_0123, arow3_off1, tmp_off1);
            tmp_off1 = spu_madd(hrow4_0123, arow4_off1, tmp_off1);
            
            vector float tmp_off2 = zero;
            tmp_off2 = spu_madd(hrow0_0123, arow0_off2, tmp_off2);
            tmp_off2 = spu_madd(hrow1_0123, arow1_off2, tmp_off2);
            tmp_off2 = spu_madd(hrow2_0123, arow2_off2, tmp_off2);
            tmp_off2 = spu_madd(hrow3_0123, arow3_off2, tmp_off2);
            tmp_off2 = spu_madd(hrow4_0123, arow4_off2, tmp_off2);
            
            vector float tmp_off3 = zero;
            tmp_off3 = spu_madd(hrow0_0123, arow0_off3, tmp_off3);
            tmp_off3 = spu_madd(hrow1_0123, arow1_off3, tmp_off3);
            tmp_off3 = spu_madd(hrow2_0123, arow2_off3, tmp_off3);
            tmp_off3 = spu_madd(hrow3_0123, arow3_off3, tmp_off3);
            tmp_off3 = spu_madd(hrow4_0123, arow4_off3, tmp_off3);
            
            // Store the addend for each offset into a single vector.
            vector float crow_addend;
#if 0            
            // This was supposed to be a faster version of the code to sum across the float4 vectors (below).
            // It has a bug and gives incorrect results, but since it doesn't really make the program any faster,
            // I didn't bother debugging it.
            // Looking at it, it appears to have 1/2 the ops, though, so I don't know why it didn't help performance.

            crow_addend = sum_across_four_float4s(tmp_off0, tmp_off1, tmp_off2, tmp_off3, mask000f, mask00f0, mask0f00, maskf000);
#else            
            float addend_off0 = sum_across_float4__(tmp_off0);
            float addend_off1 = sum_across_float4__(tmp_off1);
            float addend_off2 = sum_across_float4__(tmp_off2);
            float addend_off3 = sum_across_float4__(tmp_off3);
            crow_addend = spu_insert(addend_off0, crow_addend, 0);
            crow_addend = spu_insert(addend_off1, crow_addend, 1);
            crow_addend = spu_insert(addend_off2, crow_addend, 2);
            crow_addend = spu_insert(addend_off3, crow_addend, 3);
#endif            
            
            // There are 4/5 elements per summed so far, but the last 1/5 input is at the end of each arow_offN.
            // There are 4 _offN vectors being done in parallel, and 4 end bits; do them all at once.
            // There is one madd vector op here per row (5 rows) to clean up the end bits.
            vector float tmp_ends = zero;
            tmp_ends = spu_madd(hrow0_4, arow0_4567, tmp_ends);
            tmp_ends = spu_madd(hrow1_4, arow1_4567, tmp_ends);
            tmp_ends = spu_madd(hrow2_4, arow2_4567, tmp_ends);
            tmp_ends = spu_madd(hrow3_4, arow3_4567, tmp_ends);
            tmp_ends = spu_madd(hrow4_4, arow4_4567, tmp_ends);

            // Rotate the rows of "a": 0 <- 1, 1 <- 2, 2 <- 3, 3 <- 4
            // Each of these is a vector of floats.
            arow0_off0 = arow1_off0; arow0_off1 = arow1_off1; arow0_off2 = arow1_off2; arow0_off3 = arow1_off3; arow0_4567 = arow1_4567;
            arow1_off0 = arow2_off0; arow1_off1 = arow2_off1; arow1_off2 = arow2_off2; arow1_off3 = arow2_off3; arow1_4567 = arow2_4567;
            arow2_off0 = arow3_off0; arow2_off1 = arow3_off1; arow2_off2 = arow3_off2; arow2_off3 = arow3_off3; arow2_4567 = arow3_4567;
            arow3_off0 = arow4_off0; arow3_off1 = arow4_off1; arow3_off2 = arow4_off2; arow3_off3 = arow4_off3; arow3_4567 = arow4_4567;
            
            // Write the computed row into c.
            *((vector float *)(c + n*CPITCH + m)) = spu_add(crow_addend, tmp_ends);;
        }
    }
}

