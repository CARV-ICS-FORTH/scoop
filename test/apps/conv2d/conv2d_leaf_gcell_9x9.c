#include <assert.h>
#include <stdio.h>
#include <spu_intrinsics.h>

#include "conv2d_leaf_cell.h"
#include "conv2d.h"

  
void conv2d_leafext(float *A, float *H, float *C)
{
    unsigned long lU = 9;
    unsigned long lV = 9;
    unsigned long lN = S; //C->numElmts[0];
    unsigned long lM = T; //C->numElmts[1];

    // Check that h is 9x9, and that each row is aligned on a 16-byte boundary; this
    // is true due to the padding that the compiler performs.
    //assert(H->numElmts[0] == 9);
    //assert(H->numElmts[1] == 9);
#if 0
    assert(((unsigned int)sqElmtSpec2D(conv2d_leafext,c,0,0) & (unsigned int)0x0000000f) == 0);
    assert(((unsigned int)sqElmtSpec2D(conv2d_leafext,h,0,0) & (unsigned int)0x0000000f) == 0);
    assert(((unsigned int)sqElmtSpec2D(conv2d_leafext,a,0,0) & (unsigned int)0x0000000f) == 0);
#endif
    
    //float *a = (float*)(((unsigned char*)A->ptr) + (A->offset[0]*A->pitch[1] + A->offset[1])*A->elmtSize);
    //float *h = (float*)(((unsigned char*)H->ptr) + (H->offset[0]*H->pitch[1] + H->offset[1])*H->elmtSize);
    //float *c = (float*)(((unsigned char*)C->ptr) + (C->offset[0]*C->pitch[1] + C->offset[1])*C->elmtSize);
    
    float *a = A;
    float *h = H;
    float *c = C;

    unsigned int APITCH = lV+T-1;
    unsigned int HPITCH = 12;//lV;
    unsigned int CPITCH = T;

    vector unsigned int mask000f = spu_maskw(1);
    vector unsigned int mask00f0 = spu_maskw(2);
    vector unsigned int mask0f00 = spu_maskw(4);
    vector unsigned int maskf000 = spu_maskw(8);

    vector float hrow0_0123 = *((vector float*)(h + 0*HPITCH));
    vector float hrow1_0123 = *((vector float*)(h + 1*HPITCH));
    vector float hrow2_0123 = *((vector float*)(h + 2*HPITCH));
    vector float hrow3_0123 = *((vector float*)(h + 3*HPITCH));
    vector float hrow4_0123 = *((vector float*)(h + 4*HPITCH));
    vector float hrow5_0123 = *((vector float*)(h + 5*HPITCH));
    vector float hrow6_0123 = *((vector float*)(h + 6*HPITCH));
    vector float hrow7_0123 = *((vector float*)(h + 7*HPITCH));
    vector float hrow8_0123 = *((vector float*)(h + 8*HPITCH));
    
    vector float hrow0_4567 = *((vector float*)(h + 0*HPITCH + 4));
    vector float hrow1_4567 = *((vector float*)(h + 1*HPITCH + 4));
    vector float hrow2_4567 = *((vector float*)(h + 2*HPITCH + 4));
    vector float hrow3_4567 = *((vector float*)(h + 3*HPITCH + 4));
    vector float hrow4_4567 = *((vector float*)(h + 4*HPITCH + 4));
    vector float hrow5_4567 = *((vector float*)(h + 5*HPITCH + 4));
    vector float hrow6_4567 = *((vector float*)(h + 6*HPITCH + 4));
    vector float hrow7_4567 = *((vector float*)(h + 7*HPITCH + 4));
    vector float hrow8_4567 = *((vector float*)(h + 8*HPITCH + 4));

    vector float hrow0_8 = spu_splats(*(h + 0*HPITCH + 8));
    vector float hrow1_8 = spu_splats(*(h + 1*HPITCH + 8));
    vector float hrow2_8 = spu_splats(*(h + 2*HPITCH + 8));
    vector float hrow3_8 = spu_splats(*(h + 3*HPITCH + 8));
    vector float hrow4_8 = spu_splats(*(h + 4*HPITCH + 8));
    vector float hrow5_8 = spu_splats(*(h + 5*HPITCH + 8));
    vector float hrow6_8 = spu_splats(*(h + 6*HPITCH + 8));
    vector float hrow7_8 = spu_splats(*(h + 7*HPITCH + 8));
    vector float hrow8_8 = spu_splats(*(h + 8*HPITCH + 8));

    vector float zero = spu_splats((float)0.0);
    
    // The "horizontal" dimensions become the outer loops; these dimensions are vectorized.
    // We compute 8 elements of a row of c in a SIMD/unrolled way.
    int n, m, u, v;
    for (m = 0; m < lM; m += 8 ) {
    
        // For "a", create an aligned vector for each offset 0,1,2,3,4,5,6,7,8,9,10,11 in the row
        // (i.e.) shift data across lanes.
        
        vector float arow0_0123 = *((vector float *)(a + 0*APITCH + m));
        vector float arow0_4567 = *((vector float *)(a + 0*APITCH + m+4));
        vector float arow0_89ab = *((vector float *)(a + 0*APITCH + m+8));
        vector float arow0_cdef = *((vector float *)(a + 0*APITCH + m+12));
        vector float arow0_off0 = arow0_0123;
        vector float arow0_off1 = load_misaligned_vector_float__(4, arow0_0123, arow0_4567);
        vector float arow0_off2 = load_misaligned_vector_float__(8, arow0_0123, arow0_4567);
        vector float arow0_off3 = load_misaligned_vector_float__(12, arow0_0123, arow0_4567);
        vector float arow0_off4 = arow0_4567;
        vector float arow0_off5 = load_misaligned_vector_float__(4, arow0_4567, arow0_89ab);
        vector float arow0_off6 = load_misaligned_vector_float__(8, arow0_4567, arow0_89ab);
        vector float arow0_off7 = load_misaligned_vector_float__(12, arow0_4567, arow0_89ab);
        vector float arow0_off8 = arow0_89ab;
        vector float arow0_off9 = load_misaligned_vector_float__(4, arow0_89ab, arow0_cdef);
        vector float arow0_off10 = load_misaligned_vector_float__(8, arow0_89ab, arow0_cdef);
        vector float arow0_off11 = load_misaligned_vector_float__(12, arow0_89ab, arow0_cdef);        
        
        vector float arow1_0123 = *((vector float *)(a + 1*APITCH + m));
        vector float arow1_4567 = *((vector float *)(a + 1*APITCH + m+4));
        vector float arow1_89ab = *((vector float *)(a + 1*APITCH + m+8));
        vector float arow1_cdef = *((vector float *)(a + 1*APITCH + m+12));
        vector float arow1_off0 = arow1_0123;
        vector float arow1_off1 = load_misaligned_vector_float__(4, arow1_0123, arow1_4567);
        vector float arow1_off2 = load_misaligned_vector_float__(8, arow1_0123, arow1_4567);
        vector float arow1_off3 = load_misaligned_vector_float__(12, arow1_0123, arow1_4567);
        vector float arow1_off4 = arow1_4567;
        vector float arow1_off5 = load_misaligned_vector_float__(4, arow1_4567, arow1_89ab);
        vector float arow1_off6 = load_misaligned_vector_float__(8, arow1_4567, arow1_89ab);
        vector float arow1_off7 = load_misaligned_vector_float__(12, arow1_4567, arow1_89ab);
        vector float arow1_off8 = arow1_89ab;
        vector float arow1_off9 = load_misaligned_vector_float__(4, arow1_89ab, arow1_cdef);
        vector float arow1_off10 = load_misaligned_vector_float__(8, arow1_89ab, arow1_cdef);
        vector float arow1_off11 = load_misaligned_vector_float__(12, arow1_89ab, arow1_cdef);
        
        vector float arow2_0123 = *((vector float *)(a + 2*APITCH + m));
        vector float arow2_4567 = *((vector float *)(a + 2*APITCH + m+4));
        vector float arow2_89ab = *((vector float *)(a + 2*APITCH + m+8));
        vector float arow2_cdef = *((vector float *)(a + 2*APITCH + m+12));
        vector float arow2_off0 = arow2_0123;
        vector float arow2_off1 = load_misaligned_vector_float__(4, arow2_0123, arow2_4567);
        vector float arow2_off2 = load_misaligned_vector_float__(8, arow2_0123, arow2_4567);
        vector float arow2_off3 = load_misaligned_vector_float__(12, arow2_0123, arow2_4567);
        vector float arow2_off4 = arow2_4567;
        vector float arow2_off5 = load_misaligned_vector_float__(4, arow2_4567, arow2_89ab);
        vector float arow2_off6 = load_misaligned_vector_float__(8, arow2_4567, arow2_89ab);
        vector float arow2_off7 = load_misaligned_vector_float__(12, arow2_4567, arow2_89ab);
        vector float arow2_off8 = arow2_89ab;
        vector float arow2_off9 = load_misaligned_vector_float__(4, arow2_89ab, arow2_cdef);
        vector float arow2_off10 = load_misaligned_vector_float__(8, arow2_89ab, arow2_cdef);
        vector float arow2_off11 = load_misaligned_vector_float__(12, arow2_89ab, arow2_cdef);
        
        vector float arow3_0123 = *((vector float *)(a + 3*APITCH + m));
        vector float arow3_4567 = *((vector float *)(a + 3*APITCH + m+4));
        vector float arow3_89ab = *((vector float *)(a + 3*APITCH + m+8));
        vector float arow3_cdef = *((vector float *)(a + 3*APITCH + m+12));
        vector float arow3_off0 = arow3_0123;
        vector float arow3_off1 = load_misaligned_vector_float__(4, arow3_0123, arow3_4567);
        vector float arow3_off2 = load_misaligned_vector_float__(8, arow3_0123, arow3_4567);
        vector float arow3_off3 = load_misaligned_vector_float__(12, arow3_0123, arow3_4567);
        vector float arow3_off4 = arow3_4567;
        vector float arow3_off5 = load_misaligned_vector_float__(4, arow3_4567, arow3_89ab);
        vector float arow3_off6 = load_misaligned_vector_float__(8, arow3_4567, arow3_89ab);
        vector float arow3_off7 = load_misaligned_vector_float__(12, arow3_4567, arow3_89ab);
        vector float arow3_off8 = arow3_89ab;
        vector float arow3_off9 = load_misaligned_vector_float__(4, arow3_89ab, arow3_cdef);
        vector float arow3_off10 = load_misaligned_vector_float__(8, arow3_89ab, arow3_cdef);
        vector float arow3_off11 = load_misaligned_vector_float__(12, arow3_89ab, arow3_cdef);
        
        vector float arow4_0123 = *((vector float *)(a + 4*APITCH + m));
        vector float arow4_4567 = *((vector float *)(a + 4*APITCH + m+4));
        vector float arow4_89ab = *((vector float *)(a + 4*APITCH + m+8));
        vector float arow4_cdef = *((vector float *)(a + 4*APITCH + m+12));
        vector float arow4_off0 = arow4_0123;;
        vector float arow4_off1 = load_misaligned_vector_float__(4, arow4_0123, arow4_4567);
        vector float arow4_off2 = load_misaligned_vector_float__(8, arow4_0123, arow4_4567);
        vector float arow4_off3 = load_misaligned_vector_float__(12, arow4_0123, arow4_4567);
        vector float arow4_off4 = arow4_4567;
        vector float arow4_off5 = load_misaligned_vector_float__(4, arow4_4567, arow4_89ab);
        vector float arow4_off6 = load_misaligned_vector_float__(8, arow4_4567, arow4_89ab);
        vector float arow4_off7 = load_misaligned_vector_float__(12, arow4_4567, arow4_89ab);
        vector float arow4_off8 = arow4_89ab;
        vector float arow4_off9 = load_misaligned_vector_float__(4, arow4_89ab, arow4_cdef);
        vector float arow4_off10 = load_misaligned_vector_float__(8, arow4_89ab, arow4_cdef);
        vector float arow4_off11 = load_misaligned_vector_float__(12, arow4_89ab, arow4_cdef);
        
        vector float arow5_0123 = *((vector float *)(a + 5*APITCH + m));
        vector float arow5_4567 = *((vector float *)(a + 5*APITCH + m+4));
        vector float arow5_89ab = *((vector float *)(a + 5*APITCH + m+8));
        vector float arow5_cdef = *((vector float *)(a + 5*APITCH + m+12));
        vector float arow5_off0 = arow5_0123;;
        vector float arow5_off1 = load_misaligned_vector_float__(4, arow5_0123, arow5_4567);
        vector float arow5_off2 = load_misaligned_vector_float__(8, arow5_0123, arow5_4567);
        vector float arow5_off3 = load_misaligned_vector_float__(12, arow5_0123, arow5_4567);
        vector float arow5_off4 = arow5_4567;
        vector float arow5_off5 = load_misaligned_vector_float__(4, arow5_4567, arow5_89ab);
        vector float arow5_off6 = load_misaligned_vector_float__(8, arow5_4567, arow5_89ab);
        vector float arow5_off7 = load_misaligned_vector_float__(12, arow5_4567, arow5_89ab);
        vector float arow5_off8 = arow5_89ab;
        vector float arow5_off9 = load_misaligned_vector_float__(4, arow5_89ab, arow5_cdef);
        vector float arow5_off10 = load_misaligned_vector_float__(8, arow5_89ab, arow5_cdef);
        vector float arow5_off11 = load_misaligned_vector_float__(12, arow5_89ab, arow5_cdef);
        
        vector float arow6_0123 = *((vector float *)(a + 6*APITCH + m));
        vector float arow6_4567 = *((vector float *)(a + 6*APITCH + m+4));
        vector float arow6_89ab = *((vector float *)(a + 6*APITCH + m+8));
        vector float arow6_cdef = *((vector float *)(a + 6*APITCH + m+12));
        vector float arow6_off0 = arow6_0123;;
        vector float arow6_off1 = load_misaligned_vector_float__(4, arow6_0123, arow6_4567);
        vector float arow6_off2 = load_misaligned_vector_float__(8, arow6_0123, arow6_4567);
        vector float arow6_off3 = load_misaligned_vector_float__(12, arow6_0123, arow6_4567);
        vector float arow6_off4 = arow6_4567;
        vector float arow6_off5 = load_misaligned_vector_float__(4, arow6_4567, arow6_89ab);
        vector float arow6_off6 = load_misaligned_vector_float__(8, arow6_4567, arow6_89ab);
        vector float arow6_off7 = load_misaligned_vector_float__(12, arow6_4567, arow6_89ab);
        vector float arow6_off8 = arow6_89ab;
        vector float arow6_off9 = load_misaligned_vector_float__(4, arow6_89ab, arow6_cdef);
        vector float arow6_off10 = load_misaligned_vector_float__(8, arow6_89ab, arow6_cdef);
        vector float arow6_off11 = load_misaligned_vector_float__(12, arow6_89ab, arow6_cdef);
        
        vector float arow7_0123 = *((vector float *)(a + 7*APITCH + m));
        vector float arow7_4567 = *((vector float *)(a + 7*APITCH + m+4));
        vector float arow7_89ab = *((vector float *)(a + 7*APITCH + m+8));
        vector float arow7_cdef = *((vector float *)(a + 7*APITCH + m+12));
        vector float arow7_off0 = arow7_0123;;
        vector float arow7_off1 = load_misaligned_vector_float__(4, arow7_0123, arow7_4567);
        vector float arow7_off2 = load_misaligned_vector_float__(8, arow7_0123, arow7_4567);
        vector float arow7_off3 = load_misaligned_vector_float__(12, arow7_0123, arow7_4567);
        vector float arow7_off4 = arow7_4567;
        vector float arow7_off5 = load_misaligned_vector_float__(4, arow7_4567, arow7_89ab);
        vector float arow7_off6 = load_misaligned_vector_float__(8, arow7_4567, arow7_89ab);
        vector float arow7_off7 = load_misaligned_vector_float__(12, arow7_4567, arow7_89ab);
        vector float arow7_off8 = arow7_89ab;
        vector float arow7_off9 = load_misaligned_vector_float__(4, arow7_89ab, arow7_cdef);
        vector float arow7_off10 = load_misaligned_vector_float__(8, arow7_89ab, arow7_cdef);
        vector float arow7_off11 = load_misaligned_vector_float__(12, arow7_89ab, arow7_cdef);
        
        for (n = 0; n < lN; n++) {

            // Read in the 9th row of "a"; 8 are already ready.
            vector float *arow8_ptr = (vector float *)(a + (n+8)*APITCH + m);
            vector float arow8_0123 = *(arow8_ptr);
            vector float arow8_4567 = *(arow8_ptr + 1);
            vector float arow8_89ab = *(arow8_ptr + 2);
            vector float arow8_cdef = *(arow8_ptr + 3);
            vector float arow8_off0 = arow8_0123;
            vector float arow8_off1 = load_misaligned_vector_float__(4, arow8_0123, arow8_4567);
            vector float arow8_off2 = load_misaligned_vector_float__(8, arow8_0123, arow8_4567);
            vector float arow8_off3 = load_misaligned_vector_float__(12, arow8_0123, arow8_4567);
            vector float arow8_off4 = arow8_4567;
            vector float arow8_off5 = load_misaligned_vector_float__(4, arow8_4567, arow8_89ab);
            vector float arow8_off6 = load_misaligned_vector_float__(8, arow8_4567, arow8_89ab);
            vector float arow8_off7 = load_misaligned_vector_float__(12, arow8_4567, arow8_89ab);        
            vector float arow8_off8 = arow8_89ab;
            vector float arow8_off9 = load_misaligned_vector_float__(4, arow8_89ab, arow8_cdef);
            vector float arow8_off10 = load_misaligned_vector_float__(8, arow8_89ab, arow8_cdef);
            vector float arow8_off11 = load_misaligned_vector_float__(12, arow8_89ab, arow8_cdef);        
            
            vector float tmp0123_off0 = zero;
            tmp0123_off0 = spu_madd(hrow0_0123, arow0_off0, tmp0123_off0);
            tmp0123_off0 = spu_madd(hrow1_0123, arow1_off0, tmp0123_off0);
            tmp0123_off0 = spu_madd(hrow2_0123, arow2_off0, tmp0123_off0);
            tmp0123_off0 = spu_madd(hrow3_0123, arow3_off0, tmp0123_off0);
            tmp0123_off0 = spu_madd(hrow4_0123, arow4_off0, tmp0123_off0);
            tmp0123_off0 = spu_madd(hrow5_0123, arow5_off0, tmp0123_off0);
            tmp0123_off0 = spu_madd(hrow6_0123, arow6_off0, tmp0123_off0);
            tmp0123_off0 = spu_madd(hrow7_0123, arow7_off0, tmp0123_off0);
            tmp0123_off0 = spu_madd(hrow8_0123, arow8_off0, tmp0123_off0);
            vector float tmp4567_off0 = zero;
            tmp4567_off0 = spu_madd(hrow0_4567, arow0_off4, tmp4567_off0);
            tmp4567_off0 = spu_madd(hrow1_4567, arow1_off4, tmp4567_off0);
            tmp4567_off0 = spu_madd(hrow2_4567, arow2_off4, tmp4567_off0);
            tmp4567_off0 = spu_madd(hrow3_4567, arow3_off4, tmp4567_off0);
            tmp4567_off0 = spu_madd(hrow4_4567, arow4_off4, tmp4567_off0);
            tmp4567_off0 = spu_madd(hrow5_4567, arow5_off4, tmp4567_off0);
            tmp4567_off0 = spu_madd(hrow6_4567, arow6_off4, tmp4567_off0);
            tmp4567_off0 = spu_madd(hrow7_4567, arow7_off4, tmp4567_off0);
            tmp4567_off0 = spu_madd(hrow8_4567, arow8_off4, tmp4567_off0);
           
            vector float tmp0123_off1 = zero;
            tmp0123_off1 = spu_madd(hrow0_0123, arow0_off1, tmp0123_off1);
            tmp0123_off1 = spu_madd(hrow1_0123, arow1_off1, tmp0123_off1);
            tmp0123_off1 = spu_madd(hrow2_0123, arow2_off1, tmp0123_off1);
            tmp0123_off1 = spu_madd(hrow3_0123, arow3_off1, tmp0123_off1);
            tmp0123_off1 = spu_madd(hrow4_0123, arow4_off1, tmp0123_off1);
            tmp0123_off1 = spu_madd(hrow5_0123, arow5_off1, tmp0123_off1);
            tmp0123_off1 = spu_madd(hrow6_0123, arow6_off1, tmp0123_off1);
            tmp0123_off1 = spu_madd(hrow7_0123, arow7_off1, tmp0123_off1);
            tmp0123_off1 = spu_madd(hrow8_0123, arow8_off1, tmp0123_off1);
            vector float tmp4567_off1 = zero;
            tmp4567_off1 = spu_madd(hrow0_4567, arow0_off5, tmp4567_off1);
            tmp4567_off1 = spu_madd(hrow1_4567, arow1_off5, tmp4567_off1);
            tmp4567_off1 = spu_madd(hrow2_4567, arow2_off5, tmp4567_off1);
            tmp4567_off1 = spu_madd(hrow3_4567, arow3_off5, tmp4567_off1);
            tmp4567_off1 = spu_madd(hrow4_4567, arow4_off5, tmp4567_off1);
            tmp4567_off1 = spu_madd(hrow5_4567, arow5_off5, tmp4567_off1);
            tmp4567_off1 = spu_madd(hrow6_4567, arow6_off5, tmp4567_off1);
            tmp4567_off1 = spu_madd(hrow7_4567, arow7_off5, tmp4567_off1);
            tmp4567_off1 = spu_madd(hrow8_4567, arow8_off5, tmp4567_off1);
            
            vector float tmp0123_off2 = zero;
            tmp0123_off2 = spu_madd(hrow0_0123, arow0_off2, tmp0123_off2);
            tmp0123_off2 = spu_madd(hrow1_0123, arow1_off2, tmp0123_off2);
            tmp0123_off2 = spu_madd(hrow2_0123, arow2_off2, tmp0123_off2);
            tmp0123_off2 = spu_madd(hrow3_0123, arow3_off2, tmp0123_off2);
            tmp0123_off2 = spu_madd(hrow4_0123, arow4_off2, tmp0123_off2);
            tmp0123_off2 = spu_madd(hrow5_0123, arow5_off2, tmp0123_off2);
            tmp0123_off2 = spu_madd(hrow6_0123, arow6_off2, tmp0123_off2);
            tmp0123_off2 = spu_madd(hrow7_0123, arow7_off2, tmp0123_off2);
            tmp0123_off2 = spu_madd(hrow8_0123, arow8_off2, tmp0123_off2);
            vector float tmp4567_off2 = zero;
            tmp4567_off2 = spu_madd(hrow0_4567, arow0_off6, tmp4567_off2);
            tmp4567_off2 = spu_madd(hrow1_4567, arow1_off6, tmp4567_off2);
            tmp4567_off2 = spu_madd(hrow2_4567, arow2_off6, tmp4567_off2);
            tmp4567_off2 = spu_madd(hrow3_4567, arow3_off6, tmp4567_off2);
            tmp4567_off2 = spu_madd(hrow4_4567, arow4_off6, tmp4567_off2);
            tmp4567_off2 = spu_madd(hrow5_4567, arow5_off6, tmp4567_off2);
            tmp4567_off2 = spu_madd(hrow6_4567, arow6_off6, tmp4567_off2);
            tmp4567_off2 = spu_madd(hrow7_4567, arow7_off6, tmp4567_off2);
            tmp4567_off2 = spu_madd(hrow8_4567, arow8_off6, tmp4567_off2);
            
            vector float tmp0123_off3 = zero;
            tmp0123_off3 = spu_madd(hrow0_0123, arow0_off3, tmp0123_off3);
            tmp0123_off3 = spu_madd(hrow1_0123, arow1_off3, tmp0123_off3);
            tmp0123_off3 = spu_madd(hrow2_0123, arow2_off3, tmp0123_off3);
            tmp0123_off3 = spu_madd(hrow3_0123, arow3_off3, tmp0123_off3);
            tmp0123_off3 = spu_madd(hrow4_0123, arow4_off3, tmp0123_off3);
            tmp0123_off3 = spu_madd(hrow5_0123, arow5_off3, tmp0123_off3);
            tmp0123_off3 = spu_madd(hrow6_0123, arow6_off3, tmp0123_off3);
            tmp0123_off3 = spu_madd(hrow7_0123, arow7_off3, tmp0123_off3);
            tmp0123_off3 = spu_madd(hrow8_0123, arow8_off3, tmp0123_off3);
            vector float tmp4567_off3 = zero;
            tmp4567_off3 = spu_madd(hrow0_4567, arow0_off7, tmp4567_off3);
            tmp4567_off3 = spu_madd(hrow1_4567, arow1_off7, tmp4567_off3);
            tmp4567_off3 = spu_madd(hrow2_4567, arow2_off7, tmp4567_off3);
            tmp4567_off3 = spu_madd(hrow3_4567, arow3_off7, tmp4567_off3);
            tmp4567_off3 = spu_madd(hrow4_4567, arow4_off7, tmp4567_off3);
            tmp4567_off3 = spu_madd(hrow5_4567, arow5_off7, tmp4567_off3);
            tmp4567_off3 = spu_madd(hrow6_4567, arow6_off7, tmp4567_off3);
            tmp4567_off3 = spu_madd(hrow7_4567, arow7_off7, tmp4567_off3);
            tmp4567_off3 = spu_madd(hrow8_4567, arow8_off7, tmp4567_off3);
            
            vector float tmp0123_off4 = zero;
            tmp0123_off4 = spu_madd(hrow0_0123, arow0_off4, tmp0123_off4);
            tmp0123_off4 = spu_madd(hrow1_0123, arow1_off4, tmp0123_off4);
            tmp0123_off4 = spu_madd(hrow2_0123, arow2_off4, tmp0123_off4);
            tmp0123_off4 = spu_madd(hrow3_0123, arow3_off4, tmp0123_off4);
            tmp0123_off4 = spu_madd(hrow4_0123, arow4_off4, tmp0123_off4);
            tmp0123_off4 = spu_madd(hrow5_0123, arow5_off4, tmp0123_off4);
            tmp0123_off4 = spu_madd(hrow6_0123, arow6_off4, tmp0123_off4);
            tmp0123_off4 = spu_madd(hrow7_0123, arow7_off4, tmp0123_off4);
            tmp0123_off4 = spu_madd(hrow8_0123, arow8_off4, tmp0123_off4);
            vector float tmp4567_off4 = zero;
            tmp4567_off4 = spu_madd(hrow0_4567, arow0_off8, tmp4567_off4);
            tmp4567_off4 = spu_madd(hrow1_4567, arow1_off8, tmp4567_off4);
            tmp4567_off4 = spu_madd(hrow2_4567, arow2_off8, tmp4567_off4);
            tmp4567_off4 = spu_madd(hrow3_4567, arow3_off8, tmp4567_off4);
            tmp4567_off4 = spu_madd(hrow4_4567, arow4_off8, tmp4567_off4);
            tmp4567_off4 = spu_madd(hrow5_4567, arow5_off8, tmp4567_off4);
            tmp4567_off4 = spu_madd(hrow6_4567, arow6_off8, tmp4567_off4);
            tmp4567_off4 = spu_madd(hrow7_4567, arow7_off8, tmp4567_off4);
            tmp4567_off4 = spu_madd(hrow8_4567, arow8_off8, tmp4567_off4);
            
            vector float tmp0123_off5 = zero;
            tmp0123_off5 = spu_madd(hrow0_0123, arow0_off5, tmp0123_off5);
            tmp0123_off5 = spu_madd(hrow1_0123, arow1_off5, tmp0123_off5);
            tmp0123_off5 = spu_madd(hrow2_0123, arow2_off5, tmp0123_off5);
            tmp0123_off5 = spu_madd(hrow3_0123, arow3_off5, tmp0123_off5);
            tmp0123_off5 = spu_madd(hrow4_0123, arow4_off5, tmp0123_off5);
            tmp0123_off5 = spu_madd(hrow5_0123, arow5_off5, tmp0123_off5);
            tmp0123_off5 = spu_madd(hrow6_0123, arow6_off5, tmp0123_off5);
            tmp0123_off5 = spu_madd(hrow7_0123, arow7_off5, tmp0123_off5);
            tmp0123_off5 = spu_madd(hrow8_0123, arow8_off5, tmp0123_off5);
            vector float tmp4567_off5 = zero;
            tmp4567_off5 = spu_madd(hrow0_4567, arow0_off9, tmp4567_off5);
            tmp4567_off5 = spu_madd(hrow1_4567, arow1_off9, tmp4567_off5);
            tmp4567_off5 = spu_madd(hrow2_4567, arow2_off9, tmp4567_off5);
            tmp4567_off5 = spu_madd(hrow3_4567, arow3_off9, tmp4567_off5);
            tmp4567_off5 = spu_madd(hrow4_4567, arow4_off9, tmp4567_off5);
            tmp4567_off5 = spu_madd(hrow5_4567, arow5_off9, tmp4567_off5);
            tmp4567_off5 = spu_madd(hrow6_4567, arow6_off9, tmp4567_off5);
            tmp4567_off5 = spu_madd(hrow7_4567, arow7_off9, tmp4567_off5);
            tmp4567_off5 = spu_madd(hrow8_4567, arow8_off9, tmp4567_off5);
            
            vector float tmp0123_off6 = zero;
            tmp0123_off6 = spu_madd(hrow0_0123, arow0_off6, tmp0123_off6);
            tmp0123_off6 = spu_madd(hrow1_0123, arow1_off6, tmp0123_off6);
            tmp0123_off6 = spu_madd(hrow2_0123, arow2_off6, tmp0123_off6);
            tmp0123_off6 = spu_madd(hrow3_0123, arow3_off6, tmp0123_off6);
            tmp0123_off6 = spu_madd(hrow4_0123, arow4_off6, tmp0123_off6);
            tmp0123_off6 = spu_madd(hrow5_0123, arow5_off6, tmp0123_off6);
            tmp0123_off6 = spu_madd(hrow6_0123, arow6_off6, tmp0123_off6);
            tmp0123_off6 = spu_madd(hrow7_0123, arow7_off6, tmp0123_off6);
            tmp0123_off6 = spu_madd(hrow8_0123, arow8_off6, tmp0123_off6);
            vector float tmp4567_off6 = zero;
            tmp4567_off6 = spu_madd(hrow0_4567, arow0_off10, tmp4567_off6);
            tmp4567_off6 = spu_madd(hrow1_4567, arow1_off10, tmp4567_off6);
            tmp4567_off6 = spu_madd(hrow2_4567, arow2_off10, tmp4567_off6);
            tmp4567_off6 = spu_madd(hrow3_4567, arow3_off10, tmp4567_off6);
            tmp4567_off6 = spu_madd(hrow4_4567, arow4_off10, tmp4567_off6);
            tmp4567_off6 = spu_madd(hrow5_4567, arow5_off10, tmp4567_off6);
            tmp4567_off6 = spu_madd(hrow6_4567, arow6_off10, tmp4567_off6);
            tmp4567_off6 = spu_madd(hrow7_4567, arow7_off10, tmp4567_off6);
            tmp4567_off6 = spu_madd(hrow8_4567, arow8_off10, tmp4567_off6);
            
            vector float tmp0123_off7 = zero;
            tmp0123_off7 = spu_madd(hrow0_0123, arow0_off7, tmp0123_off7);
            tmp0123_off7 = spu_madd(hrow1_0123, arow1_off7, tmp0123_off7);
            tmp0123_off7 = spu_madd(hrow2_0123, arow2_off7, tmp0123_off7);
            tmp0123_off7 = spu_madd(hrow3_0123, arow3_off7, tmp0123_off7);
            tmp0123_off7 = spu_madd(hrow4_0123, arow4_off7, tmp0123_off7);
            tmp0123_off7 = spu_madd(hrow5_0123, arow5_off7, tmp0123_off7);
            tmp0123_off7 = spu_madd(hrow6_0123, arow6_off7, tmp0123_off7);
            tmp0123_off7 = spu_madd(hrow7_0123, arow7_off7, tmp0123_off7);
            tmp0123_off7 = spu_madd(hrow8_0123, arow8_off7, tmp0123_off7);
            vector float tmp4567_off7 = zero;
            tmp4567_off7 = spu_madd(hrow0_4567, arow0_off11, tmp4567_off7);
            tmp4567_off7 = spu_madd(hrow1_4567, arow1_off11, tmp4567_off7);
            tmp4567_off7 = spu_madd(hrow2_4567, arow2_off11, tmp4567_off7);
            tmp4567_off7 = spu_madd(hrow3_4567, arow3_off11, tmp4567_off7);
            tmp4567_off7 = spu_madd(hrow4_4567, arow4_off11, tmp4567_off7);
            tmp4567_off7 = spu_madd(hrow5_4567, arow5_off11, tmp4567_off7);
            tmp4567_off7 = spu_madd(hrow6_4567, arow6_off11, tmp4567_off7);
            tmp4567_off7 = spu_madd(hrow7_4567, arow7_off11, tmp4567_off7);
            tmp4567_off7 = spu_madd(hrow8_4567, arow8_off11, tmp4567_off7);
            
            // Store the addend for each offset into a single vector.
            vector float crow0123_addend;
            vector float crow4567_addend;
            
            float addend_off0 = sum_across_float4__(spu_add(tmp0123_off0,tmp4567_off0));
            float addend_off1 = sum_across_float4__(spu_add(tmp0123_off1,tmp4567_off1));
            float addend_off2 = sum_across_float4__(spu_add(tmp0123_off2,tmp4567_off2));
            float addend_off3 = sum_across_float4__(spu_add(tmp0123_off3,tmp4567_off3));
            float addend_off4 = sum_across_float4__(spu_add(tmp0123_off4,tmp4567_off4));
            float addend_off5 = sum_across_float4__(spu_add(tmp0123_off5,tmp4567_off5));
            float addend_off6 = sum_across_float4__(spu_add(tmp0123_off6,tmp4567_off6));
            float addend_off7 = sum_across_float4__(spu_add(tmp0123_off7,tmp4567_off7));
            crow0123_addend = spu_insert(addend_off0, crow0123_addend, 0);
            crow0123_addend = spu_insert(addend_off1, crow0123_addend, 1);
            crow0123_addend = spu_insert(addend_off2, crow0123_addend, 2);
            crow0123_addend = spu_insert(addend_off3, crow0123_addend, 3);
            crow4567_addend = spu_insert(addend_off4, crow4567_addend, 0);
            crow4567_addend = spu_insert(addend_off5, crow4567_addend, 1);
            crow4567_addend = spu_insert(addend_off6, crow4567_addend, 2);
            crow4567_addend = spu_insert(addend_off7, crow4567_addend, 3);
            
            // There are 8/9 elements per row summed so far, but the last 1/9 input is at the end of each arow_offN.
            // There are 8 _offN vectors being done in parallel, and 8 end bits; do them all at once in 2 SIMD operations.
            // There are 2 madd vector ops here per row (9 rows) to clean up the end bits.
            vector float tmp0123_ends = zero;
            vector float tmp4567_ends = zero;
            tmp0123_ends = spu_madd(hrow0_8, arow0_89ab, tmp0123_ends);
            tmp0123_ends = spu_madd(hrow1_8, arow1_89ab, tmp0123_ends);
            tmp0123_ends = spu_madd(hrow2_8, arow2_89ab, tmp0123_ends);
            tmp0123_ends = spu_madd(hrow3_8, arow3_89ab, tmp0123_ends);
            tmp0123_ends = spu_madd(hrow4_8, arow4_89ab, tmp0123_ends);
            tmp0123_ends = spu_madd(hrow5_8, arow5_89ab, tmp0123_ends);
            tmp0123_ends = spu_madd(hrow6_8, arow6_89ab, tmp0123_ends);
            tmp0123_ends = spu_madd(hrow7_8, arow7_89ab, tmp0123_ends);
            tmp0123_ends = spu_madd(hrow8_8, arow8_89ab, tmp0123_ends);
            
            tmp4567_ends = spu_madd(hrow0_8, arow0_cdef, tmp4567_ends);
            tmp4567_ends = spu_madd(hrow1_8, arow1_cdef, tmp4567_ends);
            tmp4567_ends = spu_madd(hrow2_8, arow2_cdef, tmp4567_ends);
            tmp4567_ends = spu_madd(hrow3_8, arow3_cdef, tmp4567_ends);
            tmp4567_ends = spu_madd(hrow4_8, arow4_cdef, tmp4567_ends);
            tmp4567_ends = spu_madd(hrow5_8, arow5_cdef, tmp4567_ends);
            tmp4567_ends = spu_madd(hrow6_8, arow6_cdef, tmp4567_ends);
            tmp4567_ends = spu_madd(hrow7_8, arow7_cdef, tmp4567_ends);
            tmp4567_ends = spu_madd(hrow8_8, arow8_cdef, tmp4567_ends);

            // Rotate the rows of "a": 0 <- 1, 1 <- 2, 2 <- 3, 3 <- 4, etc.
            // Each of these is a vector of floats.
            arow0_off0 = arow1_off0; arow0_off1 = arow1_off1; arow0_off2 = arow1_off2; arow0_off3 = arow1_off3; arow0_off4 = arow1_off4; arow0_off5 = arow1_off5; arow0_off6 = arow1_off6; arow0_off7 = arow1_off7; arow0_off8 = arow1_off8; arow0_off9 = arow1_off9; arow0_off10 = arow1_off10; arow0_off11 = arow1_off11; arow0_4567 = arow1_4567; arow0_89ab = arow1_89ab; arow0_cdef = arow1_cdef;
            arow1_off0 = arow2_off0; arow1_off1 = arow2_off1; arow1_off2 = arow2_off2; arow1_off3 = arow2_off3; arow1_off4 = arow2_off4; arow1_off5 = arow2_off5; arow1_off6 = arow2_off6; arow1_off7 = arow2_off7; arow1_off8 = arow2_off8; arow1_off9 = arow2_off9; arow1_off10 = arow2_off10; arow1_off11 = arow2_off11; arow1_4567 = arow2_4567; arow1_89ab = arow2_89ab; arow1_cdef = arow2_cdef;
            arow2_off0 = arow3_off0; arow2_off1 = arow3_off1; arow2_off2 = arow3_off2; arow2_off3 = arow3_off3; arow2_off4 = arow3_off4; arow2_off5 = arow3_off5; arow2_off6 = arow3_off6; arow2_off7 = arow3_off7; arow2_off8 = arow3_off8; arow2_off9 = arow3_off9; arow2_off10 = arow3_off10; arow2_off11 = arow3_off11; arow2_4567 = arow3_4567; arow2_89ab = arow3_89ab; arow2_cdef = arow3_cdef;
            arow3_off0 = arow4_off0; arow3_off1 = arow4_off1; arow3_off2 = arow4_off2; arow3_off3 = arow4_off3; arow3_off4 = arow4_off4; arow3_off5 = arow4_off5; arow3_off6 = arow4_off6; arow3_off7 = arow4_off7; arow3_off8 = arow4_off8; arow3_off9 = arow4_off9; arow3_off10 = arow4_off10; arow3_off11 = arow4_off11; arow3_4567 = arow4_4567; arow3_89ab = arow4_89ab; arow3_cdef = arow4_cdef;
            arow4_off0 = arow5_off0; arow4_off1 = arow5_off1; arow4_off2 = arow5_off2; arow4_off3 = arow5_off3; arow4_off4 = arow5_off4; arow4_off5 = arow5_off5; arow4_off6 = arow5_off6; arow4_off7 = arow5_off7; arow4_off8 = arow5_off8; arow4_off9 = arow5_off9; arow4_off10 = arow5_off10; arow4_off11 = arow5_off11; arow4_4567 = arow5_4567; arow4_89ab = arow5_89ab; arow4_cdef = arow5_cdef;
            arow5_off0 = arow6_off0; arow5_off1 = arow6_off1; arow5_off2 = arow6_off2; arow5_off3 = arow6_off3; arow5_off4 = arow6_off4; arow5_off5 = arow6_off5; arow5_off6 = arow6_off6; arow5_off7 = arow6_off7; arow5_off8 = arow6_off8; arow5_off9 = arow6_off9; arow5_off10 = arow6_off10; arow5_off11 = arow6_off11; arow5_4567 = arow6_4567; arow5_89ab = arow6_89ab; arow5_cdef = arow6_cdef;
            arow6_off0 = arow7_off0; arow6_off1 = arow7_off1; arow6_off2 = arow7_off2; arow6_off3 = arow7_off3; arow6_off4 = arow7_off4; arow6_off5 = arow7_off5; arow6_off6 = arow7_off6; arow6_off7 = arow7_off7; arow6_off8 = arow7_off8; arow6_off9 = arow7_off9; arow6_off10 = arow7_off10; arow6_off11 = arow7_off11; arow6_4567 = arow7_4567; arow6_89ab = arow7_89ab; arow6_cdef = arow7_cdef;
            arow7_off0 = arow8_off0; arow7_off1 = arow8_off1; arow7_off2 = arow8_off2; arow7_off3 = arow8_off3; arow7_off4 = arow8_off4; arow7_off5 = arow8_off5; arow7_off6 = arow8_off6; arow7_off7 = arow8_off7; arow7_off8 = arow8_off8; arow7_off9 = arow8_off9; arow7_off10 = arow8_off10; arow7_off11 = arow8_off11; arow7_4567 = arow8_4567; arow7_89ab = arow8_89ab; arow7_cdef = arow8_cdef;
            
            // Write the computed row into c.
            *((vector float *)(c + n*CPITCH + m)) = spu_add(crow0123_addend, tmp0123_ends);;
            *((vector float *)(c + n*CPITCH + m+4)) = spu_add(crow4567_addend, tmp4567_ends);;
        }
    }
}

