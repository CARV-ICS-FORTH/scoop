//==============================================================================================
//
//  Innovative Computing Laboratory - Computer Science Department - University of Tennessee
//  Written by Jakub Kurzak
//
//==============================================================================================

#include <spu_intrinsics.h>

#include "spu_spotrf_tile.h"

//----------------------------------------------------------------------------------------------

#define  BLK 64
#define VBLK 16

//----------------------------------------------------------------------------------------------

void spu_spotrf_tile(float *A)
{
    vector float *Ap  = (vector float*)A;
    vector float *Bp  = (vector float*)A;
    vector float *Cp  = (vector float*)A;
    vector float *Tp  = (vector float*)A;

    float *T = A;

    vector float t0_0, t0_1, t0_2, t0_3;
    vector float t1_0, t1_1, t1_2, t1_3;
    vector float t2_0, t2_1, t2_2, t2_3;
    vector float t3_0, t3_1, t3_2, t3_3;

    vector float a0_0, a0_1, a0_2, a0_3;
    vector float a1_0, a1_1, a1_2, a1_3;
    vector float a2_0, a2_1, a2_2, a2_3;
    vector float a3_0, a3_1, a3_2, a3_3;

    vector float c0, c1, c2, c3;
    vector float in, y0, out;

    vector float zero = (vector float) {0.0, 0.0, 0.0, 0.0};

    vector unsigned char shufflehi = (vector unsigned char) {
        0x00, 0x01, 0x02, 0x03, 0x10, 0x11, 0x12, 0x13,
        0x04, 0x05, 0x06, 0x07, 0x14, 0x15, 0x16, 0x17};

    vector unsigned char shufflelo = (vector unsigned char) {
        0x08, 0x09, 0x0A, 0x0B, 0x18, 0x19, 0x1A, 0x1B,
        0x0C, 0x0D, 0x0E, 0x0F, 0x1C, 0x1D, 0x1E, 0x1F};

    vector float aibj;
    vector float ckdl;
    vector float emfn;
    vector float gohp;

    int i, j, k;

    //----------------------------------------------------------

    #define spotrf_sqrt(Ajj)\
    \
        in = spu_promote(Ajj, 0);\
        y0 = spu_rsqrte(in);\
        out = spu_mul(spu_nmsub(in, spu_mul(y0, y0), (vec_float4)(spu_splats((int)0x40400001))),\
              spu_mul(y0, spu_mul(in, spu_splats(0.5f))));\
        out = spu_andc(out, (vec_float4)spu_cmpeq(in, spu_splats(0.0f)));\
        Ajj = spu_extract(out, 0);\

    //----------------------------------------------------------

    #define spotrf_spotf2\
    \
        spotrf_sqrt(T[0]);\
        T[1*BLK] /= T[0];\
        T[2*BLK] /= T[0];\
        T[3*BLK] /= T[0];\
        \
        T[1*BLK+1] -= T[1*BLK+0] * T[1*BLK+0];\
        spotrf_sqrt(T[1*BLK+1]);\
        T[2*BLK+1] -= T[2*BLK+0] * T[1*BLK+0];\
        T[2*BLK+1] /= T[1*BLK+1];\
        T[3*BLK+1] -= T[3*BLK+0] * T[1*BLK+0];\
        T[3*BLK+1] /= T[1*BLK+1];\
        \
        T[2*BLK+2] -= T[2*BLK+0] * T[2*BLK+0];\
        T[2*BLK+2] -= T[2*BLK+1] * T[2*BLK+1];\
        spotrf_sqrt(T[2*BLK+2]);\
        T[3*BLK+2] -= T[3*BLK+0] * T[2*BLK+0];\
        T[3*BLK+2] -= T[3*BLK+1] * T[2*BLK+1];\
        T[3*BLK+2] /= T[2*BLK+2];\
        \
        T[3*BLK+3] -= T[3*BLK+0] * T[3*BLK+0];\
        T[3*BLK+3] -= T[3*BLK+1] * T[3*BLK+1];\
        T[3*BLK+3] -= T[3*BLK+2] * T[3*BLK+2];\
        spotrf_sqrt(T[3*BLK+3]);\

    //----------------------------------------------------------

    #define spotrf_load_T\
    \
        t0_0 = spu_splats(1.0f/T[0*BLK+0]);\
        t1_1 = spu_splats(1.0f/T[1*BLK+1]);\
        t2_2 = spu_splats(1.0f/T[2*BLK+2]);\
        t3_3 = spu_splats(1.0f/T[3*BLK+3]);\
        \
        t1_0 = spu_splats(T[1*BLK+0]);\
        t2_0 = spu_splats(T[2*BLK+0]);\
        t2_1 = spu_splats(T[2*BLK+1]);\
        t3_0 = spu_splats(T[3*BLK+0]);\
        t3_1 = spu_splats(T[3*BLK+1]);\
        t3_2 = spu_splats(T[3*BLK+2]);\

    //----------------------------------------------------------

    #define spotrf_strsm_4x4\
    \
        c0 = Cp[0*VBLK];\
        c1 = Cp[1*VBLK];\
        c2 = Cp[2*VBLK];\
        c3 = Cp[3*VBLK];\
        shuffle_4x1(c0, c1, c2, c3);\
        \
        c0 = spu_mul(c0, t0_0);\
        c1 = spu_nmsub(c0, t1_0, c1);\
        c1 = spu_mul(c1, t1_1);\
        c2 = spu_nmsub(c0, t2_0, c2);\
        c2 = spu_nmsub(c1, t2_1, c2);\
        c2 = spu_mul(c2, t2_2);\
        c3 = spu_nmsub(c0, t3_0, c3);\
        c3 = spu_nmsub(c1, t3_1, c3);\
        c3 = spu_nmsub(c2, t3_2, c3);\
        c3 = spu_mul(c3, t3_3);\
        \
        shuffle_4x1(c0, c1, c2, c3);\
        Cp[0*VBLK] = c0;\
        Cp[1*VBLK] = c1;\
        Cp[2*VBLK] = c2;\
        Cp[3*VBLK] = c3;\

    //----------------------------------------------------------

    #define shuffle_4x1(abcd, efgh, ijkl, mnop)\
    \
        aibj = spu_shuffle(abcd, ijkl, shufflehi);\
        ckdl = spu_shuffle(abcd, ijkl, shufflelo);\
        emfn = spu_shuffle(efgh, mnop, shufflehi);\
        gohp = spu_shuffle(efgh, mnop, shufflelo);\
        \
        abcd = spu_shuffle(aibj, emfn, shufflehi);\
        efgh = spu_shuffle(aibj, emfn, shufflelo);\
        ijkl = spu_shuffle(ckdl, gohp, shufflehi);\
        mnop = spu_shuffle(ckdl, gohp, shufflelo);\

    //----------------------------------------------------------

    #define spotrf_gemm_trmm_store(a, Ap)\
    \
        shuffle_4x1(a##0_0, a##0_1, a##0_2, a##0_3);\
        shuffle_4x1(a##1_0, a##1_1, a##1_2, a##1_3);\
        shuffle_4x1(a##2_0, a##2_1, a##2_2, a##2_3);\
        shuffle_4x1(a##3_0, a##3_1, a##3_2, a##3_3);\
        \
        a##0_0 = spu_add(a##0_0, a##0_1);\
        a##0_0 = spu_add(a##0_0, a##0_2);\
        a##0_0 = spu_add(a##0_0, a##0_3);\
        \
        a##1_0 = spu_add(a##1_0, a##1_1);\
        a##1_0 = spu_add(a##1_0, a##1_2);\
        a##1_0 = spu_add(a##1_0, a##1_3);\
        \
        a##2_0 = spu_add(a##2_0, a##2_1);\
        a##2_0 = spu_add(a##2_0, a##2_2);\
        a##2_0 = spu_add(a##2_0, a##2_3);\
        \
        a##3_0 = spu_add(a##3_0, a##3_1);\
        a##3_0 = spu_add(a##3_0, a##3_2);\
        a##3_0 = spu_add(a##3_0, a##3_3);\
        \
        shuffle_4x1(a##0_0, a##1_0, a##2_0, a##3_0);\
        \
        Ap[0*VBLK] = spu_sub(Ap[0*VBLK], a##0_0);\
        Ap[1*VBLK] = spu_sub(Ap[1*VBLK], a##1_0);\
        Ap[2*VBLK] = spu_sub(Ap[2*VBLK], a##2_0);\
        Ap[3*VBLK] = spu_sub(Ap[3*VBLK], a##3_0);\

    //----------------------------------------------------------

    #define spotrf_syrk_init\
    \
        a0_0 = spu_mul(Ap[0*VBLK], Ap[0*VBLK]);\
        a0_1 = spu_mul(Ap[1*VBLK], Ap[0*VBLK]);\
        a0_2 = spu_mul(Ap[2*VBLK], Ap[0*VBLK]);\
        a0_3 = spu_mul(Ap[3*VBLK], Ap[0*VBLK]);\
        \
        a1_0 = zero;\
        a1_1 = spu_mul(Ap[1*VBLK], Ap[1*VBLK]);\
        a1_2 = spu_mul(Ap[2*VBLK], Ap[1*VBLK]);\
        a1_3 = spu_mul(Ap[3*VBLK], Ap[1*VBLK]);\
        \
        a2_0 = zero;\
        a2_1 = zero;\
        a2_2 = spu_mul(Ap[2*VBLK], Ap[2*VBLK]);\
        a2_3 = spu_mul(Ap[3*VBLK], Ap[2*VBLK]);\
        \
        a3_0 = zero;\
        a3_1 = zero;\
        a3_2 = zero;\
        a3_3 = spu_mul(Ap[3*VBLK], Ap[3*VBLK]);\

    //----------------------------------------------------------

    #define spotrf_syrk_continue\
    \
        a0_0 = spu_madd(Ap[0*VBLK], Ap[0*VBLK], a0_0);\
        a0_1 = spu_madd(Ap[1*VBLK], Ap[0*VBLK], a0_1);\
        a0_2 = spu_madd(Ap[2*VBLK], Ap[0*VBLK], a0_2);\
        a0_3 = spu_madd(Ap[3*VBLK], Ap[0*VBLK], a0_3);\
        \
        a1_0 = zero;\
        a1_1 = spu_madd(Ap[1*VBLK], Ap[1*VBLK], a1_1);\
        a1_2 = spu_madd(Ap[2*VBLK], Ap[1*VBLK], a1_2);\
        a1_3 = spu_madd(Ap[3*VBLK], Ap[1*VBLK], a1_3);\
        \
        a2_0 = zero;\
        a2_1 = zero;\
        a2_2 = spu_madd(Ap[2*VBLK], Ap[2*VBLK], a2_2);\
        a2_3 = spu_madd(Ap[3*VBLK], Ap[2*VBLK], a2_3);\
        \
        a3_0 = zero;\
        a3_1 = zero;\
        a3_2 = zero;\
        a3_3 = spu_madd(Ap[3*VBLK], Ap[3*VBLK], a3_3);\

    //----------------------------------------------------------

    #define spotrf_gemm_init\
    \
        a0_0 = spu_mul(Bp[0*VBLK], Ap[0*VBLK]);\
        a0_1 = spu_mul(Bp[1*VBLK], Ap[0*VBLK]);\
        a0_2 = spu_mul(Bp[2*VBLK], Ap[0*VBLK]);\
        a0_3 = spu_mul(Bp[3*VBLK], Ap[0*VBLK]);\
        \
        a1_0 = spu_mul(Bp[0*VBLK], Ap[1*VBLK]);\
        a1_1 = spu_mul(Bp[1*VBLK], Ap[1*VBLK]);\
        a1_2 = spu_mul(Bp[2*VBLK], Ap[1*VBLK]);\
        a1_3 = spu_mul(Bp[3*VBLK], Ap[1*VBLK]);\
        \
        a2_0 = spu_mul(Bp[0*VBLK], Ap[2*VBLK]);\
        a2_1 = spu_mul(Bp[1*VBLK], Ap[2*VBLK]);\
        a2_2 = spu_mul(Bp[2*VBLK], Ap[2*VBLK]);\
        a2_3 = spu_mul(Bp[3*VBLK], Ap[2*VBLK]);\
        \
        a3_0 = spu_mul(Bp[0*VBLK], Ap[3*VBLK]);\
        a3_1 = spu_mul(Bp[1*VBLK], Ap[3*VBLK]);\
        a3_2 = spu_mul(Bp[2*VBLK], Ap[3*VBLK]);\
        a3_3 = spu_mul(Bp[3*VBLK], Ap[3*VBLK]);\

    //----------------------------------------------------------

    #define spotrf_gemm_continue\
    \
        a0_0 = spu_madd(Bp[0*VBLK], Ap[0*VBLK], a0_0);\
        a0_1 = spu_madd(Bp[1*VBLK], Ap[0*VBLK], a0_1);\
        a0_2 = spu_madd(Bp[2*VBLK], Ap[0*VBLK], a0_2);\
        a0_3 = spu_madd(Bp[3*VBLK], Ap[0*VBLK], a0_3);\
        \
        a1_0 = spu_madd(Bp[0*VBLK], Ap[1*VBLK], a1_0);\
        a1_1 = spu_madd(Bp[1*VBLK], Ap[1*VBLK], a1_1);\
        a1_2 = spu_madd(Bp[2*VBLK], Ap[1*VBLK], a1_2);\
        a1_3 = spu_madd(Bp[3*VBLK], Ap[1*VBLK], a1_3);\
        \
        a2_0 = spu_madd(Bp[0*VBLK], Ap[2*VBLK], a2_0);\
        a2_1 = spu_madd(Bp[1*VBLK], Ap[2*VBLK], a2_1);\
        a2_2 = spu_madd(Bp[2*VBLK], Ap[2*VBLK], a2_2);\
        a2_3 = spu_madd(Bp[3*VBLK], Ap[2*VBLK], a2_3);\
        \
        a3_0 = spu_madd(Bp[0*VBLK], Ap[3*VBLK], a3_0);\
        a3_1 = spu_madd(Bp[1*VBLK], Ap[3*VBLK], a3_1);\
        a3_2 = spu_madd(Bp[2*VBLK], Ap[3*VBLK], a3_2);\
        a3_3 = spu_madd(Bp[3*VBLK], Ap[3*VBLK], a3_3);\

    //----------------------------------------------------------

    // POTF2
    T = &A[0];
    spotrf_spotf2;

    // TRSM
    Cp = Ap+BLK;        
    spotrf_load_T;
    for (j = 1; j < VBLK; j++)
    {
        spotrf_strsm_4x4;
        Cp += BLK;
    }

    for (i = 1; i <= 15; i++)
    {
        // SYRK
        Ap += BLK;
        Tp  = Ap+i;
        spotrf_syrk_init;
        for (k = 0; k < i-1; k++)
        {
            Ap++;
            spotrf_syrk_continue;
        }
        spotrf_gemm_trmm_store(a, Tp);
        Ap -= (i-1);

        // POTF2
        T = &A[i*(4*BLK+4)];
        spotrf_spotf2;

        // GEMM
        Bp = Ap+BLK;
        Cp = Tp+BLK;
        for (j = 1; j < VBLK-i; j++)
        {
            spotrf_gemm_init;
            for (k = 0; k < i-1; k++)
            {
                Ap++;
                Bp++;
                spotrf_gemm_continue;
            }
            spotrf_gemm_trmm_store(a, Cp);
            Ap -= (i-1);
            Bp -= (i-1);
            Bp += BLK;
            Cp += BLK;
        }

        // TRSM
        Cp = Ap+BLK+i;        
        spotrf_load_T;
        for (j = 1; j < VBLK-i; j++)
        {
            spotrf_strsm_4x4;
            Cp += BLK;
        }
    }
}

//----------------------------------------------------------------------------------------------
