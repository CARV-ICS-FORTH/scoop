//==============================================================================================
//
//  Innovative Computing Laboratory - Computer Science Department - University of Tennessee
//  Written by Jakub Kurzak
//
//==============================================================================================

// #include <spu_intrinsics.h>

#include "spu_sgemm_tile.h"

//----------------------------------------------------------------------------------------------

#define  BLK 64
#define VBLK 16

//----------------------------------------------------------------------------------------------

// #pragma css task input(A[64][64], B[64][64]) inout(C[64][64])
void spu_sgemm_tile(float *A, float *B, float *C)
{
    vector float *Abase = (vector float*)A;
    vector float *Bbase = (vector float*)B;
    vector float *Cbase = (vector float*)C;

    vector float *Ap = (vector float*)A;
    vector float *Bp = (vector float*)B;
    vector float *Cp = (vector float*)C;

    vector float *Xp = (vector float*)A;
    vector float *Yp = (vector float*)B;
    vector float *Zp = (vector float*)C;

    vector float  a0,  a1,  a2,  a3;
    vector float  b0,  b1,  b2,  b3;
    vector float aa0, aa1, aa2, aa3;
    vector float bb0, bb1, bb2, bb3;
    vector float  c0,  c1,  c2,  c3;

    vector float c0_0, c0_1, c0_2, c0_3;
    vector float c1_0, c1_1, c1_2, c1_3;
    vector float c2_0, c2_1, c2_2, c2_3;
    vector float c3_0, c3_1, c3_2, c3_3;

    vector float  x0,  x1,  x2,  x3;
    vector float  y0,  y1,  y2,  y3;
    vector float xx0, xx1, xx2, xx3;
    vector float yy0, yy1, yy2, yy3;
    vector float  z0,  z1,  z2,  z3;

    vector float z0_0, z0_1, z0_2, z0_3;
    vector float z1_0, z1_1, z1_2, z1_3;
    vector float z2_0, z2_1, z2_2, z2_3;
    vector float z3_0, z3_1, z3_2, z3_3;

    vector unsigned char shufflehi = (vector unsigned char){
        0x00, 0x01, 0x02, 0x03, 0x10, 0x11, 0x12, 0x13,
        0x04, 0x05, 0x06, 0x07, 0x14, 0x15, 0x16, 0x17};

    vector unsigned char shufflelo = (vector unsigned char){
        0x08, 0x09, 0x0A, 0x0B, 0x18, 0x19, 0x1A, 0x1B,
        0x0C, 0x0D, 0x0E, 0x0F, 0x1C, 0x1D, 0x1E, 0x1F};

    vector float aibj;
    vector float ckdl;
    vector float emfn;
    vector float gohp;

    int tile_front;
    int tile_back;
    int i;

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


    #define load_ab_4x4(OFFSET, a, b, Ap, Bp)\
    \
        a##0 = Ap[OFFSET + 0*VBLK];     b##0 = Bp[OFFSET + 0*VBLK];\
        a##1 = Ap[OFFSET + 1*VBLK];     b##1 = Bp[OFFSET + 1*VBLK];\
        a##2 = Ap[OFFSET + 2*VBLK];     b##2 = Bp[OFFSET + 2*VBLK];\
        a##3 = Ap[OFFSET + 3*VBLK];     b##3 = Bp[OFFSET + 3*VBLK];\


    #define mul_ab_4x4(c, a, b)\
    \
        c##0_0 = spu_mul(b##0, a##0);   c##0_1 = spu_mul(b##0, a##1);\
        c##0_2 = spu_mul(b##0, a##2);   c##0_3 = spu_mul(b##0, a##3);\
        c##1_0 = spu_mul(b##1, a##0);   c##1_1 = spu_mul(b##1, a##1);\
        c##1_2 = spu_mul(b##1, a##2);   c##1_3 = spu_mul(b##1, a##3);\
        c##2_0 = spu_mul(b##2, a##0);   c##2_1 = spu_mul(b##2, a##1);\
        c##2_2 = spu_mul(b##2, a##2);   c##2_3 = spu_mul(b##2, a##3);\
        c##3_0 = spu_mul(b##3, a##0);   c##3_1 = spu_mul(b##3, a##1);\
        c##3_2 = spu_mul(b##3, a##2);   c##3_3 = spu_mul(b##3, a##3);\


    #define madd_ab_4x4(c, a, b)\
    \
        c##0_0 = spu_madd(b##0, a##0, c##0_0);  c##0_1 = spu_madd(b##0, a##1, c##0_1);\
        c##0_2 = spu_madd(b##0, a##2, c##0_2);  c##0_3 = spu_madd(b##0, a##3, c##0_3);\
        c##1_0 = spu_madd(b##1, a##0, c##1_0);  c##1_1 = spu_madd(b##1, a##1, c##1_1);\
        c##1_2 = spu_madd(b##1, a##2, c##1_2);  c##1_3 = spu_madd(b##1, a##3, c##1_3);\
        c##2_0 = spu_madd(b##2, a##0, c##2_0);  c##2_1 = spu_madd(b##2, a##1, c##2_1);\
        c##2_2 = spu_madd(b##2, a##2, c##2_2);  c##2_3 = spu_madd(b##2, a##3, c##2_3);\
        c##3_0 = spu_madd(b##3, a##0, c##3_0);  c##3_1 = spu_madd(b##3, a##1, c##3_1);\
        c##3_2 = spu_madd(b##3, a##2, c##3_2);  c##3_3 = spu_madd(b##3, a##3, c##3_3);\

    //----------------------------------------------------------

    #define sgemm_4x4xNB_first()\
    {\
        load_ab_4x4( 0,  a,  b, Ap, Bp);\
        load_ab_4x4( 1, aa, bb, Ap, Bp);     mul_ab_4x4(c,  a,  b);\
        load_ab_4x4( 2,  a,  b, Ap, Bp);    madd_ab_4x4(c, aa, bb);\
        load_ab_4x4( 3, aa, bb, Ap, Bp);    madd_ab_4x4(c,  a,  b);\
        load_ab_4x4( 4,  a,  b, Ap, Bp);    madd_ab_4x4(c, aa, bb);\
        load_ab_4x4( 5, aa, bb, Ap, Bp);    madd_ab_4x4(c,  a,  b);\
        load_ab_4x4( 6,  a,  b, Ap, Bp);    madd_ab_4x4(c, aa, bb);\
        load_ab_4x4( 7, aa, bb, Ap, Bp);    madd_ab_4x4(c,  a,  b);\
        load_ab_4x4( 8,  a,  b, Ap, Bp);    madd_ab_4x4(c, aa, bb);\
        load_ab_4x4( 9, aa, bb, Ap, Bp);    madd_ab_4x4(c,  a,  b);\
        load_ab_4x4(10,  a,  b, Ap, Bp);    madd_ab_4x4(c, aa, bb);\
        load_ab_4x4(11, aa, bb, Ap, Bp);    madd_ab_4x4(c,  a,  b);\
        load_ab_4x4(12,  a,  b, Ap, Bp);    madd_ab_4x4(c, aa, bb);\
        load_ab_4x4(13, aa, bb, Ap, Bp);    madd_ab_4x4(c,  a,  b);\
        load_ab_4x4(14,  a,  b, Ap, Bp);    madd_ab_4x4(c, aa, bb);\
        \
        tile_back += 2;\
        Xp = Abase + (tile_back >> 4) * BLK;\
        Yp = Bbase + (tile_back & 0x0F) * BLK;\
        Zp = Cbase + (tile_back & 0x0F) + (tile_back >> 4) * BLK;\
        \
        load_ab_4x4(15, aa, bb, Ap, Bp);    madd_ab_4x4(c,  a,  b);\
        load_ab_4x4( 0,  x,  y, Xp, Yp);    madd_ab_4x4(c, aa, bb);\
    }

    //----------------------------------------------------------

    #define sgemm_4x4xNB_odd()\
    \
        shuffle_4x1(c0_0, c0_1, c0_2, c0_3);\
        shuffle_4x1(c1_0, c1_1, c1_2, c1_3);\
        shuffle_4x1(c2_0, c2_1, c2_2, c2_3);\
        shuffle_4x1(c3_0, c3_1, c3_2, c3_3);\
        \
        c0_0 = spu_add(c0_0, c0_1); c0_0 = spu_add(c0_0, c0_2); c0_0 = spu_add(c0_0, c0_3);\
        c1_0 = spu_add(c1_0, c1_1); c1_0 = spu_add(c1_0, c1_2); c1_0 = spu_add(c1_0, c1_3);\
        c2_0 = spu_add(c2_0, c2_1); c2_0 = spu_add(c2_0, c2_2); c2_0 = spu_add(c2_0, c2_3);\
        c3_0 = spu_add(c3_0, c3_1); c3_0 = spu_add(c3_0, c3_2); c3_0 = spu_add(c3_0, c3_3);\
        \
        shuffle_4x1(c0_0, c1_0, c2_0, c3_0);\
        \
        c0 = Cp[0*VBLK];    c0 = spu_sub(c0, c0_0);\
        c1 = Cp[1*VBLK];    c1 = spu_sub(c1, c1_0);\
        c2 = Cp[2*VBLK];    c2 = spu_sub(c2, c2_0);\
        c3 = Cp[3*VBLK];    c3 = spu_sub(c3, c3_0);\
        \
        load_ab_4x4( 1, xx, yy, Xp, Yp);     mul_ab_4x4(z,  x,  y);\
        load_ab_4x4( 2,  x,  y, Xp, Yp);    madd_ab_4x4(z, xx, yy);\
        load_ab_4x4( 3, xx, yy, Xp, Yp);    madd_ab_4x4(z,  x,  y);\
        load_ab_4x4( 4,  x,  y, Xp, Yp);    madd_ab_4x4(z, xx, yy);\
        load_ab_4x4( 5, xx, yy, Xp, Yp);    madd_ab_4x4(z,  x,  y);\
        load_ab_4x4( 6,  x,  y, Xp, Yp);    madd_ab_4x4(z, xx, yy);\
        load_ab_4x4( 7, xx, yy, Xp, Yp);    madd_ab_4x4(z,  x,  y);\
        load_ab_4x4( 8,  x,  y, Xp, Yp);    madd_ab_4x4(z, xx, yy);\
        load_ab_4x4( 9, xx, yy, Xp, Yp);    madd_ab_4x4(z,  x,  y);\
        load_ab_4x4(10,  x,  y, Xp, Yp);    madd_ab_4x4(z, xx, yy);\
        load_ab_4x4(11, xx, yy, Xp, Yp);    madd_ab_4x4(z,  x,  y);\
        load_ab_4x4(12,  x,  y, Xp, Yp);    madd_ab_4x4(z, xx, yy);\
        \
        Cp[0 + 0*VBLK] = c0;\
        Cp[0 + 1*VBLK] = c1;\
        Cp[0 + 2*VBLK] = c2;\
        Cp[0 + 3*VBLK] = c3;\
        \
        load_ab_4x4(13, xx, yy, Xp, Yp);    madd_ab_4x4(z,  x,  y);\
        load_ab_4x4(14,  x,  y, Xp, Yp);    madd_ab_4x4(z, xx, yy);\
        \
        tile_front += 2;\
        Ap = Abase + (tile_front >> 4) * BLK;\
        Bp = Bbase + (tile_front & 0x0F) * BLK;\
        Cp = Cbase + (tile_front & 0x0F) + (tile_front >> 4) * BLK;\
        \
        load_ab_4x4(15, xx, yy, Xp, Yp);    madd_ab_4x4(z,  x,  y);\
        load_ab_4x4( 0,  a,  b, Ap, Bp);    madd_ab_4x4(z, xx, yy);\

    //----------------------------------------------------------

    #define sgemm_4x4xNB_even()\
    \
        shuffle_4x1(z0_0, z0_1, z0_2, z0_3);\
        shuffle_4x1(z1_0, z1_1, z1_2, z1_3);\
        shuffle_4x1(z2_0, z2_1, z2_2, z2_3);\
        shuffle_4x1(z3_0, z3_1, z3_2, z3_3);\
        \
        z0_0 = spu_add(z0_0, z0_1); z0_0 = spu_add(z0_0, z0_2); z0_0 = spu_add(z0_0, z0_3);\
        z1_0 = spu_add(z1_0, z1_1); z1_0 = spu_add(z1_0, z1_2); z1_0 = spu_add(z1_0, z1_3);\
        z2_0 = spu_add(z2_0, z2_1); z2_0 = spu_add(z2_0, z2_2); z2_0 = spu_add(z2_0, z2_3);\
        z3_0 = spu_add(z3_0, z3_1); z3_0 = spu_add(z3_0, z3_2); z3_0 = spu_add(z3_0, z3_3);\
        \
        shuffle_4x1(z0_0, z1_0, z2_0, z3_0);\
        \
        z0 = Zp[0*VBLK];    z0 = spu_sub(z0, z0_0);\
        z1 = Zp[1*VBLK];    z1 = spu_sub(z1, z1_0);\
        z2 = Zp[2*VBLK];    z2 = spu_sub(z2, z2_0);\
        z3 = Zp[3*VBLK];    z3 = spu_sub(z3, z3_0);\
        \
        load_ab_4x4( 1, aa, bb, Ap, Bp);     mul_ab_4x4(c,  a,  b);\
        load_ab_4x4( 2,  a,  b, Ap, Bp);    madd_ab_4x4(c, aa, bb);\
        load_ab_4x4( 3, aa, bb, Ap, Bp);    madd_ab_4x4(c,  a,  b);\
        load_ab_4x4( 4,  a,  b, Ap, Bp);    madd_ab_4x4(c, aa, bb);\
        load_ab_4x4( 5, aa, bb, Ap, Bp);    madd_ab_4x4(c,  a,  b);\
        load_ab_4x4( 6,  a,  b, Ap, Bp);    madd_ab_4x4(c, aa, bb);\
        load_ab_4x4( 7, aa, bb, Ap, Bp);    madd_ab_4x4(c,  a,  b);\
        load_ab_4x4( 8,  a,  b, Ap, Bp);    madd_ab_4x4(c, aa, bb);\
        load_ab_4x4( 9, aa, bb, Ap, Bp);    madd_ab_4x4(c,  a,  b);\
        load_ab_4x4(10,  a,  b, Ap, Bp);    madd_ab_4x4(c, aa, bb);\
        load_ab_4x4(11, aa, bb, Ap, Bp);    madd_ab_4x4(c,  a,  b);\
        load_ab_4x4(12,  a,  b, Ap, Bp);    madd_ab_4x4(c, aa, bb);\
        \
        Zp[0 + 0*VBLK] = z0;\
        Zp[0 + 1*VBLK] = z1;\
        Zp[0 + 2*VBLK] = z2;\
        Zp[0 + 3*VBLK] = z3;\
        \
        load_ab_4x4(13, aa, bb, Ap, Bp);    madd_ab_4x4(c,  a,  b);\
        load_ab_4x4(14,  a,  b, Ap, Bp);    madd_ab_4x4(c, aa, bb);\
        \
        tile_back += 2;\
        Xp = Abase + (tile_back >> 4) * BLK;\
        Yp = Bbase + (tile_back & 0x0F) * BLK;\
        Zp = Cbase + (tile_back & 0x0F) + (tile_back >> 4) * BLK;\
        \
        load_ab_4x4(15, aa, bb, Ap, Bp);    madd_ab_4x4(c,  a,  b);\
        load_ab_4x4( 0,  x,  y, Xp, Yp);    madd_ab_4x4(c, aa, bb);\

    //----------------------------------------------------------

    #define sgemm_4x4xNB_last()\
    \
        shuffle_4x1(c0_0, c0_1, c0_2, c0_3);\
        shuffle_4x1(c1_0, c1_1, c1_2, c1_3);\
        shuffle_4x1(c2_0, c2_1, c2_2, c2_3);\
        shuffle_4x1(c3_0, c3_1, c3_2, c3_3);\
        \
        c0_0 = spu_add(c0_0, c0_1); c0_0 = spu_add(c0_0, c0_2); c0_0 = spu_add(c0_0, c0_3);\
        c1_0 = spu_add(c1_0, c1_1); c1_0 = spu_add(c1_0, c1_2); c1_0 = spu_add(c1_0, c1_3);\
        c2_0 = spu_add(c2_0, c2_1); c2_0 = spu_add(c2_0, c2_2); c2_0 = spu_add(c2_0, c2_3);\
        c3_0 = spu_add(c3_0, c3_1); c3_0 = spu_add(c3_0, c3_2); c3_0 = spu_add(c3_0, c3_3);\
        \
        shuffle_4x1(c0_0, c1_0, c2_0, c3_0);\
        \
        c0 = Cp[0*VBLK];    c0 = spu_sub(c0, c0_0);\
        c1 = Cp[1*VBLK];    c1 = spu_sub(c1, c1_0);\
        c2 = Cp[2*VBLK];    c2 = spu_sub(c2, c2_0);\
        c3 = Cp[3*VBLK];    c3 = spu_sub(c3, c3_0);\
        \
        load_ab_4x4( 1, xx, yy, Xp, Yp);     mul_ab_4x4(z,  x,  y);\
        load_ab_4x4( 2,  x,  y, Xp, Yp);    madd_ab_4x4(z, xx, yy);\
        load_ab_4x4( 3, xx, yy, Xp, Yp);    madd_ab_4x4(z,  x,  y);\
        load_ab_4x4( 4,  x,  y, Xp, Yp);    madd_ab_4x4(z, xx, yy);\
        load_ab_4x4( 5, xx, yy, Xp, Yp);    madd_ab_4x4(z,  x,  y);\
        load_ab_4x4( 6,  x,  y, Xp, Yp);    madd_ab_4x4(z, xx, yy);\
        load_ab_4x4( 7, xx, yy, Xp, Yp);    madd_ab_4x4(z,  x,  y);\
        load_ab_4x4( 8,  x,  y, Xp, Yp);    madd_ab_4x4(z, xx, yy);\
        load_ab_4x4( 9, xx, yy, Xp, Yp);    madd_ab_4x4(z,  x,  y);\
        load_ab_4x4(10,  x,  y, Xp, Yp);    madd_ab_4x4(z, xx, yy);\
        load_ab_4x4(11, xx, yy, Xp, Yp);    madd_ab_4x4(z,  x,  y);\
        load_ab_4x4(12,  x,  y, Xp, Yp);    madd_ab_4x4(z, xx, yy);\
        \
        Cp[0 + 0*VBLK] = c0;\
        Cp[0 + 1*VBLK] = c1;\
        Cp[0 + 2*VBLK] = c2;\
        Cp[0 + 3*VBLK] = c3;\
        \
        load_ab_4x4(13, xx, yy, Xp, Yp);    madd_ab_4x4(z,  x,  y);\
        load_ab_4x4(14,  x,  y, Xp, Yp);    madd_ab_4x4(z, xx, yy);\
        \
        tile_front += 2;\
        Ap = Abase + (tile_front >> 4) * BLK;\
        Bp = Bbase + (tile_front & 0x0F) * BLK;\
        Cp = Cbase + (tile_front & 0x0F) + (tile_front >> 4) * BLK;\
        \
        load_ab_4x4(15, xx, yy, Xp, Yp);    madd_ab_4x4(z,  x,  y);\
                                            madd_ab_4x4(z, xx, yy);\
        \
        shuffle_4x1(z0_0, z0_1, z0_2, z0_3);\
        shuffle_4x1(z1_0, z1_1, z1_2, z1_3);\
        shuffle_4x1(z2_0, z2_1, z2_2, z2_3);\
        shuffle_4x1(z3_0, z3_1, z3_2, z3_3);\
        \
        z0_0 = spu_add(z0_0, z0_1); z0_0 = spu_add(z0_0, z0_2); z0_0 = spu_add(z0_0, z0_3);\
        z1_0 = spu_add(z1_0, z1_1); z1_0 = spu_add(z1_0, z1_2); z1_0 = spu_add(z1_0, z1_3);\
        z2_0 = spu_add(z2_0, z2_1); z2_0 = spu_add(z2_0, z2_2); z2_0 = spu_add(z2_0, z2_3);\
        z3_0 = spu_add(z3_0, z3_1); z3_0 = spu_add(z3_0, z3_2); z3_0 = spu_add(z3_0, z3_3);\
        \
        shuffle_4x1(z0_0, z1_0, z2_0, z3_0);\
        \
        z0 = Zp[0*VBLK];    z0 = spu_sub(z0, z0_0);\
        z1 = Zp[1*VBLK];    z1 = spu_sub(z1, z1_0);\
        z2 = Zp[2*VBLK];    z2 = spu_sub(z2, z2_0);\
        z3 = Zp[3*VBLK];    z3 = spu_sub(z3, z3_0);\
        \
        Zp[0 + 0*VBLK] = z0;\
        Zp[0 + 1*VBLK] = z1;\
        Zp[0 + 2*VBLK] = z2;\
        Zp[0 + 3*VBLK] = z3;\

    //----------------------------------------------------------

    tile_front = 0;
    tile_back = -1;

    sgemm_4x4xNB_first();
    for (i = 1; i <= 127; i++)
    {
        sgemm_4x4xNB_odd();
        sgemm_4x4xNB_even();
    } 
    sgemm_4x4xNB_last();
}

//----------------------------------------------------------------------------------------------
