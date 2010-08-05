//==============================================================================================
//
//  Innovative Computing Laboratory - Computer Science Department - University of Tennessee
//  Written by Jakub Kurzak
//
//==============================================================================================

#include <spu_intrinsics.h>

#include "spu_strsm_tile.h"

//----------------------------------------------------------------------------------------------

#define  BLK 64
#define VBLK 16
// #define vector __attribute__((__spu_vector__))

//----------------------------------------------------------------------------------------------

// #pragma css task input(T[64][64]) inout(B[64][64])
void spu_strsm_tile(float *T, float *B)
{
    vector float *Bp  = (vector float*)B;
    vector float *BIp = (vector float*)B;
    vector float *BJp = (vector float*)B;
    
    float *Tp = T;

    vector float bi0, bi1, bi2, bi3;

    #define bj(N)\
    \
        vector float bj##N##_0;\
        vector float bj##N##_1;\
        vector float bj##N##_2;\
        vector float bj##N##_3;\

    bj(0)  bj(1)  bj(2)  bj(3)
    bj(4)  bj(5)  bj(6)  bj(7)
    bj(8)  bj(9)  bj(10) bj(11)
    bj(12) bj(13) bj(14) bj(15)

    vector float t0_0, t0_1, t0_2, t0_3;
    vector float t1_0, t1_1, t1_2, t1_3;
    vector float t2_0, t2_1, t2_2, t2_3;
    vector float t3_0, t3_1, t3_2, t3_3;

    vector unsigned char shufflehi = (vector unsigned char)
        {0x00, 0x01, 0x02, 0x03, 0x10, 0x11, 0x12, 0x13,
        0x04, 0x05, 0x06, 0x07, 0x14, 0x15, 0x16, 0x17};

    vector unsigned char shufflelo = (vector unsigned char)
        {0x08, 0x09, 0x0A, 0x0B, 0x18, 0x19, 0x1A, 0x1B,
        0x0C, 0x0D, 0x0E, 0x0F, 0x1C, 0x1D, 0x1E, 0x1F};

    vector float aibj;
    vector float ckdl;
    vector float emfn;
    vector float gohp;

    int i, j;

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

    #define trsm_shuffle_4x4(OFFX, OFFY)\
        shuffle_4x1(\
            BJp[OFFX+OFFY+0*VBLK],\
            BJp[OFFX+OFFY+1*VBLK],\
            BJp[OFFX+OFFY+2*VBLK],\
            BJp[OFFX+OFFY+3*VBLK]);\


    #define trsm_shuffle_4xNB(OFFY)\
        trsm_shuffle_4x4( 0, OFFY);\
        trsm_shuffle_4x4( 1, OFFY);\
        trsm_shuffle_4x4( 2, OFFY);\
        trsm_shuffle_4x4( 3, OFFY);\
        trsm_shuffle_4x4( 4, OFFY);\
        trsm_shuffle_4x4( 5, OFFY);\
        trsm_shuffle_4x4( 6, OFFY);\
        trsm_shuffle_4x4( 7, OFFY);\
        trsm_shuffle_4x4( 8, OFFY);\
        trsm_shuffle_4x4( 9, OFFY);\
        trsm_shuffle_4x4(10, OFFY);\
        trsm_shuffle_4x4(11, OFFY);\
        trsm_shuffle_4x4(12, OFFY);\
        trsm_shuffle_4x4(13, OFFY);\
        trsm_shuffle_4x4(14, OFFY);\
        trsm_shuffle_4x4(15, OFFY);\

    //----------------------------------------------------------

    #define trsm_4x4x4_load(OFFI, N)\
    \
        bj##N##_0 = BJp[OFFI+0*VBLK];\
        bj##N##_1 = BJp[OFFI+1*VBLK];\
        bj##N##_2 = BJp[OFFI+2*VBLK];\
        bj##N##_3 = BJp[OFFI+3*VBLK];\


    #define trsm_4x4xRHS_load(OFFJ) \
    \
        trsm_4x4x4_load(OFFJ+ 0*BLK, 0);\
        trsm_4x4x4_load(OFFJ+ 1*BLK, 1);\
        trsm_4x4x4_load(OFFJ+ 2*BLK, 2);\
        trsm_4x4x4_load(OFFJ+ 3*BLK, 3);\
        trsm_4x4x4_load(OFFJ+ 4*BLK, 4);\
        trsm_4x4x4_load(OFFJ+ 5*BLK, 5);\
        trsm_4x4x4_load(OFFJ+ 6*BLK, 6);\
        trsm_4x4x4_load(OFFJ+ 7*BLK, 7);\
        trsm_4x4x4_load(OFFJ+ 8*BLK, 8);\
        trsm_4x4x4_load(OFFJ+ 9*BLK, 9);\
        trsm_4x4x4_load(OFFJ+10*BLK,10);\
        trsm_4x4x4_load(OFFJ+11*BLK,11);\
        trsm_4x4x4_load(OFFJ+12*BLK,12);\
        trsm_4x4x4_load(OFFJ+13*BLK,13);\
        trsm_4x4x4_load(OFFJ+14*BLK,14);\
        trsm_4x4x4_load(OFFJ+15*BLK,15);\

    //----------------------------------------------------------

    #define trsm_4x4x4(OFFI, N)\
    \
        bi0 = BIp[OFFI+0*VBLK];\
        bi1 = BIp[OFFI+1*VBLK];\
        bi2 = BIp[OFFI+2*VBLK];\
        bi3 = BIp[OFFI+3*VBLK];\
        \
        bj##N##_0 = spu_nmsub(bi0, t0_0, bj##N##_0);\
        bj##N##_0 = spu_nmsub(bi1, t0_1, bj##N##_0);\
        bj##N##_0 = spu_nmsub(bi2, t0_2, bj##N##_0);\
        bj##N##_0 = spu_nmsub(bi3, t0_3, bj##N##_0);\
        \
        bj##N##_1 = spu_nmsub(bi0, t1_0, bj##N##_1);\
        bj##N##_1 = spu_nmsub(bi1, t1_1, bj##N##_1);\
        bj##N##_1 = spu_nmsub(bi2, t1_2, bj##N##_1);\
        bj##N##_1 = spu_nmsub(bi3, t1_3, bj##N##_1);\
        \
        bj##N##_2 = spu_nmsub(bi0, t2_0, bj##N##_2);\
        bj##N##_2 = spu_nmsub(bi1, t2_1, bj##N##_2);\
        bj##N##_2 = spu_nmsub(bi2, t2_2, bj##N##_2);\
        bj##N##_2 = spu_nmsub(bi3, t2_3, bj##N##_2);\
        \
        bj##N##_3 = spu_nmsub(bi0, t3_0, bj##N##_3);\
        bj##N##_3 = spu_nmsub(bi1, t3_1, bj##N##_3);\
        bj##N##_3 = spu_nmsub(bi2, t3_2, bj##N##_3);\
        bj##N##_3 = spu_nmsub(bi3, t3_3, bj##N##_3);\


    #define trsm_4x4xRHS(OFFJ) \
    \
        t0_0 = spu_splats(Tp[BLK*0+0]);\
        t0_1 = spu_splats(Tp[BLK*0+1]);\
        t0_2 = spu_splats(Tp[BLK*0+2]);\
        t0_3 = spu_splats(Tp[BLK*0+3]);\
        \
        t1_0 = spu_splats(Tp[BLK*1+0]);\
        t1_1 = spu_splats(Tp[BLK*1+1]);\
        t1_2 = spu_splats(Tp[BLK*1+2]);\
        t1_3 = spu_splats(Tp[BLK*1+3]);\
        \
        t2_0 = spu_splats(Tp[BLK*2+0]);\
        t2_1 = spu_splats(Tp[BLK*2+1]);\
        t2_2 = spu_splats(Tp[BLK*2+2]);\
        t2_3 = spu_splats(Tp[BLK*2+3]);\
        \
        t3_0 = spu_splats(Tp[BLK*3+0]);\
        t3_1 = spu_splats(Tp[BLK*3+1]);\
        t3_2 = spu_splats(Tp[BLK*3+2]);\
        t3_3 = spu_splats(Tp[BLK*3+3]);\
        \
        trsm_4x4x4(OFFJ+ 0*BLK, 0);\
        trsm_4x4x4(OFFJ+ 1*BLK, 1);\
        trsm_4x4x4(OFFJ+ 2*BLK, 2);\
        trsm_4x4x4(OFFJ+ 3*BLK, 3);\
        trsm_4x4x4(OFFJ+ 4*BLK, 4);\
        trsm_4x4x4(OFFJ+ 5*BLK, 5);\
        trsm_4x4x4(OFFJ+ 6*BLK, 6);\
        trsm_4x4x4(OFFJ+ 7*BLK, 7);\
        trsm_4x4x4(OFFJ+ 8*BLK, 8);\
        trsm_4x4x4(OFFJ+ 9*BLK, 9);\
        trsm_4x4x4(OFFJ+10*BLK,10);\
        trsm_4x4x4(OFFJ+11*BLK,11);\
        trsm_4x4x4(OFFJ+12*BLK,12);\
        trsm_4x4x4(OFFJ+13*BLK,13);\
        trsm_4x4x4(OFFJ+14*BLK,14);\
        trsm_4x4x4(OFFJ+15*BLK,15);\

    //----------------------------------------------------------

    #define trsm_4x4x4_(OFFI, N)\
    \
        bj##N##_0 = spu_mul(bj##N##_0, t0_0);\
        \
        bj##N##_1 = spu_nmsub(bj##N##_0, t1_0, bj##N##_1);\
        bj##N##_1 = spu_mul(bj##N##_1, t1_1);\
        \
        bj##N##_2 = spu_nmsub(bj##N##_0, t2_0, bj##N##_2);\
        bj##N##_2 = spu_nmsub(bj##N##_1, t2_1, bj##N##_2);\
        bj##N##_2 = spu_mul(bj##N##_2, t2_2);\
        \
        bj##N##_3 = spu_nmsub(bj##N##_0, t3_0, bj##N##_3);\
        bj##N##_3 = spu_nmsub(bj##N##_1, t3_1, bj##N##_3);\
        bj##N##_3 = spu_nmsub(bj##N##_2, t3_2, bj##N##_3);\
        bj##N##_3 = spu_mul(bj##N##_3, t3_3);\


    #define trsm_4x4xRHS_(OFFJ)\
    \
        t0_0 = spu_splats(1.0f/Tp[BLK*0+0]);\
        t1_1 = spu_splats(1.0f/Tp[BLK*1+1]);\
        t2_2 = spu_splats(1.0f/Tp[BLK*2+2]);\
        t3_3 = spu_splats(1.0f/Tp[BLK*3+3]);\
        \
        t1_0 = spu_splats(Tp[BLK*1+0]);\
        t2_0 = spu_splats(Tp[BLK*2+0]);\
        t2_1 = spu_splats(Tp[BLK*2+1]);\
        t3_0 = spu_splats(Tp[BLK*3+0]);\
        t3_1 = spu_splats(Tp[BLK*3+1]);\
        t3_2 = spu_splats(Tp[BLK*3+2]);\
        \
        trsm_4x4x4_(OFFJ+ 0*BLK, 0);\
        trsm_4x4x4_(OFFJ+ 1*BLK, 1);\
        trsm_4x4x4_(OFFJ+ 2*BLK, 2);\
        trsm_4x4x4_(OFFJ+ 3*BLK, 3);\
        trsm_4x4x4_(OFFJ+ 4*BLK, 4);\
        trsm_4x4x4_(OFFJ+ 5*BLK, 5);\
        trsm_4x4x4_(OFFJ+ 6*BLK, 6);\
        trsm_4x4x4_(OFFJ+ 7*BLK, 7);\
        trsm_4x4x4_(OFFJ+ 8*BLK, 8);\
        trsm_4x4x4_(OFFJ+ 9*BLK, 9);\
        trsm_4x4x4_(OFFJ+10*BLK,10);\
        trsm_4x4x4_(OFFJ+11*BLK,11);\
        trsm_4x4x4_(OFFJ+12*BLK,12);\
        trsm_4x4x4_(OFFJ+13*BLK,13);\
        trsm_4x4x4_(OFFJ+14*BLK,14);\
        trsm_4x4x4_(OFFJ+15*BLK,15);\

    //----------------------------------------------------------

    #define trsm_4x4x4_store(OFFI, N)\
    \
        BJp[OFFI+0*VBLK] = bj##N##_0;\
        BJp[OFFI+1*VBLK] = bj##N##_1;\
        BJp[OFFI+2*VBLK] = bj##N##_2;\
        BJp[OFFI+3*VBLK] = bj##N##_3;\


    #define trsm_4x4xRHS_store(OFFJ) \
    \
        trsm_4x4x4_store(OFFJ+ 0*BLK, 0);\
        trsm_4x4x4_store(OFFJ+ 1*BLK, 1);\
        trsm_4x4x4_store(OFFJ+ 2*BLK, 2);\
        trsm_4x4x4_store(OFFJ+ 3*BLK, 3);\
        trsm_4x4x4_store(OFFJ+ 4*BLK, 4);\
        trsm_4x4x4_store(OFFJ+ 5*BLK, 5);\
        trsm_4x4x4_store(OFFJ+ 6*BLK, 6);\
        trsm_4x4x4_store(OFFJ+ 7*BLK, 7);\
        trsm_4x4x4_store(OFFJ+ 8*BLK, 8);\
        trsm_4x4x4_store(OFFJ+ 9*BLK, 9);\
        trsm_4x4x4_store(OFFJ+10*BLK,10);\
        trsm_4x4x4_store(OFFJ+11*BLK,11);\
        trsm_4x4x4_store(OFFJ+12*BLK,12);\
        trsm_4x4x4_store(OFFJ+13*BLK,13);\
        trsm_4x4x4_store(OFFJ+14*BLK,14);\
        trsm_4x4x4_store(OFFJ+15*BLK,15);\

    //----------------------------------------------------------

    for (i = 0; i < VBLK; i++)
    {
        trsm_shuffle_4xNB(0);
        BJp += BLK;
    }
    BJp = (vector float*)B;

    for (j = 0; j < VBLK; j++)
    {
        trsm_4x4xRHS_load(0);
        for (i = 0; i < j; i++)
        {
            trsm_4x4xRHS(0);
            Tp += 4;        
            BIp++;
        }
        trsm_4x4xRHS_(0);
        trsm_4x4xRHS_store(0);

        Tp  -= j*4;
        BIp -= j;

        Tp += BLK*4;
        BJp++;
    }
    BJp = (vector float*)B;

    for (i = 0; i < VBLK; i++)
    {
        trsm_shuffle_4xNB(0);
        BJp += BLK;
    }
}

//----------------------------------------------------------------------------------------------
