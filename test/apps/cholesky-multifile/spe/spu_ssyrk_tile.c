//==============================================================================================
//
//  Innovative Computing Laboratory - Computer Science Department - University of Tennessee
//  Written by Jakub Kurzak
//
//==============================================================================================

// #include <spu_intrinsics.h>

#include "spu_ssyrk_tile.h"

//----------------------------------------------------------------------------------------------

#define  BLK 64
#define VBLK 16

//----------------------------------------------------------------------------------------------

// #pragma css task input(A[64][64]) inout(C[64][64])
void spu_ssyrk_tile(float *A, float *C)
{
    vector float *Ap = (vector float*)A;
    vector float *Tp = (vector float*)A;
    vector float *Cp = (vector float*)C;

    vector float c0_0, c0_1, c0_2, c0_3;
    vector float c1_0, c1_1, c1_2, c1_3;
    vector float c2_0, c2_1, c2_2, c2_3;
    vector float c3_0, c3_1, c3_2, c3_3;

    vector float zero = (vector float){ 0.0, 0.0, 0.0, 0.0};

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

    int i, j;

    //----------------------------------------------------------

    #define ssyrk_1x1xNB(c, OFFA, OFFAT)\
    \
        c = spu_mul (Ap[OFFA+ 0], Tp[OFFAT+ 0]);\
        c = spu_madd(Ap[OFFA+ 1], Tp[OFFAT+ 1], c);\
        c = spu_madd(Ap[OFFA+ 2], Tp[OFFAT+ 2], c);\
        c = spu_madd(Ap[OFFA+ 3], Tp[OFFAT+ 3], c);\
        c = spu_madd(Ap[OFFA+ 4], Tp[OFFAT+ 4], c);\
        c = spu_madd(Ap[OFFA+ 5], Tp[OFFAT+ 5], c);\
        c = spu_madd(Ap[OFFA+ 6], Tp[OFFAT+ 6], c);\
        c = spu_madd(Ap[OFFA+ 7], Tp[OFFAT+ 7], c);\
        c = spu_madd(Ap[OFFA+ 8], Tp[OFFAT+ 8], c);\
        c = spu_madd(Ap[OFFA+ 9], Tp[OFFAT+ 9], c);\
        c = spu_madd(Ap[OFFA+10], Tp[OFFAT+10], c);\
        c = spu_madd(Ap[OFFA+11], Tp[OFFAT+11], c);\
        c = spu_madd(Ap[OFFA+12], Tp[OFFAT+12], c);\
        c = spu_madd(Ap[OFFA+13], Tp[OFFAT+13], c);\
        c = spu_madd(Ap[OFFA+14], Tp[OFFAT+14], c);\
        c = spu_madd(Ap[OFFA+15], Tp[OFFAT+15], c);\

    //----------------------------------------------------------

    #define ssyrk_4x1xNB(c0, c1, c2, c3, OFFAT)\
    \
        ssyrk_1x1xNB(c0, 0*VBLK, OFFAT);\
        ssyrk_1x1xNB(c1, 1*VBLK, OFFAT);\
        ssyrk_1x1xNB(c2, 2*VBLK, OFFAT);\
        ssyrk_1x1xNB(c3, 3*VBLK, OFFAT);\

    //----------------------------------------------------------

    #define ssyrk_4x4xNB(OFFAT)\
    \
        ssyrk_4x1xNB(c0_0, c0_1, c0_2, c0_3, OFFAT + 0*VBLK);\
        ssyrk_4x1xNB(c1_0, c1_1, c1_2, c1_3, OFFAT + 1*VBLK);\
        ssyrk_4x1xNB(c2_0, c2_1, c2_2, c2_3, OFFAT + 2*VBLK);\
        ssyrk_4x1xNB(c3_0, c3_1, c3_2, c3_3, OFFAT + 3*VBLK);\

    //----------------------------------------------------------

    #define ssyrk_4x4xNB_(OFFAT)\
    \
        ssyrk_1x1xNB(c0_0, 0*VBLK, OFFAT + 0*VBLK);\
        ssyrk_1x1xNB(c0_1, 1*VBLK, OFFAT + 0*VBLK);\
        ssyrk_1x1xNB(c0_2, 2*VBLK, OFFAT + 0*VBLK);\
        ssyrk_1x1xNB(c0_3, 3*VBLK, OFFAT + 0*VBLK);\
        \
        c1_0 = zero;\
        ssyrk_1x1xNB(c1_1, 1*VBLK, OFFAT + 1*VBLK);\
        ssyrk_1x1xNB(c1_2, 2*VBLK, OFFAT + 1*VBLK);\
        ssyrk_1x1xNB(c1_3, 3*VBLK, OFFAT + 1*VBLK);\
        \
        c2_0 = zero;\
        c2_1 = zero;\
        ssyrk_1x1xNB(c2_2, 2*VBLK, OFFAT + 2*VBLK);\
        ssyrk_1x1xNB(c2_3, 3*VBLK, OFFAT + 2*VBLK);\
        \
        c3_0 = zero;\
        c3_1 = zero;\
        c3_2 = zero;\
        ssyrk_1x1xNB(c3_3, 3*VBLK, OFFAT + 3*VBLK);\

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

    #define shuffle_4x4(OFFC)\
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
        Cp[OFFC + 0*VBLK] = spu_sub(Cp[OFFC + 0*VBLK], c0_0);\
        Cp[OFFC + 1*VBLK] = spu_sub(Cp[OFFC + 1*VBLK], c1_0);\
        Cp[OFFC + 2*VBLK] = spu_sub(Cp[OFFC + 2*VBLK], c2_0);\
        Cp[OFFC + 3*VBLK] = spu_sub(Cp[OFFC + 3*VBLK], c3_0);\

    //----------------------------------------------------------

    for (j = 0; j < VBLK; j++)
    {
        for (i = 0; i < j; i++)
        {
            ssyrk_4x4xNB(0);
            shuffle_4x4(0);

            Tp += BLK;
            Cp++;
        }
        ssyrk_4x4xNB_(0);
        shuffle_4x4(0);

        Ap += BLK;
        Tp -= BLK*j;
        Cp += BLK-j;
    }
}

//----------------------------------------------------------------------------------------------
