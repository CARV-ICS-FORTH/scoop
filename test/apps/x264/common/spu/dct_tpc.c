/*****************************************************************************
 * dct.c: h264 encoder library
 *****************************************************************************
 * Copyright (C) 2003-2008 x264 project
 *
 * Authors: Loren Merritt <lorenm@u.washington.edu>
 *          Laurent Aimar <fenrir@via.ecp.fr>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02111, USA.
 *****************************************************************************/

#include "common_tpc.h"
#include "dct_tpc.h"
#include <spu_intrinsics.h>
#include <vec_types.h> ///TODO: Do we need ?
#include <spu_internals.h>


int x264_dct4_weight2_zigzag[2][16];
int x264_dct8_weight2_zigzag[2][64];

/*
 * XXX For all dct dc : input could be equal to output so ...
 */

void dct2x2dc( int16_t d[2][2] )
{
    int tmp[2][2];

    tmp[0][0] = d[0][0] + d[0][1];
    tmp[1][0] = d[0][0] - d[0][1];
    tmp[0][1] = d[1][0] + d[1][1];
    tmp[1][1] = d[1][0] - d[1][1];

    d[0][0] = tmp[0][0] + tmp[0][1];
    d[1][0] = tmp[1][0] + tmp[1][1];
    d[0][1] = tmp[0][0] - tmp[0][1];
    d[1][1] = tmp[1][0] - tmp[1][1];
}

void dct4x4dc( int16_t d[4][4] )
{
    int tmp[4][4];
    int s01, s23;
    int d01, d23;
    int i;
#ifdef VEC
    int as01[4], as23[4];
    int ad01[4], ad23[4];

    vector signed int *ptr_tmp_vec, *ptr_tmp_vec1, *ptr_tmp_vec2;
    vector signed int d_vec0, d_vec1, d_vec2, d_vec3;
    vector signed int as_vec0, as_vec1, as_vec2, as_vec3;
    vector signed int assos_vec = {1,1,1,1};

    // Load vectors 
    d_vec0 = spu_insert( d[0][0], d_vec0, 0);
    d_vec0 = spu_insert( d[1][0], d_vec0, 1);
    d_vec0 = spu_insert( d[2][0], d_vec0, 2);
    d_vec0 = spu_insert( d[3][0], d_vec0, 3);

    d_vec1 = spu_insert( d[0][1], d_vec1, 0);
    d_vec1 = spu_insert( d[1][1], d_vec1, 1);
    d_vec1 = spu_insert( d[2][1], d_vec1, 2);
    d_vec1 = spu_insert( d[3][1], d_vec1, 3);

    d_vec2 = spu_insert( d[0][2], d_vec2, 0);
    d_vec2 = spu_insert( d[1][2], d_vec2, 1);
    d_vec2 = spu_insert( d[2][2], d_vec2, 2);
    d_vec2 = spu_insert( d[3][2], d_vec2, 3);

    d_vec3 = spu_insert( d[0][3], d_vec3, 0);
    d_vec3 = spu_insert( d[1][3], d_vec3, 1);
    d_vec3 = spu_insert( d[2][3], d_vec3, 2);
    d_vec3 = spu_insert( d[3][3], d_vec3, 3);


    // Add first

    ptr_tmp_vec = (vector signed int *) &as01[0];
    *(ptr_tmp_vec) =  d_vec0 + d_vec1;
    ptr_tmp_vec = (vector signed int *)  &as23[0];
    *(ptr_tmp_vec) =  d_vec2 + d_vec3 ;


    // Sub 
    ptr_tmp_vec = (vector signed int *)  &ad23[0];
    *(ptr_tmp_vec) = d_vec2 - d_vec3;
    ptr_tmp_vec = (vector signed int *)  &ad01[0];
    *(ptr_tmp_vec) =  d_vec0 - d_vec1;


    // Now calc tmp
    ptr_tmp_vec1 = (vector signed int *)  &as01[0];
    ptr_tmp_vec2 = (vector signed int *)  &as23[0];
    ptr_tmp_vec  = (vector signed int *)  &tmp[0][0];

    *(ptr_tmp_vec) =   *(ptr_tmp_vec1) + *(ptr_tmp_vec2) ;

    ptr_tmp_vec  = (vector signed int *)  &tmp[1][0];
    *(ptr_tmp_vec) =   *(ptr_tmp_vec1) - *(ptr_tmp_vec2) ;


    ptr_tmp_vec1 = (vector signed int *)  &ad01[0];
    ptr_tmp_vec2 = (vector signed int *)  &ad23[0];

    ptr_tmp_vec  = (vector signed int *)  &tmp[2][0];
    *(ptr_tmp_vec) =  *(ptr_tmp_vec1) - *(ptr_tmp_vec2) ;

    ptr_tmp_vec  = (vector signed int *)  &tmp[3][0];
    *(ptr_tmp_vec) = *(ptr_tmp_vec1) + *(ptr_tmp_vec2) ;


////////////

   
 
#else
    for( i = 0; i < 4; i++ )
    {
        s01 = d[i][0] + d[i][1];
        d01 = d[i][0] - d[i][1];
        s23 = d[i][2] + d[i][3];
        d23 = d[i][2] - d[i][3];

        tmp[0][i] = s01 + s23;
        tmp[1][i] = s01 - s23;
        tmp[2][i] = d01 - d23;
        tmp[3][i] = d01 + d23;
    }

#endif
    for( i = 0; i < 4; i++ )
    {
        s01 = tmp[i][0] + tmp[i][1];
        d01 = tmp[i][0] - tmp[i][1];
        s23 = tmp[i][2] + tmp[i][3];
        d23 = tmp[i][2] - tmp[i][3];

        d[i][0] = ( s01 + s23 + 1 ) >> 1;
        d[i][1] = ( s01 - s23 + 1 ) >> 1;
        d[i][2] = ( d01 - d23 + 1 ) >> 1;
        d[i][3] = ( d01 + d23 + 1 ) >> 1;
    }
}

static inline void pixel_sub_wxh( int16_t *diff, int i_size,
                                  uint8_t *pix1, int i_pix1, uint8_t *pix2, int i_pix2 )
{
    int y, x;
    for( y = 0; y < i_size; y++ )
    {
        for( x = 0; x < i_size; x++ )
        {
            diff[x + y*i_size] = pix1[x] - pix2[x];
        }
        pix1 += i_pix1;
        pix2 += i_pix2;
    }
}

static inline void sub4x4_dct( int16_t dct[4][4], uint8_t *pix1, uint8_t *pix2 )
{
    int16_t d[4][4];
    int tmp[4][4];
    int i;
    pixel_sub_wxh( (int16_t*)d, 4, pix1, FENC_STRIDE, pix2, FDEC_STRIDE );

    for( i = 0; i < 4; i++ )
    {
        const int s03 = d[i][0] + d[i][3];
        const int s12 = d[i][1] + d[i][2];
        const int d03 = d[i][0] - d[i][3];
        const int d12 = d[i][1] - d[i][2];

        tmp[0][i] =   s03 +   s12;
        tmp[1][i] = 2*d03 +   d12;
        tmp[2][i] =   s03 -   s12;
        tmp[3][i] =   d03 - 2*d12;
    }

    for( i = 0; i < 4; i++ )
    {
        const int s03 = tmp[i][0] + tmp[i][3];
        const int s12 = tmp[i][1] + tmp[i][2];
        const int d03 = tmp[i][0] - tmp[i][3];
        const int d12 = tmp[i][1] - tmp[i][2];

        dct[i][0] =   s03 +   s12;
        dct[i][1] = 2*d03 +   d12;
        dct[i][2] =   s03 -   s12;
        dct[i][3] =   d03 - 2*d12;
    }
}

void sub8x8_dct( int16_t dct[4][4][4], uint8_t *pix1, uint8_t *pix2 )
{
    sub4x4_dct( dct[0], &pix1[0], &pix2[0] );
    sub4x4_dct( dct[1], &pix1[4], &pix2[4] );
    sub4x4_dct( dct[2], &pix1[4*FENC_STRIDE+0], &pix2[4*FDEC_STRIDE+0] );
    sub4x4_dct( dct[3], &pix1[4*FENC_STRIDE+4], &pix2[4*FDEC_STRIDE+4] );
}

void sub16x16_dct( int16_t dct[16][4][4], uint8_t *pix1, uint8_t *pix2 )
{
    sub8x8_dct( &dct[ 0], &pix1[0], &pix2[0] );
    sub8x8_dct( &dct[ 4], &pix1[8], &pix2[8] );
    sub8x8_dct( &dct[ 8], &pix1[8*FENC_STRIDE+0], &pix2[8*FDEC_STRIDE+0] );
    sub8x8_dct( &dct[12], &pix1[8*FENC_STRIDE+8], &pix2[8*FDEC_STRIDE+8] );
}


void add4x4_idct( uint8_t *p_dst, int16_t dct[4][4] )
{
    int d[4][4];
    int tmp[4][4];
    int x, y;
    int i;

    for( i = 0; i < 4; i++ )
    {
        const int s02 =  dct[0][i]     +  dct[2][i];
        const int d02 =  dct[0][i]     -  dct[2][i];
        const int s13 =  dct[1][i]     + (dct[3][i]>>1);
        const int d13 = (dct[1][i]>>1) -  dct[3][i];

        tmp[i][0] = s02 + s13;
        tmp[i][1] = d02 + d13;
        tmp[i][2] = d02 - d13;
        tmp[i][3] = s02 - s13;
    }

    for( i = 0; i < 4; i++ )
    {
        const int s02 =  tmp[0][i]     +  tmp[2][i];
        const int d02 =  tmp[0][i]     -  tmp[2][i];
        const int s13 =  tmp[1][i]     + (tmp[3][i]>>1);
        const int d13 = (tmp[1][i]>>1) -  tmp[3][i];

        d[0][i] = ( s02 + s13 + 32 ) >> 6;
        d[1][i] = ( d02 + d13 + 32 ) >> 6;
        d[2][i] = ( d02 - d13 + 32 ) >> 6;
        d[3][i] = ( s02 - s13 + 32 ) >> 6;
    }

    for( y = 0; y < 4; y++ )
    {
        p_dst[0] = x264_clip_uint8( p_dst[0] + d[y][0] );
        p_dst[1] = x264_clip_uint8( p_dst[1] + d[y][1] );
        p_dst[2] = x264_clip_uint8( p_dst[2] + d[y][2] );
        p_dst[3] = x264_clip_uint8( p_dst[3] + d[y][3] );
        p_dst += FDEC_STRIDE;

    }
}

void add8x8_idct( uint8_t *p_dst, int16_t dct[4][4][4] )
{
    add4x4_idct( &p_dst[0],               dct[0] );
    add4x4_idct( &p_dst[4],               dct[1] );
    add4x4_idct( &p_dst[4*FDEC_STRIDE+0], dct[2] );
    add4x4_idct( &p_dst[4*FDEC_STRIDE+4], dct[3] );
}

void add16x16_idct( uint8_t *p_dst, int16_t dct[16][4][4] )
{
    add8x8_idct( &p_dst[0],               &dct[0] );
    add8x8_idct( &p_dst[8],               &dct[4] );
    add8x8_idct( &p_dst[8*FDEC_STRIDE+0], &dct[8] );
    add8x8_idct( &p_dst[8*FDEC_STRIDE+8], &dct[12] );
}

/****************************************************************************
 * 8x8 transform:
 ****************************************************************************/

#define DCT8_1D {\
    const int s07 = SRC(0) + SRC(7);\
    const int s16 = SRC(1) + SRC(6);\
    const int s25 = SRC(2) + SRC(5);\
    const int s34 = SRC(3) + SRC(4);\
    const int a0 = s07 + s34;\
    const int a1 = s16 + s25;\
    const int a2 = s07 - s34;\
    const int a3 = s16 - s25;\
    const int d07 = SRC(0) - SRC(7);\
    const int d16 = SRC(1) - SRC(6);\
    const int d25 = SRC(2) - SRC(5);\
    const int d34 = SRC(3) - SRC(4);\
    const int a4 = d16 + d25 + (d07 + (d07>>1));\
    const int a5 = d07 - d34 - (d25 + (d25>>1));\
    const int a6 = d07 + d34 - (d16 + (d16>>1));\
    const int a7 = d16 - d25 + (d34 + (d34>>1));\
    DST(0) =  a0 + a1     ;\
    DST(1) =  a4 + (a7>>2);\
    DST(2) =  a2 + (a3>>1);\
    DST(3) =  a5 + (a6>>2);\
    DST(4) =  a0 - a1     ;\
    DST(5) =  a6 - (a5>>2);\
    DST(6) = (a2>>1) - a3 ;\
    DST(7) = (a4>>2) - a7 ;\
}

void sub8x8_dct8( int16_t dct[8][8], uint8_t *pix1, uint8_t *pix2 )
{
    int i;
    int tmp[8][8];

    pixel_sub_wxh( (int16_t*)tmp, 8, pix1, FENC_STRIDE, pix2, FDEC_STRIDE );

#define SRC(x) tmp[x][i]
#define DST(x) tmp[x][i]
    for( i = 0; i < 8; i++ )
        DCT8_1D
#undef SRC
#undef DST

#define SRC(x) tmp[i][x]
#define DST(x) dct[x][i]
    for( i = 0; i < 8; i++ )
        DCT8_1D
#undef SRC
#undef DST
}

void sub16x16_dct8( int16_t dct[4][8][8], uint8_t *pix1, uint8_t *pix2 )
{
    sub8x8_dct8( dct[0], &pix1[0],               &pix2[0] );
    sub8x8_dct8( dct[1], &pix1[8],               &pix2[8] );
    sub8x8_dct8( dct[2], &pix1[8*FENC_STRIDE+0], &pix2[8*FDEC_STRIDE+0] );
    sub8x8_dct8( dct[3], &pix1[8*FENC_STRIDE+8], &pix2[8*FDEC_STRIDE+8] );
}

#define IDCT8_1D {\
    const int a0 =  SRC(0) + SRC(4);\
    const int a2 =  SRC(0) - SRC(4);\
    const int a4 = (SRC(2)>>1) - SRC(6);\
    const int a6 = (SRC(6)>>1) + SRC(2);\
    const int b0 = a0 + a6;\
    const int b2 = a2 + a4;\
    const int b4 = a2 - a4;\
    const int b6 = a0 - a6;\
    const int a1 = -SRC(3) + SRC(5) - SRC(7) - (SRC(7)>>1);\
    const int a3 =  SRC(1) + SRC(7) - SRC(3) - (SRC(3)>>1);\
    const int a5 = -SRC(1) + SRC(7) + SRC(5) + (SRC(5)>>1);\
    const int a7 =  SRC(3) + SRC(5) + SRC(1) + (SRC(1)>>1);\
    const int b1 = (a7>>2) + a1;\
    const int b3 =  a3 + (a5>>2);\
    const int b5 = (a3>>2) - a5;\
    const int b7 =  a7 - (a1>>2);\
    DST(0, b0 + b7);\
    DST(1, b2 + b5);\
    DST(2, b4 + b3);\
    DST(3, b6 + b1);\
    DST(4, b6 - b1);\
    DST(5, b4 - b3);\
    DST(6, b2 - b5);\
    DST(7, b0 - b7);\
}

void add8x8_idct8( uint8_t *dst, int16_t dct[8][8] )
{
    int i;

    dct[0][0] += 32; // rounding for the >>6 at the end

#define SRC(x)     dct[x][i]
#define DST(x,rhs) dct[x][i] = (rhs)
    for( i = 0; i < 8; i++ )
        IDCT8_1D
#undef SRC
#undef DST

#define SRC(x)     dct[i][x]
#define DST(x,rhs) dst[i + x*FDEC_STRIDE] = x264_clip_uint8( dst[i + x*FDEC_STRIDE] + ((rhs) >> 6) );
    for( i = 0; i < 8; i++ )
        IDCT8_1D
#undef SRC
#undef DST
}

void add16x16_idct8( uint8_t *dst, int16_t dct[4][8][8] )
{
    add8x8_idct8( &dst[0],               dct[0] );
    add8x8_idct8( &dst[8],               dct[1] );
    add8x8_idct8( &dst[8*FDEC_STRIDE+0], dct[2] );
    add8x8_idct8( &dst[8*FDEC_STRIDE+8], dct[3] );
}

/// TODO: fix  it
#if 0
void x264_dct_init_weights( void )
{
    int i, j;
    for( j=0; j<2; j++ )
    {
        for( i=0; i<16; i++ )
            x264_dct4_weight2_zigzag[j][i] = x264_dct4_weight2_tab[ x264_zigzag_scan4[j][i] ];
        for( i=0; i<64; i++ )
            x264_dct8_weight2_zigzag[j][i] = x264_dct8_weight2_tab[ x264_zigzag_scan8[j][i] ];
    }
}
#endif

// gcc pessimizes multi-dimensional arrays here, even with constant indices
#define ZIG(i,y,x) level[i] = dct[0][x*8+y];


#undef ZIG
#define ZIG(i,y,x) level[i] = dct[0][x*4+y];

#ifdef VEC
void zigzag_scan_4x4_frame( int16_t level[16], int16_t *dct )
{
	vector signed short *level_vec0 = (vector signed short *) &level[0];
	vector signed short *level_vec1 = (vector signed short *) &level[8];
	vector signed short *dct_vec0 = (vector signed short *) &dct[0];
	vector signed short *dct_vec1 = (vector signed short *) &dct[8];
	vector unsigned char pattern0 = {0,1,8,9,2,3,4,5,10,11,16,17,24,25,18,19 } ;
	vector unsigned char pattern1 = {12,13,6,7,14,15,20,21,26,27,28,29,22,23,30,31 } ;
	*level_vec0 = spu_shuffle( *dct_vec0, *dct_vec1 , pattern0);
	*level_vec1 = spu_shuffle( *dct_vec0, *dct_vec1 , pattern1);


}
#else
void zigzag_scan_4x4_frame( int16_t level[16],  int16_t dct[4][4]  )
{
    ZIG( 0,0,0) ZIG( 1,0,1) ZIG( 2,1,0) ZIG( 3,2,0)
    ZIG( 4,1,1) ZIG( 5,0,2) ZIG( 6,0,3) ZIG( 7,1,2)
    ZIG( 8,2,1) ZIG( 9,3,0) ZIG(10,3,1) ZIG(11,2,2)
    ZIG(12,1,3) ZIG(13,2,3) ZIG(14,3,2) ZIG(15,3,3)
}
#endif
void zigzag_scan_4x4_field( int16_t level[16], int16_t dct[4][4] )
{
    *(uint32_t*)level = *(uint32_t*)dct;
    ZIG(2,0,1) ZIG(3,2,0) ZIG(4,3,0) ZIG(5,1,1)
    *(uint32_t*)(level+6) = *(uint32_t*)(*dct+6);
    *(uint64_t*)(level+8) = *(uint64_t*)(*dct+8);
    *(uint64_t*)(level+12) = *(uint64_t*)(*dct+12);
}

#undef ZIG
#define ZIG(i,y,x) {\
    int oe = x+y*FENC_STRIDE;\
    int od = x+y*FDEC_STRIDE;\
    level[i] = p_src[oe] - p_dst[od];\
}
#define COPY4x4\
    *(uint32_t*)(p_dst+0*FDEC_STRIDE) = *(uint32_t*)(p_src+0*FENC_STRIDE);\
    *(uint32_t*)(p_dst+1*FDEC_STRIDE) = *(uint32_t*)(p_src+1*FENC_STRIDE);\
    *(uint32_t*)(p_dst+2*FDEC_STRIDE) = *(uint32_t*)(p_src+2*FENC_STRIDE);\
    *(uint32_t*)(p_dst+3*FDEC_STRIDE) = *(uint32_t*)(p_src+3*FENC_STRIDE);\

void zigzag_sub_4x4_frame( int16_t level[16], const uint8_t *p_src, uint8_t *p_dst )
{
    ZIG( 0,0,0) ZIG( 1,0,1) ZIG( 2,1,0) ZIG( 3,2,0)
    ZIG( 4,1,1) ZIG( 5,0,2) ZIG( 6,0,3) ZIG( 7,1,2)
    ZIG( 8,2,1) ZIG( 9,3,0) ZIG(10,3,1) ZIG(11,2,2)
    ZIG(12,1,3) ZIG(13,2,3) ZIG(14,3,2) ZIG(15,3,3)
    COPY4x4
}


#undef ZIG
#undef COPY4x4




