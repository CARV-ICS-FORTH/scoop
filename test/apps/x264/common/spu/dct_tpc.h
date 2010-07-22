/*****************************************************************************
 * dct.h: h264 encoder library
 *****************************************************************************
 * Copyright (C) 2004-2008 Loren Merritt <lorenm@u.washington.edu>
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

#ifndef X264_DCT_H
#define X264_DCT_H

/* the inverse of the scaling factors introduced by 8x8 fdct */
#define W(i) (i==0 ? FIX8(1.0000) :\
              i==1 ? FIX8(0.8859) :\
              i==2 ? FIX8(1.6000) :\
              i==3 ? FIX8(0.9415) :\
              i==4 ? FIX8(1.2651) :\
              i==5 ? FIX8(1.1910) :0)
static const uint16_t x264_dct8_weight_tab[64] = {
    W(0), W(3), W(4), W(3),  W(0), W(3), W(4), W(3),
    W(3), W(1), W(5), W(1),  W(3), W(1), W(5), W(1),
    W(4), W(5), W(2), W(5),  W(4), W(5), W(2), W(5),
    W(3), W(1), W(5), W(1),  W(3), W(1), W(5), W(1),

    W(0), W(3), W(4), W(3),  W(0), W(3), W(4), W(3),
    W(3), W(1), W(5), W(1),  W(3), W(1), W(5), W(1),
    W(4), W(5), W(2), W(5),  W(4), W(5), W(2), W(5),
    W(3), W(1), W(5), W(1),  W(3), W(1), W(5), W(1)
};
#undef W

#define W(i) (i==0 ? FIX8(1.76777) :\
              i==1 ? FIX8(1.11803) :\
              i==2 ? FIX8(0.70711) :0)
static const uint16_t x264_dct4_weight_tab[16] = {
    W(0), W(1), W(0), W(1),
    W(1), W(2), W(1), W(2),
    W(0), W(1), W(0), W(1),
    W(1), W(2), W(1), W(2)
};
#undef W

/* inverse squared */
#define W(i) (i==0 ? FIX8(3.125) :\
              i==1 ? FIX8(1.25) :\
              i==2 ? FIX8(0.5) :0)
static const uint16_t x264_dct4_weight2_tab[16] = {
    W(0), W(1), W(0), W(1),
    W(1), W(2), W(1), W(2),
    W(0), W(1), W(0), W(1),
    W(1), W(2), W(1), W(2)
};
#undef W

#define W(i) (i==0 ? FIX8(1.00000) :\
              i==1 ? FIX8(0.78487) :\
              i==2 ? FIX8(2.56132) :\
              i==3 ? FIX8(0.88637) :\
              i==4 ? FIX8(1.60040) :\
              i==5 ? FIX8(1.41850) :0)
static const uint16_t x264_dct8_weight2_tab[64] = {
    W(0), W(3), W(4), W(3),  W(0), W(3), W(4), W(3),
    W(3), W(1), W(5), W(1),  W(3), W(1), W(5), W(1),
    W(4), W(5), W(2), W(5),  W(4), W(5), W(2), W(5),
    W(3), W(1), W(5), W(1),  W(3), W(1), W(5), W(1),

    W(0), W(3), W(4), W(3),  W(0), W(3), W(4), W(3),
    W(3), W(1), W(5), W(1),  W(3), W(1), W(5), W(1),
    W(4), W(5), W(2), W(5),  W(4), W(5), W(2), W(5),
    W(3), W(1), W(5), W(1),  W(3), W(1), W(5), W(1)
};
#undef W

extern int x264_dct4_weight2_zigzag[2][16]; // [2] = {frame, field}
extern int x264_dct8_weight2_zigzag[2][64];


void dct2x2dc( int16_t d[2][2] );

void dct4x4dc( int16_t d[4][4] );
void idct4x4dc( int16_t d[4][4] );


void add4x4_idct( uint8_t *p_dst, int16_t dct[4][4] );

static inline void pixel_sub_wxh( int16_t *diff, int i_size,
                                  uint8_t *pix1, int i_pix1, uint8_t *pix2, int i_pix2 );
void sub8x8_dct( int16_t dct[4][4][4], uint8_t *pix1, uint8_t *pix2 );
void sub16x16_dct( int16_t dct[16][4][4], uint8_t *pix1, uint8_t *pix2 );
void add8x8_idct( uint8_t *p_dst, int16_t dct[4][4][4] );

void add16x16_idct( uint8_t *p_dst, int16_t dct[16][4][4] );
void sub8x8_dct8( int16_t dct[8][8], uint8_t *pix1, uint8_t *pix2 );
void sub16x16_dct8( int16_t dct[4][8][8], uint8_t *pix1, uint8_t *pix2 );
void add8x8_idct8( uint8_t *dst, int16_t dct[8][8] );
void add16x16_idct8( uint8_t *dst, int16_t dct[4][8][8] );




void zigzag_scan_8x8_frame( int16_t level[64], int16_t dct[8][8] );
#ifdef VEC
void zigzag_scan_4x4_frame( int16_t level[16], int16_t *dct );
#else
void zigzag_scan_4x4_frame( int16_t level[16], int16_t dct[4][4]  );
#endif
void zigzag_sub_4x4_frame( int16_t level[16], const uint8_t *p_src, uint8_t *p_dst );
void zigzag_scan_4x4_field( int16_t level[16], int16_t dct[4][4] );

#endif
