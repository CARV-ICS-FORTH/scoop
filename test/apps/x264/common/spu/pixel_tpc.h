/*****************************************************************************
 * pixel.h: h264 encoder library
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

#ifndef X264_PIXEL_TPC_H
#define X264_PIXEL_TPC_H

// SSD assumes all args aligned
// other cmp functions assume first arg aligned
typedef int  (*x264_pixel_cmp_t) ( uint8_t *, int, uint8_t *, int );
typedef void (*x264_pixel_cmp_x3_t) ( uint8_t *, uint8_t *, uint8_t *, uint8_t *, int, int[3] );
typedef void (*x264_pixel_cmp_x4_t) ( uint8_t *, uint8_t *, uint8_t *, uint8_t *, uint8_t *, int, int[4] );
typedef int  (*x264_adsf_t)( int enc_dc[4], uint16_t *sums, int delta,
               uint16_t *cost_mvx, int16_t *mvs, int width, int thresh );

enum
{
    PIXEL_16x16 = 0,
    PIXEL_16x8  = 1,
    PIXEL_8x16  = 2,
    PIXEL_8x8   = 3,
    PIXEL_8x4   = 4,
    PIXEL_4x8   = 5,
    PIXEL_4x4   = 6,
    PIXEL_4x2   = 7,
    PIXEL_2x4   = 8,
    PIXEL_2x2   = 9,
};

static const struct {
    int w;
    int h;
} x264_pixel_size[7] = {
    { 16, 16 },
    { 16,  8 }, {  8, 16 },
    {  8,  8 },
    {  8,  4 }, {  4,  8 },
    {  4,  4 }
};

static const uint8_t x264_size2pixel[5][5] = {
    { 0, },
    { 0, PIXEL_4x4, PIXEL_8x4, 0, 0 },
    { 0, PIXEL_4x8, PIXEL_8x8, 0, PIXEL_16x8 },
    { 0, },
    { 0, 0,        PIXEL_8x16, 0, PIXEL_16x16 }
};


static inline int x264_pixel_sad_16x16( uint8_t *pix1, int i_stride_pix1,
                 uint8_t *pix2, int i_stride_pix2 );


static inline  int x264_pixel_sad_16x8( uint8_t *pix1, int i_stride_pix1,
                 uint8_t *pix2, int i_stride_pix2 );

static inline  int x264_pixel_sad_8x16( uint8_t *pix1, int i_stride_pix1,
                 uint8_t *pix2, int i_stride_pix2 );

 inline  int x264_pixel_sad_8x8( uint8_t *pix1, int i_stride_pix1,
                 uint8_t *pix2, int i_stride_pix2 );




int x264_pixel_ssd_16x16( uint8_t *pix1, int i_stride_pix1,
                 uint8_t *pix2, int i_stride_pix2 );


int x264_pixel_ssd_16x8( uint8_t *pix1, int i_stride_pix1,
                 uint8_t *pix2, int i_stride_pix2 );

int x264_pixel_ssd_8x16( uint8_t *pix1, int i_stride_pix1,
                 uint8_t *pix2, int i_stride_pix2 );

int x264_pixel_ssd_8x8( uint8_t *pix1, int i_stride_pix1,
                 uint8_t *pix2, int i_stride_pix2 );


int x264_pixel_satd_16x16( uint8_t *pix1, int i_stride_pix1,
                            uint8_t *pix2, int i_stride_pix2 );

int x264_pixel_sa8d_16x16( uint8_t *pix1, int i_stride_pix1,
                                               uint8_t *pix2, int i_stride_pix2 );

int x264_pixel_var_16x16( uint8_t *pix, int i_stride, uint32_t *sad );

void x264_pixel_sad_x3_16x16( uint8_t *fenc, uint8_t *pix0, uint8_t *pix1, uint8_t *pix2, int i_stride, int scores[3] );
void x264_pixel_sad_x4_16x16( uint8_t *fenc, uint8_t *pix0, uint8_t *pix1, uint8_t *pix2, uint8_t *pix3, int i_stride, int scores[4] );

void x264_pixel_satd_x3_16x16( uint8_t *fenc, uint8_t *pix0, uint8_t *pix1, uint8_t *pix2, int i_stride, int scores[3] );
void x264_pixel_satd_x4_16x16( uint8_t *fenc, uint8_t *pix0, uint8_t *pix1, uint8_t *pix2, uint8_t *pix3, int i_stride, int scores[4] );



int x264_pixel_ads4( int enc_dc[4], uint16_t *sums, int delta,
                            uint16_t *cost_mvx, int16_t *mvs, int width, int thresh );

int x264_pixel_ads2( int enc_dc[2], uint16_t *sums, int delta,
                            uint16_t *cost_mvx, int16_t *mvs, int width, int thresh );

int x264_pixel_ads1( int enc_dc[1], uint16_t *sums, int delta,
                            uint16_t *cost_mvx, int16_t *mvs, int width, int thresh );

extern x264_pixel_cmp_t  sadf[7];
extern x264_pixel_cmp_t  ssdf[7];
extern x264_pixel_cmp_t satdf[7];
extern x264_pixel_cmp_t sa8df[7];

#if 1
extern x264_pixel_cmp_t mbcmpf[7]; /* either satd or sad for subpel refine and mode decision */
extern x264_pixel_cmp_t fpelcmpf[7]; /* either satd or sad for fullpel motion search */

extern x264_pixel_cmp_x3_t fpelcmp_x3f[7];
extern x264_pixel_cmp_x4_t fpelcmp_x4f[7];
#else

extern x264_pixel_cmp_t mbcmpf[7];
extern x264_pixel_cmp_t fpelcmpf[7]; /* either satd or sad for fullpel motion search */

extern x264_pixel_cmp_x3_t fpelcmp_x3f[7];
extern x264_pixel_cmp_x4_t fpelcmp_x4f[7];
#endif



/* multiple parallel calls to cmp. */
extern x264_pixel_cmp_x3_t sad_x3f[7];
extern x264_pixel_cmp_x4_t sad_x4f[7];
extern x264_pixel_cmp_x3_t satd_x3f[7];
extern x264_pixel_cmp_x4_t satd_x4f[7];

/* abs-diff-sum for successive elimination.
 * may round width up to a multiple of 16. */



extern x264_adsf_t adsf[7];


#endif
