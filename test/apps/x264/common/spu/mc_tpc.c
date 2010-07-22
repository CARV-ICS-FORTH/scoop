/*****************************************************************************
 * mc.c: h264 encoder library (Motion Compensation)
 *****************************************************************************
 * Copyright (C) 2003-2008 x264 project
 *
 * Authors: Laurent Aimar <fenrir@via.ecp.fr>
 *          Loren Merritt <lorenm@u.washington.edu>
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

#include <spu_intrinsics.h>
#include "common_tpc.h"





static inline void pixel_avg( uint8_t *dst,  int i_dst_stride,
                              uint8_t *src1, int i_src1_stride,
                              uint8_t *src2, int i_src2_stride,
                              int i_width, int i_height )
{
    int x, y;
    for( y = 0; y < i_height; y++ )
    {
        for( x = 0; x < i_width; x++ )
        {
            dst[x] = ( src1[x] + src2[x] + 1 ) >> 1;
        }
        dst  += i_dst_stride;
        src1 += i_src1_stride;
        src2 += i_src2_stride;
    }
}






static inline void pixel_avg_wxh( uint8_t *dst, int i_dst, uint8_t *src1, int i_src1, uint8_t *src2, int i_src2, int width, int height )
{
    int x, y;
    for( y = 0; y < height; y++ )
    {
        for( x = 0; x < width; x++ )
        {
            dst[x] = ( src1[x] + src2[x] + 1 ) >> 1;
        }
        src1 += i_src1;
        src2 += i_src2;
        dst += i_dst;
    }
}


/* Implicit weighted bipred only:
 * assumes log2_denom = 5, offset = 0, weight1 + weight2 = 64 */
#define op_scale2(x) dst[x] = x264_clip_uint8( (src1[x]*i_weight1 + src2[x]*i_weight2 + (1<<5)) >> 6 )
static inline void pixel_avg_weight_wxh( uint8_t *dst, int i_dst, uint8_t *src1, int i_src1, uint8_t *src2, int i_src2, int width, int height, int i_weight1 )
{
    int y;
    const int i_weight2 = 64 - i_weight1;
    for( y = 0; y<height; y++, dst += i_dst, src1 += i_src1, src2 += i_src2 )
    {
        op_scale2(0);
        op_scale2(1);
        if(width==2) continue;
        op_scale2(2);
        op_scale2(3);
        if(width==4) continue;
        op_scale2(4);
        op_scale2(5);
        op_scale2(6);
        op_scale2(7);
        if(width==8) continue;
        op_scale2(8);
        op_scale2(9);
        op_scale2(10);
        op_scale2(11);
        op_scale2(12);
        op_scale2(13);
        op_scale2(14);
        op_scale2(15);
    }
}
#undef op_scale2

#define PIXEL_AVG_C( name, width, height ) \
void name( uint8_t *pix1, int i_stride_pix1, \
                  uint8_t *pix2, int i_stride_pix2, \
                  uint8_t *pix3, int i_stride_pix3, int weight ) \
{ \
    if( weight == 32 )\
        pixel_avg_wxh( pix1, i_stride_pix1, pix2, i_stride_pix2, pix3, i_stride_pix3, width, height ); \
    else\
        pixel_avg_weight_wxh( pix1, i_stride_pix1, pix2, i_stride_pix2, pix3, i_stride_pix3, width, height, weight ); \
}

PIXEL_AVG_C( pixel_avg_16x8,  16, 8 )
PIXEL_AVG_C( pixel_avg_8x16,  8, 16 )
PIXEL_AVG_C( pixel_avg_8x8,   8, 8 )
PIXEL_AVG_C( pixel_avg_8x4,   8, 4 )
PIXEL_AVG_C( pixel_avg_4x8,   4, 8 )
PIXEL_AVG_C( pixel_avg_4x4,   4, 4 )
PIXEL_AVG_C( pixel_avg_4x2,   4, 2 )
PIXEL_AVG_C( pixel_avg_2x4,   2, 4 )
PIXEL_AVG_C( pixel_avg_2x2,   2, 2 )


#ifdef VEC
void pixel_avg_16x16( uint8_t *dst,  int i_dst,
                                  uint8_t *src1, int i_src1,
                                  uint8_t *src2, int i_src2,
                                  int i_height )
{
    int y;
    vector unsigned char *src1v, *src2v, *dstv;


    for( y = 0; y < 16; y++ )
    {

       src1v = (vector unsigned char *) src1;
       src2v = (vector unsigned char *) src2;
       dstv = (vector unsigned char *) dst;

       *dstv = spu_avg(*src1v,*src2v);

       dst  += i_dst;
       src1 += i_src1;
       src2 += i_src2;
    }
}

#else

PIXEL_AVG_C( pixel_avg_16x16, 16, 16 )

#endif


x264_pixel_avg_t avgf[10]={
 	pixel_avg_16x16,
 	pixel_avg_16x8,
 	pixel_avg_8x16,
 	pixel_avg_8x8,
 	pixel_avg_8x4,
 	pixel_avg_4x8,
 	pixel_avg_4x4,
 	pixel_avg_4x2,
 	pixel_avg_2x4,
 	pixel_avg_2x4
};


void mc_copy( uint8_t *src, int i_src_stride, uint8_t *dst, int i_dst_stride, int i_width, int i_height )
{
    int y;
    for( y = 0; y < i_height; y++ )
    {
        memcpy( dst, src, i_width );

        src += i_src_stride;
        dst += i_dst_stride;
    }
}




#define MC_COPY(W) \
void mc_copy_w##W( uint8_t *dst, int i_dst, uint8_t *src, int i_src, int i_height ) \
{ \
    mc_copy( src, i_src, dst, i_dst, W, i_height ); \
}
MC_COPY( 8 )
MC_COPY( 4 )

#ifdef VEC
void mc_copy_w16( uint8_t *dst, int i_dst_stride, uint8_t *src, int i_src_stride, int i_height ) 
{ 
    int y;
    vector unsigned char *dst_v ;
    vector unsigned char *src_v ;
    for( y = 0; y < i_height; y++ )
    {
	dst_v =  (vector unsigned char *) dst ;
	src_v =  (vector unsigned char *) src ;

	*dst_v = *src_v;

        src += i_src_stride;
        dst += i_dst_stride;
    }
}

#else
MC_COPY( 16 )
#endif

#define TAPFILTER(pix, d) ((pix)[x-2*d] + (pix)[x+3*d] - 5*((pix)[x-d] + (pix)[x+2*d]) + 20*((pix)[x] + (pix)[x+d]))
void hpel_filter_tpc( uint8_t *dsth, uint8_t *dstv, uint8_t *dstc, uint8_t *src,
                         int stride, int width, int height )
{

    int16_t *buf = malloc((width+5)*sizeof(int16_t));

    int x, y;
    for( y=0; y<height; y++ )
    {
        for( x=-2; x<width+3; x++ )
        {
            int v = TAPFILTER(src,stride);
            dstv[x] = x264_clip_uint8((v + 16) >> 5);
            buf[x+2] = v;
        }
        for( x=0; x<width; x++ )
            dstc[x] = x264_clip_uint8((TAPFILTER(buf+2,1) + 512) >> 10);
        for( x=0; x<width; x++ )
            dsth[x] = x264_clip_uint8((TAPFILTER(src,1) + 16) >> 5);
        dsth += stride;
        dstv += stride;
        dstc += stride;
        src += stride;
    }
    free(buf);
}
static const int hpel_ref0[16] = {0,1,1,1,0,1,1,1,2,3,3,3,0,1,1,1};
static const int hpel_ref1[16] = {0,0,0,0,2,2,3,2,2,2,3,2,2,2,3,2};

void mc_luma( uint8_t *dst,    int i_dst_stride,
                     uint8_t *src[4], int i_src_stride,
                     int mvx, int mvy,
                     int i_width, int i_height )
{
    int qpel_idx = ((mvy&3)<<2) + (mvx&3);
    int offset = (mvy>>2)*i_src_stride + (mvx>>2);
    uint8_t *src1 = src[hpel_ref0[qpel_idx]] + offset + ((mvy&3) == 3) * i_src_stride;



    if( qpel_idx & 5 ) /* qpel interpolation needed */
    {
        uint8_t *src2 = src[hpel_ref1[qpel_idx]] + offset + ((mvx&3) == 3);
        pixel_avg( dst, i_dst_stride, src1, i_src_stride,
                   src2, i_src_stride, i_width, i_height );
    }
    else
    {
        mc_copy( src1, i_src_stride, dst, i_dst_stride, i_width, i_height );
    }
}

uint8_t *get_ref( uint8_t *dst,   int *i_dst_stride,
                         uint8_t *src[4], int i_src_stride,
                         int mvx, int mvy,
                         int i_width, int i_height )
{
    int qpel_idx = ((mvy&3)<<2) + (mvx&3);
    int offset = (mvy>>2)*i_src_stride + (mvx>>2);
    uint8_t *src1 = src[hpel_ref0[qpel_idx]] + offset + ((mvy&3) == 3) * i_src_stride;

    if( qpel_idx & 5 ) /* qpel interpolation needed */
    {
        uint8_t *src2 = src[hpel_ref1[qpel_idx]] + offset + ((mvx&3) == 3);
        pixel_avg( dst, *i_dst_stride, src1, i_src_stride,
                   src2, i_src_stride, i_width, i_height );
        return dst;
    }
    else
    {
        *i_dst_stride = i_src_stride;
        return src1;
    }
}

/* full chroma mc (ie until 1/8 pixel)*/
void mc_chroma( uint8_t *dst, int i_dst_stride,
                       uint8_t *src, int i_src_stride,
                       int mvx, int mvy,
                       int i_width, int i_height )
{
    uint8_t *srcp;
    int x, y;

    const int d8x = mvx&0x07;
    const int d8y = mvy&0x07;

    const int cA = (8-d8x)*(8-d8y);
    const int cB = d8x    *(8-d8y);
    const int cC = (8-d8x)*d8y;
    const int cD = d8x    *d8y;

    src  += (mvy >> 3) * i_src_stride + (mvx >> 3);
    srcp = &src[i_src_stride];

    for( y = 0; y < i_height; y++ )
    {
        for( x = 0; x < i_width; x++ )
        {
            dst[x] = ( cA*src[x]  + cB*src[x+1] +
                       cC*srcp[x] + cD*srcp[x+1] + 32 ) >> 6;
        }
        dst  += i_dst_stride;

        src   = srcp;
        srcp += i_src_stride;
    }
}

#if 0

void x264_frame_filter( x264_data_t *data, uint8_t *pix, int mb_y, int b_end )
{
    const int b_interlaced = data->b_mbaff;
    const int stride = data->i_stride[0] << b_interlaced;
    const int width = data->i_width[0];
    int start = (mb_y*16 >> b_interlaced) - 8; // buffer = 4 for deblock + 3 for 6tap, rounded to 8
    int height = ((b_end ? data->i_lines[0] : mb_y*16) >> b_interlaced) + 8;
    int offs = start*stride - 8; // buffer = 3 for 6tap, aligned to 8 for simd
    int x, y;

    if( mb_y & b_interlaced )
        return;

    for( y=0; y<=b_interlaced; y++, offs+=frame->i_stride[0] )
    {
        h->mc.hpel_filter(
            frame->filtered[1] + offs,
            frame->filtered[2] + offs,
            frame->filtered[3] + offs,
            frame->plane[0] + offs,
            stride, width + 16, height - start );
    }

    /* generate integral image:
     * frame->integral contains 2 planes. in the upper plane, each element is
     * the sum of an 8x8 pixel region with top-left corner on that point.
     * in the lower plane, 4x4 sums (needed only with --partitions p4x4). */

    if( frame->integral )
    {
        if( start < 0 )
        {
            memset( frame->integral - PADV * stride - PADH, 0, stride * sizeof(uint16_t) );
            start = -PADV;
        }
        if( b_end )
            height += PADV-8;
        for( y = start; y < height; y++ )
        {
            uint8_t  *ref  = frame->plane[0] + y * stride - PADH;
            uint16_t *line = frame->integral + (y+1) * stride - PADH + 1;
            uint16_t v = line[0] = 0;
            for( x = 1; x < stride-1; x++ )
                line[x] = v += ref[x] + line[x-stride] - line[x-stride-1];
            line -= 8*stride;
            if( y >= 9-PADV )
            {
                uint16_t *sum4 = line + stride * (frame->i_lines[0] + PADV*2);
                for( x = 1; x < stride-8; x++, line++, sum4++ )
                {
                    sum4[0] =  line[4+4*stride] - line[4] - line[4*stride] + line[0];
                    line[0] += line[8+8*stride] - line[8] - line[8*stride];
                }
            }
        }
    }
}

#endif

