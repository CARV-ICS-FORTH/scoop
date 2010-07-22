/*****************************************************************************
 * frame.c: h264 encoder library
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

#ifdef DEBLOCK

#include "common_tpc.h"

#define ALIGN(x,a) (((x)+((a)-1))&~((a)-1))



/* Deblocking filter */
static const uint8_t i_alpha_table[52+12*2] =
{
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  4,  4,  5,  6,
     7,  8,  9, 10, 12, 13, 15, 17, 20, 22,
    25, 28, 32, 36, 40, 45, 50, 56, 63, 71,
    80, 90,101,113,127,144,162,182,203,226,
   255,255,
   255,255,255,255,255,255,255,255,255,255,255,255,
};
static const uint8_t i_beta_table[52+12*2] =
{
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  2,  2,  2,  3,
     3,  3,  3,  4,  4,  4,  6,  6,  7,  7,
     8,  8,  9,  9, 10, 10, 11, 11, 12, 12,
    13, 13, 14, 14, 15, 15, 16, 16, 17, 17,
    18, 18,
    18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18,
};
static const int8_t i_tc0_table[52+12*2][4] =
{
    {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 },
    {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 },
    {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 },
    {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 },
    {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 0 }, {-1, 0, 0, 1 },
    {-1, 0, 0, 1 }, {-1, 0, 0, 1 }, {-1, 0, 0, 1 }, {-1, 0, 1, 1 }, {-1, 0, 1, 1 }, {-1, 1, 1, 1 },
    {-1, 1, 1, 1 }, {-1, 1, 1, 1 }, {-1, 1, 1, 1 }, {-1, 1, 1, 2 }, {-1, 1, 1, 2 }, {-1, 1, 1, 2 },
    {-1, 1, 1, 2 }, {-1, 1, 2, 3 }, {-1, 1, 2, 3 }, {-1, 2, 2, 3 }, {-1, 2, 2, 4 }, {-1, 2, 3, 4 },
    {-1, 2, 3, 4 }, {-1, 3, 3, 5 }, {-1, 3, 4, 6 }, {-1, 3, 4, 6 }, {-1, 4, 5, 7 }, {-1, 4, 5, 8 },
    {-1, 4, 6, 9 }, {-1, 5, 7,10 }, {-1, 6, 8,11 }, {-1, 6, 8,13 }, {-1, 7,10,14 }, {-1, 8,11,16 },
    {-1, 9,12,18 }, {-1,10,13,20 }, {-1,11,15,23 }, {-1,13,17,25 },
    {-1,13,17,25 }, {-1,13,17,25 }, {-1,13,17,25 }, {-1,13,17,25 }, {-1,13,17,25 }, {-1,13,17,25 },
    {-1,13,17,25 }, {-1,13,17,25 }, {-1,13,17,25 }, {-1,13,17,25 }, {-1,13,17,25 }, {-1,13,17,25 },
};
#define alpha_table(x) i_alpha_table[(x)+12]
#define beta_table(x)  i_beta_table[(x)+12]
#define tc0_table(x)   i_tc0_table[(x)+12]



/* From ffmpeg */
void deblock_luma_c( uint8_t *pix, int xstride, int ystride, int alpha, int beta, int8_t *tc0 )
{
    int i, d;

    for( i = 0; i < 4; i++ )
    {
        if( tc0[i] < 0 )
        {
            pix += 4*ystride;
            continue;
        }
        for( d = 0; d < 4; d++ )
        {
            const int p2 = pix[-3*xstride];
            const int p1 = pix[-2*xstride];
            const int p0 = pix[-1*xstride];
            const int q0 = pix[ 0*xstride];
            const int q1 = pix[ 1*xstride];
            const int q2 = pix[ 2*xstride];


            if( abs( p0 - q0 ) < alpha && abs( p1 - p0 ) < beta && abs( q1 - q0 ) < beta )
            {
                int tc = tc0[i];
                int delta;
                if( abs( p2 - p0 ) < beta )
                {
                    pix[-2*xstride] = p1 + x264_clip3( (( p2 + ((p0 + q0 + 1) >> 1)) >> 1) - p1, -tc0[i], tc0[i] );
                    tc++;
                }
                if( abs( q2 - q0 ) < beta )
                {
                    pix[ 1*xstride] = q1 + x264_clip3( (( q2 + ((p0 + q0 + 1) >> 1)) >> 1) - q1, -tc0[i], tc0[i] );
                    tc++;
                }

                delta = x264_clip3( (((q0 - p0 ) << 2) + (p1 - q1) + 4) >> 3, -tc, tc );
                pix[-1*xstride] = x264_clip_uint8( p0 + delta );    /* p0' */
                pix[ 0*xstride] = x264_clip_uint8( q0 - delta );    /* q0' */
            }
            pix += ystride;
        }
    }
}
void deblock_chroma_c( uint8_t *pix, int xstride, int ystride, int alpha, int beta, int8_t *tc0 )
{
    int i, d;
    for( i = 0; i < 4; i++ )
    {
        const int tc = tc0[i];
        if( tc <= 0 )
        {
            pix += 2*ystride;
            continue;
        }
        for( d = 0; d < 2; d++ )
        {
            const int p1 = pix[-2*xstride];
            const int p0 = pix[-1*xstride];
            const int q0 = pix[ 0*xstride];
            const int q1 = pix[ 1*xstride];

            if( abs( p0 - q0 ) < alpha && abs( p1 - p0 ) < beta && abs( q1 - q0 ) < beta )
            {
                int delta = x264_clip3( (((q0 - p0 ) << 2) + (p1 - q1) + 4) >> 3, -tc, tc );
                pix[-1*xstride] = x264_clip_uint8( p0 + delta );    /* p0' */
                pix[ 0*xstride] = x264_clip_uint8( q0 - delta );    /* q0' */
            }
            pix += ystride;
        }
    }
}
void deblock_luma_intra_c( uint8_t *pix, int xstride, int ystride, int alpha, int beta )
{
    int d;
    for( d = 0; d < 16; d++ )
    {
        const int p2 = pix[-3*xstride];
        const int p1 = pix[-2*xstride];
        const int p0 = pix[-1*xstride];
        const int q0 = pix[ 0*xstride];
        const int q1 = pix[ 1*xstride];
        const int q2 = pix[ 2*xstride];

        if( abs( p0 - q0 ) < alpha && abs( p1 - p0 ) < beta && abs( q1 - q0 ) < beta )
        {
            if(abs( p0 - q0 ) < ((alpha >> 2) + 2) )
            {
                if( abs( p2 - p0 ) < beta ) /* p0', p1', p2' */
                {
                    const int p3 = pix[-4*xstride];
                    pix[-1*xstride] = ( p2 + 2*p1 + 2*p0 + 2*q0 + q1 + 4 ) >> 3;
                    pix[-2*xstride] = ( p2 + p1 + p0 + q0 + 2 ) >> 2;
                    pix[-3*xstride] = ( 2*p3 + 3*p2 + p1 + p0 + q0 + 4 ) >> 3;
                }
                else /* p0' */
                    pix[-1*xstride] = ( 2*p1 + p0 + q1 + 2 ) >> 2;
                if( abs( q2 - q0 ) < beta ) /* q0', q1', q2' */
                {
                    const int q3 = pix[3*xstride];
                    pix[0*xstride] = ( p1 + 2*p0 + 2*q0 + 2*q1 + q2 + 4 ) >> 3;
                    pix[1*xstride] = ( p0 + q0 + q1 + q2 + 2 ) >> 2;
                    pix[2*xstride] = ( 2*q3 + 3*q2 + q1 + q0 + p0 + 4 ) >> 3;
                }
                else /* q0' */
                    pix[0*xstride] = ( 2*q1 + q0 + p1 + 2 ) >> 2;
            }
            else /* p0', q0' */
            {
                pix[-1*xstride] = ( 2*p1 + p0 + q1 + 2 ) >> 2;
                pix[ 0*xstride] = ( 2*q1 + q0 + p1 + 2 ) >> 2;
            }
        }
        pix += ystride;
    }
}


void deblock_chroma_intra_c( uint8_t *pix, int xstride, int ystride, int alpha, int beta )
{
    int d;
    for( d = 0; d < 8; d++ )
    {
        const int p1 = pix[-2*xstride];
        const int p0 = pix[-1*xstride];
        const int q0 = pix[ 0*xstride];
        const int q1 = pix[ 1*xstride];

        if( abs( p0 - q0 ) < alpha && abs( p1 - p0 ) < beta && abs( q1 - q0 ) < beta )
        {
            pix[-1*xstride] = (2*p1 + p0 + q1 + 2) >> 2;   /* p0' */
            pix[ 0*xstride] = (2*q1 + q0 + p1 + 2) >> 2;   /* q0' */
        }
        pix += ystride;
    }
}

static void deblock_v_luma_c( uint8_t *pix, int stride, int alpha, int beta, int8_t *tc0 )
{
    deblock_luma_c( pix, stride, 1, alpha, beta, tc0 );

}
static void deblock_h_luma_c( uint8_t *pix, int stride, int alpha, int beta, int8_t *tc0 )
{
    deblock_luma_c( pix, 1, stride, alpha, beta, tc0 );
}
static void deblock_v_chroma_c( uint8_t *pix, int stride, int alpha, int beta, int8_t *tc0 )
{
    deblock_chroma_c( pix, stride, 1, alpha, beta, tc0 );
}
static void deblock_h_chroma_c( uint8_t *pix, int stride, int alpha, int beta, int8_t *tc0 )
{
    deblock_chroma_c( pix, 1, stride, alpha, beta, tc0 );
}


static inline void deblock_edge_h_luma( x264_filter_data_t *data, uint8_t *pix1, uint8_t *pix2, int i_stride, uint8_t bS[4], int i_qp, int b_chroma )
{
    const int index_a = i_qp + data->i_alpha_c0_offset;
    const int alpha = alpha_table(index_a);
    const int beta  = beta_table(i_qp + data->i_beta_offset);
    int8_t tc[4];

    if( !alpha || !beta )
        return;

    tc[0] = tc0_table(index_a)[bS[0]] + b_chroma;
    tc[1] = tc0_table(index_a)[bS[1]] + b_chroma;
    tc[2] = tc0_table(index_a)[bS[2]] + b_chroma;
    tc[3] = tc0_table(index_a)[bS[3]] + b_chroma;

    deblock_h_luma_c( pix1, i_stride, alpha, beta, tc );
    if( b_chroma )
        deblock_h_luma_c( pix2, i_stride, alpha, beta, tc );
}


static inline void deblock_edge_v_luma( x264_filter_data_t *data, uint8_t *pix1, uint8_t *pix2, int i_stride, uint8_t bS[4], int i_qp, int b_chroma )
{
    const int index_a = i_qp + data->i_alpha_c0_offset;
    const int alpha = alpha_table(index_a);
    const int beta  = beta_table(i_qp + data->i_beta_offset);

    int8_t tc[4];

    if( !alpha || !beta )
        return;

    tc[0] = tc0_table(index_a)[bS[0]] + b_chroma;
    tc[1] = tc0_table(index_a)[bS[1]] + b_chroma;
    tc[2] = tc0_table(index_a)[bS[2]] + b_chroma;
    tc[3] = tc0_table(index_a)[bS[3]] + b_chroma;

    deblock_v_luma_c( pix1, i_stride, alpha, beta, tc );
    if( b_chroma )
        deblock_v_luma_c( pix2, i_stride, alpha, beta, tc );
}

static inline void deblock_edge_v_chroma( x264_filter_data_t *data, uint8_t *pix1, uint8_t *pix2, int i_stride, uint8_t bS[4], int i_qp, int b_chroma )
{
    const int index_a = i_qp + data->i_alpha_c0_offset;
    const int alpha = alpha_table(index_a);
    const int beta  = beta_table(i_qp + data->i_beta_offset);

    int8_t tc[4];

    if( !alpha || !beta )
        return;

    tc[0] = tc0_table(index_a)[bS[0]] + b_chroma;
    tc[1] = tc0_table(index_a)[bS[1]] + b_chroma;
    tc[2] = tc0_table(index_a)[bS[2]] + b_chroma;
    tc[3] = tc0_table(index_a)[bS[3]] + b_chroma;

    deblock_v_chroma_c( pix1, i_stride, alpha, beta, tc );
    if( b_chroma )
        deblock_v_chroma_c( pix2, i_stride, alpha, beta, tc );
}

static inline void deblock_edge_h_chroma( x264_filter_data_t *data, uint8_t *pix1, uint8_t *pix2, int i_stride, uint8_t bS[4], int i_qp, int b_chroma )
{
    const int index_a = i_qp + data->i_alpha_c0_offset;
    const int alpha = alpha_table(index_a);
    const int beta  = beta_table(i_qp + data->i_beta_offset);

    int8_t tc[4];

    if( !alpha || !beta )
        return;

    tc[0] = tc0_table(index_a)[bS[0]] + b_chroma;
    tc[1] = tc0_table(index_a)[bS[1]] + b_chroma;
    tc[2] = tc0_table(index_a)[bS[2]] + b_chroma;
    tc[3] = tc0_table(index_a)[bS[3]] + b_chroma;

    deblock_h_chroma_c( pix1, i_stride, alpha, beta, tc );
    if( b_chroma )
        deblock_h_chroma_c( pix2, i_stride, alpha, beta, tc );
}




/* pixy */
void x264_frame_deblock_row_tpc_luma(   x264_filter_data_t  *data,  uint8_t *pix )
{
    const int s8x8 = 2 * data->i_mb_stride;
    const int s4x4 = 4 * data->i_mb_stride;
    const int b_interlaced = data->b_mbaff;
    const int mvy_limit = 4 >> b_interlaced;
    DECLARE_ALIGNED_4( uint8_t bS[4] ) = {2,2,2,2} ;

    const int qp_thresh = 15 - X264_MIN(data->i_alpha_c0_offset , data->i_beta_offset) - X264_MAX(0, data->i_chroma_qp_offset);
    const int mb_y = data->mb_y;
    int mb_x;
    int stridey   = data->i_stride[0];
    int stride2y  = stridey << b_interlaced;

    /* shift pix pointer */
  //  pix += data->offset;
     pix += 1*stridey;
     for( mb_x = 0; mb_x <data->i_mb_width; mb_x += (~b_interlaced | mb_y)&1 )
     {


	  if (mb_x<data->i_mb_width-10)   {
		     const int mb_xy  = mb_y * data->i_mb_stride + mb_x;
		     const int mb_8x8 = 2 * s8x8 * mb_y + 2 * mb_x;
		     const int mb_4x4 = 4 * s4x4 * mb_y + 4 * mb_x;
		     const int i_qp = data->i_qp;
		     int i_edge_end = 0;
		     pix = pix  + 16*mb_x;
		     if( i_qp <= qp_thresh )
			     i_edge_end = 1;
		     {
			     int i_edge = (mb_y <= b_interlaced) ;
			     int i_qpn, i, l, mbn_xy, mbn_8x8, mbn_4x4;
			     /* filtering strength */
			     if( !i_edge )
			     {
				     mbn_xy  =  mb_xy  - 1 ;
				     mbn_8x8 =  mb_8x8 - 2 ;
				     mbn_4x4 =  mb_4x4 - 4 ;



				     i_qpn= data->i_qp;

				     /* vertical edge */
				     deblock_edge_h_luma( data, pix + 4*i_edge, NULL,
						     stride2y, bS, (i_qp+i_qpn+1) >> 1, 0
						     );
				     i_edge += 1;
			     }

			     mbn_xy  = mb_xy;
			     mbn_8x8 = mb_8x8;
			     mbn_4x4 = mb_4x4;
			     for( ; i_edge < i_edge_end; i_edge+=1 )
			     {

				     /* Y plane */
				     i_qpn= data->i_qp;
				     /* vertical edge */
				     deblock_edge_h_luma( data, pix + 4*i_edge, NULL,
						     stride2y, bS, (i_qp+i_qpn+1) >> 1, 0
						     );
			     }
		     }
		     {
			     int i_edge = (mb_y <= b_interlaced) ;
			     int i_qpn, i, l, mbn_xy, mbn_8x8, mbn_4x4;
			     if( !i_edge  )
			     {
				     mbn_xy  =  mb_xy - data->i_mb_stride;
				     mbn_8x8 =  mb_8x8 - 2 * s8x8;
				     mbn_4x4 =  mb_4x4 - 4 * s4x4;
				     {
					     /* Y plane */\
						     i_qpn= data->i_qp;

					     /* horizontal edge */
					     deblock_edge_v_luma(data, pix + 4*i_edge*stride2y, NULL,
							     stride2y, bS, (i_qp+i_qpn+1) >> 1, 0
							     );
				     }
				     i_edge += 1;
			     }
			     mbn_xy  = mb_xy;
			     mbn_8x8 = mb_8x8;
			     mbn_4x4 = mb_4x4;
			     for( ; i_edge < i_edge_end; i_edge+=1 )
			     {
				     /* Y plane */
				     i_qpn= data->i_qp;
				     {
					     /* horizontal edge */
					     deblock_edge_v_luma ( data, pix + 4*i_edge*stride2y, NULL,
							     stride2y, bS, (i_qp+i_qpn+1) >> 1, 0
							     );
				     }
			     }
		     }
	     }
     }
}

/* pixuv */

void x264_macroblock_call_deblock_chroma( x264_filter_data_t *data, uint8_t *pixu, uint8_t *pixv )
{

    const int s8x8 = 2 * data->i_mb_stride;
    const int s4x4 = 4 * data->i_mb_stride;
    const int b_interlaced = data->b_mbaff;
    const int mvy_limit = 4 >> b_interlaced;


    const int mb_y = data->mb_y;
    const int qp_thresh = 15 - X264_MIN(data->i_alpha_c0_offset , data->i_beta_offset) - X264_MAX(0, data->i_chroma_qp_offset);
    int mb_x;
    int stridey   = data->i_stride[1];
    int stride2uv  = stridey << b_interlaced;
    pixu += stridey*3;
    pixv += stridey*3;
 
     DECLARE_ALIGNED_4( uint8_t bS[4] ) = { 2, 2, 2, 2 };  /* filtering strength */

    for( mb_x = 0; mb_x < data->i_mb_width; mb_x += (~b_interlaced | mb_y)&1 )
    {
        const int mb_xy  = mb_y * data->i_mb_stride + mb_x;
        const int mb_8x8 = 2 * s8x8 * mb_y + 2 * mb_x;
        const int mb_4x4 = 4 * s4x4 * mb_y + 4 * mb_x;
        const int i_qp = data->i_qp;
        int i_edge_end = 1;

        //  int i_edge_end = (mb->type[mb_xy] == P_SKIP) ? 1 : 4;
        pixu = pixu  +  8*mb_x;
        pixv = pixv  +  8*mb_x;

        if( i_qp <= qp_thresh )
            i_edge_end = 1;



        {
            int i_edge = (mb_y <= b_interlaced) ;
            int i_qpn, i, l, mbn_xy, mbn_8x8, mbn_4x4;

            {
                mbn_xy  =  mb_xy  - 1 ;
                mbn_8x8 =  mb_8x8 - 2 ;
                mbn_4x4 =  mb_4x4 - 4 ;

                {

                    /* Y plane */
                    i_qpn= data->i_qp;

                    {
                        /* U/V planes */
                        int i_qpc = data->i_qp >> 1;
                        deblock_edge_h_chroma( data, pixu + 2*i_edge, pixv + 2*i_edge,
                                stride2uv, bS, i_qpc, 1
                                );


                    }

                }
                i_edge += 1;
            }
            mbn_xy  = mb_xy;
            mbn_8x8 = mb_8x8;
            mbn_4x4 = mb_4x4;
            for( ; i_edge < i_edge_end; i_edge+=1 )
            {



                {
                    /* Y plane */
                    i_qpn= data->i_qp;
                    if( !(i_edge & 1) )
                    {
                        /* U/V planes */
                        int i_qpc = data->i_qp;
                        deblock_edge_h_chroma( data, pixu + 2*i_edge, pixv + 2*i_edge,
                                stride2uv, bS, i_qpc, 1
                                );

                    }
                }
            }

        }


        {
            int i_edge = (mb_y <= b_interlaced) ;
            int i_qpn, i, l, mbn_xy, mbn_8x8, mbn_4x4;

            {



        

	        {
	            /* Y plane */\
        	    i_qpn= data->i_qp;


	                /* U/V planes */
	                if( !(i_edge & 1) )
	                {
        	            int i_qpc = data->i_qp;

			   deblock_edge_v_chroma(
					data, pixu + 2*i_edge*stride2uv, pixv + 2*i_edge*stride2uv,
                                    stride2uv, bS, i_qpc, 1
                                    );


                        }
                }


                i_edge += 1;
            }
            mbn_xy  = mb_xy;
            mbn_8x8 = mb_8x8;
            mbn_4x4 = mb_4x4;
            for( ; i_edge < i_edge_end; i_edge+=1 )
            {
               {
                    /* Y plane */
                    i_qpn= data->i_qp;
                    {

                        /* U/V planes */
                        if( !(i_edge & 1) )
                        {
                            int i_qpc = data->i_qp;
				deblock_edge_v_chroma( data, pixu + 2*i_edge*stride2uv, pixv + 2*i_edge*stride2uv,
                                    stride2uv, bS, i_qpc, 1);

                        }
                    }
                }   	


            }
        }

    }


}


#endif

