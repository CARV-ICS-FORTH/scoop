/*****************************************************************************
 * pixel.c: h264 encoder
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
#include <spu_intrinsics.h>
#include "common_tpc.h"
// 


x264_pixel_cmp_t  ssdf[7]={ 	x264_pixel_ssd_16x16, 
				x264_pixel_ssd_16x8,
				x264_pixel_ssd_8x16,
				x264_pixel_ssd_8x8 
			};

/*x264_pixel_cmp_t sa8df[7]={ x264_pixel_sa8d_16x16 };*/

#if 1


x264_pixel_cmp_t fpelcmpf[7]={
			x264_pixel_sad_16x16,
			x264_pixel_sad_16x8,
			x264_pixel_sad_8x16,
			x264_pixel_sad_8x8

}; /* either satd or sad for fullpel motion search */

x264_pixel_cmp_x3_t fpelcmp_x3f[7]={ x264_pixel_sad_x3_16x16 };
x264_pixel_cmp_x4_t fpelcmp_x4f[7]={ x264_pixel_sad_x4_16x16 };

#else



x264_pixel_cmp_x3_t fpelcmp_x3f[7]={ x264_pixel_satd_x3_16x16 };
x264_pixel_cmp_x4_t fpelcmp_x4f[7]={ x264_pixel_satd_x4_16x16 };
#endif



/* multiple parallel calls to cmp. */
x264_pixel_cmp_x3_t sad_x3f[7]  = { x264_pixel_sad_x3_16x16  };
x264_pixel_cmp_x4_t sad_x4f[7]  = { x264_pixel_sad_x4_16x16  };
x264_pixel_cmp_x3_t satd_x3f[7] = { x264_pixel_satd_x3_16x16 };
x264_pixel_cmp_x4_t satd_x4f[7] = { x264_pixel_satd_x4_16x16 };




/****************************************************************************
 * pixel_sad_WxH
 ****************************************************************************/
#define PIXEL_SAD_C( name, lx, ly ) \
int name( uint8_t *pix1, int i_stride_pix1,  \
                 uint8_t *pix2, int i_stride_pix2 ) \
{                                                   \
    int i_sum = 0;                                  \
    int x, y;                                       \
    for( y = 0; y < ly; y++ )                       \
    {                                               \
        for( x = 0; x < lx; x++ )                   \
        {                                           \
            i_sum += abs( pix1[x] - pix2[x] );      \
        }                                           \
        pix1 += i_stride_pix1;                      \
        pix2 += i_stride_pix2;                      \
    }                                               \
    return i_sum;                                   \
}



PIXEL_SAD_C( x264_pixel_sad_4x4  ,  4,  4 )

#ifdef VEC


static inline vector unsigned char load_misaligned_vector_uint8_t (vector unsigned char *ptr)
{
        vector unsigned char  qw0, qw1;
        int shift;
        qw0 = *ptr;
        qw1 = *(ptr+1);
        shift = (unsigned) ptr & 15;
        return spu_or(
                        spu_slqwbyte(qw0, shift),
                        spu_rlmaskqwbyte(qw1, shift-16)
                     );
}


static inline int x264_pixel_sad_16x16( uint8_t *pix1, int i_pix1,
                 uint8_t *pix2, int i_pix2 )
{
        int y,i;
        int sum __attribute__ ((aligned (16)));
        sum=0;

        vector unsigned char *pix1vv, pix2vv;

        vector unsigned short sumv = { 0,0,0,0,0,0,0,0} ;
        vector unsigned char absd;

	/* Unrolled 2 times */
        for( y = 0; y < 16; y+=8 ){

                pix1vv = (vector unsigned char *) pix1;
                pix2vv = load_misaligned_vector_uint8_t((vec_uchar16 *)  pix2); 

                absd = spu_absd((*pix1vv),(pix2vv));
                sumv = spu_add(spu_sumb(absd,absd),sumv);

                pix1 += i_pix1;
                pix2 += i_pix2;

                pix1vv = (vector unsigned char *) pix1;
                pix2vv = load_misaligned_vector_uint8_t((vec_uchar16 *)  pix2); 

                absd = spu_absd((*pix1vv),(pix2vv));
                sumv = spu_add(spu_sumb(absd,absd),sumv);

                pix1 += i_pix1;
                pix2 += i_pix2;

                pix1vv = (vector unsigned char *) pix1;
                pix2vv = load_misaligned_vector_uint8_t((vec_uchar16 *)  pix2); 

                absd = spu_absd((*pix1vv),(pix2vv));
                sumv = spu_add(spu_sumb(absd,absd),sumv);

                pix1 += i_pix1;
                pix2 += i_pix2;

                pix1vv = (vector unsigned char *) pix1;
                pix2vv = load_misaligned_vector_uint8_t((vec_uchar16 *)  pix2); 

                absd = spu_absd((*pix1vv),(pix2vv));
                sumv = spu_add(spu_sumb(absd,absd),  sumv);

                pix1 += i_pix1;
                pix2 += i_pix2;

                pix1vv = (vector unsigned char *) pix1;
                pix2vv = load_misaligned_vector_uint8_t((vec_uchar16 *)  pix2); 

                absd = spu_absd((*pix1vv),(pix2vv));
                sumv = spu_add(spu_sumb(absd,absd),sumv);

                pix1 += i_pix1;
                pix2 += i_pix2;

                pix1vv = (vector unsigned char *) pix1;
                pix2vv = load_misaligned_vector_uint8_t((vec_uchar16 *)  pix2); 

                absd = spu_absd((*pix1vv),(pix2vv));
                sumv = spu_add(spu_sumb(absd,absd),sumv);

                pix1 += i_pix1;
                pix2 += i_pix2;

                pix1vv = (vector unsigned char *) pix1;
                pix2vv = load_misaligned_vector_uint8_t((vec_uchar16 *)  pix2); 

                absd = spu_absd((*pix1vv),(pix2vv));
                sumv = spu_add(spu_sumb(absd,absd),sumv);

                pix1 += i_pix1;
                pix2 += i_pix2;

                pix1vv = (vector unsigned char *) pix1;
                pix2vv = load_misaligned_vector_uint8_t((vec_uchar16 *)  pix2); 

                absd = spu_absd((*pix1vv),(pix2vv));
                sumv = spu_add(spu_sumb(absd,absd),  sumv);

                pix1 += i_pix1;
                pix2 += i_pix2;





        }

        sum+=spu_extract(sumv,0);
        sum+=spu_extract(sumv,2);
        sum+=spu_extract(sumv,4);
        sum+=spu_extract(sumv,6);

        return sum;

}



/// Vec version
inline static int x264_pixel_sad_16x8( uint8_t *pix1, int i_pix1,
                 uint8_t *pix2, int i_pix2 )
{
        int y;
        int sum = 0;

        vec_uchar16 *pix1v, pix2v;
        vec_ushort8 sumv = {0} ;
        vec_uchar16 absdv;

        for( y = 0; y < 8; y++ ){

                pix1v = (vec_uchar16 *) pix1;
                pix2v = load_misaligned_vector_uint8_t( (vec_uchar16 *) pix2); 

                absdv = spu_absd((*pix1v),(pix2v));
                sumv  = spu_sumb(absdv,absdv) + sumv ;

                pix1 += i_pix1;
                pix2 += i_pix2;
        }

        sum+= sumv[0];
        sum+= sumv[2];
        sum+= sumv[4];
        sum+= sumv[6];

        return sum;
}
/// Vec version
inline static int x264_pixel_sad_8x16( uint8_t *pix1, int i_pix1,
                 uint8_t *pix2, int i_pix2 )
{
        int y;
        int sum = 0;

        vec_uchar16 *pix1v, pix2v;
        vec_ushort8 sumv = {0} ;
        vec_uchar16 absdv;


        for( y = 0; y < 16; y++ ){

                pix1v = (vec_uchar16 *) pix1;
                pix2v = load_misaligned_vector_uint8_t( (vec_uchar16 *) pix2); 

                absdv = spu_absd((*pix1v),(pix2v));
                sumv  = spu_sumb(absdv,absdv) + sumv ;

                pix1 += i_pix1;
                pix2 += i_pix2;
        }

        sum+= sumv[0];
        sum+= sumv[2];

        return sum;

}


/// Vec version
int x264_pixel_sad_8x8( uint8_t *pix1, int i_pix1,
                 uint8_t *pix2, int i_pix2 )
{
        int y;
        int sum = 0;

        vec_uchar16 *pix1v, pix2v;
        vec_ushort8 sumv = {0} ;
        vec_uchar16 absdv;


	/* Unrolled 2 times */
        for( y = 0; y < 8; y+=4 ){

                pix1v = (vec_uchar16 *) pix1;
                pix2v = load_misaligned_vector_uint8_t( (vec_uchar16 *) pix2); 

                absdv = spu_absd((*pix1v),(pix2v));
                sumv  = spu_sumb(absdv,absdv) + sumv ;

                pix1 += i_pix1;
                pix2 += i_pix2;

                pix1v = (vec_uchar16 *) pix1;
                pix2v = load_misaligned_vector_uint8_t( (vec_uchar16 *) pix2); 

                absdv = spu_absd((*pix1v),(pix2v));
                sumv  = spu_sumb(absdv,absdv) + sumv ;

                pix1 += i_pix1;
                pix2 += i_pix2;

                pix1v = (vec_uchar16 *) pix1;
                pix2v = load_misaligned_vector_uint8_t( (vec_uchar16 *) pix2); 

                absdv = spu_absd((*pix1v),(pix2v));
                sumv  = spu_sumb(absdv,absdv) + sumv ;

                pix1 += i_pix1;
                pix2 += i_pix2;

                pix1v = (vec_uchar16 *) pix1;
                pix2v = load_misaligned_vector_uint8_t( (vec_uchar16 *) pix2); 

                absdv = spu_absd((*pix1v),(pix2v));
                sumv  = spu_sumb(absdv,absdv) + sumv ;

                pix1 += i_pix1;
                pix2 += i_pix2;
        }


        sum+=spu_extract(sumv,0);
        sum+=spu_extract(sumv,2);


        return sum;

}



#else

PIXEL_SAD_C( x264_pixel_sad_16x16, 16, 16 )
PIXEL_SAD_C( x264_pixel_sad_16x8 , 16,  8 )
PIXEL_SAD_C( x264_pixel_sad_8x16 ,  8, 16 )
PIXEL_SAD_C( x264_pixel_sad_8x8  ,  8,  8 )

#endif

x264_pixel_cmp_t mbcmpf[7]={ 

          x264_pixel_sad_16x16,
          x264_pixel_sad_16x8,
          x264_pixel_sad_8x16,
          x264_pixel_sad_8x8

}; /* either satd or sad for subpel refine and mode decision */


x264_pixel_cmp_t  sadf[7]={
          x264_pixel_sad_16x16,
          x264_pixel_sad_16x8,
          x264_pixel_sad_8x16,
          x264_pixel_sad_8x8
};




/****************************************************************************
 * pixel_ssd_WxH
 ****************************************************************************/

#define PIXEL_SSD_C( name, lx, ly ) \
int name( uint8_t *pix1, int i_stride_pix1,  \
                 uint8_t *pix2, int i_stride_pix2 ) \
{                                                   \
    int i_sum = 0;                                  \
    int x, y;                                       \
    for( y = 0; y < ly; y++ )                       \
    {                                               \
        for( x = 0; x < lx; x++ )                   \
        {                                           \
            int d = pix1[x] - pix2[x];              \
            i_sum += d*d;                           \
        }                                           \
        pix1 += i_stride_pix1;                      \
        pix2 += i_stride_pix2;                      \
    }                                               \
    return i_sum;                                   \
}

PIXEL_SSD_C( x264_pixel_ssd_16x16, 16, 16 );
PIXEL_SSD_C( x264_pixel_ssd_16x8 , 16,  8 );
PIXEL_SSD_C( x264_pixel_ssd_8x16 ,  8, 16 );

#ifdef VEC
int  x264_pixel_ssd_8x8( uint8_t *pix1, int i_stride_pix1,  
                 uint8_t *pix2, int i_stride_pix2 ) 
{                 
    int i_sum = 0;                                  
    int  y;      

    int16_t tmp0[8] __attribute__ ((aligned (16)));
    int16_t tmp1[8] __attribute__ ((aligned (16)));
   
    vec_short8 vec_tmp0, vec_tmp1;
    vec_int4  vec_sum0 ; 
    vec_int4  vec_sum1 ; 
    vec_int4  vec_sum3 = { 0 }; 

    for( y = 0; y < 8; y++ )                       
    {    
 
        tmp0[ 0] = pix1[ 0];
        tmp0[ 1] = pix1[ 1];
        tmp0[ 2] = pix1[ 2];
        tmp0[ 3] = pix1[ 3];
        tmp0[ 4] = pix1[ 4];
        tmp0[ 5] = pix1[ 5];
        tmp0[ 6] = pix1[ 6];
        tmp0[ 7] = pix1[ 7];

        vec_tmp0 = *(vec_short8 *) &tmp0[0];

        tmp1[ 0] = pix2[ 0];
        tmp1[ 1] = pix2[ 1];
        tmp1[ 2] = pix2[ 2];
        tmp1[ 3] = pix2[ 3];
        tmp1[ 4] = pix2[ 4];
        tmp1[ 5] = pix2[ 5];
        tmp1[ 6] = pix2[ 6];
        tmp1[ 7] = pix2[ 7];

        vec_tmp1 = *(vec_short8 *) &tmp1[0];                      
                    
        vec_sum0 = spu_mule(  (vec_tmp0 - vec_tmp1), (vec_tmp0 - vec_tmp1)  )  ;
        vec_sum3 += vec_sum0 ;

        vec_sum1 = spu_mulo(  (vec_tmp0 - vec_tmp1), (vec_tmp0 - vec_tmp1)  )  ;
        vec_sum3 += vec_sum1 ;

        pix1 += i_stride_pix1;                      
        pix2 += i_stride_pix2;                      
    }                            
    i_sum+=spu_extract(vec_sum3,0);
    i_sum+=spu_extract(vec_sum3,1);
    i_sum+=spu_extract(vec_sum3,2);
    i_sum+=spu_extract(vec_sum3,3);                  

    return i_sum;                                   
}


#else


PIXEL_SSD_C( x264_pixel_ssd_8x8  ,  8,  8 );

#endif


int64_t x264_pixel_ssd_wxh(  uint8_t *pix1, int i_pix1, uint8_t *pix2, int i_pix2, int i_width, int i_height )
{
    int64_t i_ssd = 0;
    int x, y;
    int align = !(((long)pix1 | (long)pix2 | i_pix1 | i_pix2) & 15);

#define SSD(size) i_ssd += ssdf[size]( pix1 + y*i_pix1 + x, i_pix1, \
                                          pix2 + y*i_pix2 + x, i_pix2 );
    for( y = 0; y < i_height-15; y += 16 )
    {
        x = 0;
        if( align )
            for( ; x < i_width-15; x += 16 )
                SSD(PIXEL_16x16);
        for( ; x < i_width-7; x += 8 )
            SSD(PIXEL_8x16);
    }
    if( y < i_height-7 )
        for( x = 0; x < i_width-7; x += 8 )
            SSD(PIXEL_8x8);
#undef SSD

#define SSD1 { int d = pix1[y*i_pix1+x] - pix2[y*i_pix2+x]; i_ssd += d*d; }
    if( i_width % 8 != 0 )
    {
        for( y = 0; y < (i_height & ~7); y++ )
            for( x = i_width & ~7; x < i_width; x++ )
                SSD1;
    }
    if( i_height % 8 != 0 )
    {
        for( y = i_height & ~7; y < i_height; y++ )
            for( x = 0; x < i_width; x++ )
                SSD1;
    }
#undef SSD1

    return i_ssd;
}

#define HADAMARD4(d0,d1,d2,d3,s0,s1,s2,s3) {\
    int t0 = s0 + s1;\
    int t1 = s0 - s1;\
    int t2 = s2 + s3;\
    int t3 = s2 - s3;\
    d0 = t0 + t2;\
    d2 = t0 - t2;\
    d1 = t1 + t3;\
    d3 = t1 - t3;\
}


/****************************************************************************
 * pixel_sa8d_WxH: sum of 8x8 Hadamard transformed differences
 ****************************************************************************/
#define SA8D_1D {\
    int b0,b1,b2,b3,b4,b5,b6,b7;\
    HADAMARD4( b0,b1,b2,b3, SRC(0), SRC(1), SRC(2), SRC(3) );\
    HADAMARD4( b4,b5,b6,b7, SRC(4), SRC(5), SRC(6), SRC(7) );\
    DST(0, b0 + b4);\
    DST(4, b0 - b4);\
    DST(1, b1 + b5);\
    DST(5, b1 - b5);\
    DST(2, b2 + b6);\
    DST(6, b2 - b6);\
    DST(3, b3 + b7);\
    DST(7, b3 - b7);\
}



/****************************************************************************
 * pixel_sad_x4
 ****************************************************************************/
#define SAD_X( size ) \
void x264_pixel_sad_x3_##size( uint8_t *fenc, uint8_t *pix0, uint8_t *pix1, uint8_t *pix2, int i_stride, int scores[3] )\
{\
    scores[0] = x264_pixel_sad_##size( fenc, FENC_STRIDE, pix0, i_stride );\
    scores[1] = x264_pixel_sad_##size( fenc, FENC_STRIDE, pix1, i_stride );\
    scores[2] = x264_pixel_sad_##size( fenc, FENC_STRIDE, pix2, i_stride );\
}\
void x264_pixel_sad_x4_##size( uint8_t *fenc, uint8_t *pix0, uint8_t *pix1, uint8_t *pix2, uint8_t *pix3, int i_stride, int scores[4] )\
{\
    scores[0] = x264_pixel_sad_##size( fenc, FENC_STRIDE, pix0, i_stride );\
    scores[1] = x264_pixel_sad_##size( fenc, FENC_STRIDE, pix1, i_stride );\
    scores[2] = x264_pixel_sad_##size( fenc, FENC_STRIDE, pix2, i_stride );\
    scores[3] = x264_pixel_sad_##size( fenc, FENC_STRIDE, pix3, i_stride );\
}

SAD_X( 16x16 )




