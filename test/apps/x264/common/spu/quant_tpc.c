/*****************************************************************************
 * quant.c: h264 encoder library
 *****************************************************************************
 * Copyright (C) 2005-2008 x264 project
 *
 * Authors: Loren Merritt <lorenm@u.washington.edu>
 *          Christian Heine <sennindemokrit@gmx.net>
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

#include <spu_intrinsics.h>
#include <vec_types.h> ///TODO: Do we need ?
#include <spu_internals.h>





#define QUANT_ONE( coef, mf, f ) \
{ \
    if( (coef) > 0 ) \
        (coef) = (f + (coef)) * (mf) >> 16; \
    else \
        (coef) = - ((f - (coef)) * (mf) >> 16); \
}


#ifdef VEC
void quant_4x4( int16_t dct[4][4], uint16_t mf[16], uint16_t bias[16] )
{
    int i;
    int16_t tmp_dct1[4][4];
    int16_t tmp_dct0[4][4];
    vector signed short *tmp0;
    vector signed short *tmp1;
    vector signed short *tmp3;
    vector signed short *tmp6;
    vector signed short *tmp7;
    vector signed short tmp4;
    vector signed short tmp5;
    vector signed int tmp_int0;
    vector signed int tmp_int1;

    tmp0 = ( vector signed short *) &bias[0] ;
    tmp1 = ( vector signed short *) &dct[0][0];
    tmp3 = ( vector signed short *) &tmp_dct0[0][0]; 

    *tmp3 =  *tmp1  + *tmp0 ;

    tmp_dct0[0][0]  =  ( tmp_dct0[0][0]   *  mf[0])  >> 16; 
    tmp_dct0[0][1]  =  ( tmp_dct0[0][1]   *  mf[1])  >> 16; 
    tmp_dct0[0][2]  =  ( tmp_dct0[0][2]   *  mf[2])  >> 16; 
    tmp_dct0[0][3]  =  ( tmp_dct0[0][3]   *  mf[3])  >> 16; 
    tmp_dct0[0][4]  =  ( tmp_dct0[0][4]   *  mf[4])  >> 16; 
    tmp_dct0[0][5]  =  ( tmp_dct0[0][5]   *  mf[5])  >> 16; 
    tmp_dct0[0][6]  =  ( tmp_dct0[0][6]   *  mf[6])  >> 16; 
    tmp_dct0[0][7]  =  ( tmp_dct0[0][7]   *  mf[7])  >> 16;

    tmp0 = ( vector signed short *) &bias[8] ;
    tmp1 = ( vector signed short *) &dct[0][8];
    tmp3 = ( vector signed short *) &tmp_dct0[0][8]; 

    *tmp3 =  *tmp1 + *tmp0 ;

    tmp_dct0[0][ 8]  =   tmp_dct0[0][ 8]   *  mf[ 8]  >> 16; 
    tmp_dct0[0][ 9]  =   tmp_dct0[0][ 9]   *  mf[ 9]  >> 16; 
    tmp_dct0[0][10]  =   tmp_dct0[0][10]   *  mf[10]  >> 16; 
    tmp_dct0[0][11]  =   tmp_dct0[0][11]   *  mf[11]  >> 16; 
    tmp_dct0[0][12]  =   tmp_dct0[0][12]   *  mf[12]  >> 16; 
    tmp_dct0[0][13]  =   tmp_dct0[0][13]   *  mf[13]  >> 16; 
    tmp_dct0[0][14]  =   tmp_dct0[0][14]   *  mf[14]  >> 16; 
    tmp_dct0[0][15]  =   tmp_dct0[0][15]   *  mf[15]  >> 16;




 
    tmp0 = ( vector signed short *) &bias[0] ;
    tmp1 = ( vector signed short *) &dct[0][0];
    tmp3 = ( vector signed short *) &tmp_dct1[0][0]; 

    *tmp3 =  *tmp0 -  *tmp1 ;

    tmp_dct1[0][ 0]  = - ( tmp_dct1[0][ 0]  *  mf[ 0]  >> 16 ); 
    tmp_dct1[0][ 1]  = - ( tmp_dct1[0][ 1]  *  mf[ 1]  >> 16 ); 
    tmp_dct1[0][ 2]  = - ( tmp_dct1[0][ 2]  *  mf[ 2]  >> 16 ); 
    tmp_dct1[0][ 3]  = - ( tmp_dct1[0][ 3]  *  mf[ 3]  >> 16 ); 
    tmp_dct1[0][ 4]  = - ( tmp_dct1[0][ 4]  *  mf[ 4]  >> 16 ); 
    tmp_dct1[0][ 5]  = - ( tmp_dct1[0][ 5]  *  mf[ 5]  >> 16 ); 
    tmp_dct1[0][ 6]  = - ( tmp_dct1[0][ 6]  *  mf[ 6]  >> 16 ); 
    tmp_dct1[0][ 7]  = - ( tmp_dct1[0][ 7]  *  mf[ 7]  >> 16 ); 

    tmp0 = ( vector signed short *) &bias[8] ;
    tmp1 = ( vector signed short *) &dct[0][8];
    tmp3 = ( vector signed short *) &tmp_dct1[0][8]; 

    *tmp3 =  *tmp0 - *tmp1 ;


    tmp_dct1[0][ 8]  = - ( tmp_dct1[0][ 8]  *  mf[ 8]  >> 16 ); 
    tmp_dct1[0][ 9]  = - ( tmp_dct1[0][ 9]  *  mf[ 9]  >> 16 ); 
    tmp_dct1[0][10]  = - ( tmp_dct1[0][10]  *  mf[10]  >> 16 ); 
    tmp_dct1[0][11]  = - ( tmp_dct1[0][11]  *  mf[11]  >> 16 ); 
    tmp_dct1[0][12]  = - ( tmp_dct1[0][12]  *  mf[12]  >> 16 ); 
    tmp_dct1[0][13]  = - ( tmp_dct1[0][13]  *  mf[13]  >> 16 ); 
    tmp_dct1[0][14]  = - ( tmp_dct1[0][14]  *  mf[14]  >> 16 ); 
    tmp_dct1[0][15]  = - ( tmp_dct1[0][15]  *  mf[15]  >> 16 ); 

    uint16_t result;


    tmp0 = ( vector signed short *) &dct[0][0] ;
    tmp1 = ( vector signed short *) &tmp_dct0[0][0];
    tmp3 = ( vector signed short *) &tmp_dct1[0][0];
    const vector unsigned short vec_15 = spu_splats( (uint16_t) 15);
    vector signed short result2 =  (*tmp0) >> vec_15;  
    *tmp0 =   (*tmp1 & ~result2) | (*tmp3 & result2);

 
    tmp0 = ( vector signed short *) &dct[0][8] ;
    tmp1 = ( vector signed short *) &tmp_dct0[0][8];
    tmp3 = ( vector signed short *) &tmp_dct1[0][8];
    result2 =  (*tmp0) >> vec_15;  
    *tmp0 =   (*tmp1 & ~result2) | (*tmp3 & result2);




}


 

#else

void quant_4x4( int16_t dct[4][4], uint16_t mf[16], uint16_t bias[16] )
{
    int i;
    for( i = 0; i < 16; i++ )
        QUANT_ONE( dct[0][i], mf[i], bias[i] );
}



#endif



void quant_2x2_dc( int16_t dct[2][2], int mf, int bias )
{
    QUANT_ONE( dct[0][0], mf, bias );
    QUANT_ONE( dct[0][1], mf, bias );
    QUANT_ONE( dct[0][2], mf, bias );
    QUANT_ONE( dct[0][3], mf, bias );
}

#define DEQUANT_SHL( x ) \
    dct[y][x] = ( dct[y][x] * dequant_mf[i_mf][y][x] ) << i_qbits

#define DEQUANT_SHR( x ) \
    dct[y][x] = ( dct[y][x] * dequant_mf[i_mf][y][x] + f ) >> (-i_qbits)

#ifdef VEC

void dequant_4x4( int16_t dct[4][4], int dequant_mf[6][4][4], int i_qp )
{
    const int i_mf = i_qp%6;
    const int i_qbits = i_qp/6 - 4;

#if 0


    short int dct_int_a[4][4];
    short int dct_int_b[4][4];

    {
        const int i_dmf = dequant_mf[i_qp%6][0][0] << i_qbits;


	    dct_int_a[0][0] = ( dct[0][0] * dequant_mf[i_mf][0][0] ) << i_qbits;
	    dct_int_a[0][1] = ( dct[0][1] * dequant_mf[i_mf][0][1] ) << i_qbits;
	    dct_int_a[0][2] = ( dct[0][2] * dequant_mf[i_mf][0][2] ) << i_qbits;
	    dct_int_a[0][3] = ( dct[0][3] * dequant_mf[i_mf][0][3] ) << i_qbits;

	    dct_int_a[1][0] = ( dct[1][0] * dequant_mf[i_mf][1][0] ) << i_qbits;
	    dct_int_a[1][1] = ( dct[1][1] * dequant_mf[i_mf][1][1] ) << i_qbits;
	    dct_int_a[1][2] = ( dct[1][2] * dequant_mf[i_mf][1][2] ) << i_qbits;
	    dct_int_a[1][3] = ( dct[1][3] * dequant_mf[i_mf][1][3] ) << i_qbits;

	    dct_int_a[2][0] = ( dct[2][0] * dequant_mf[i_mf][2][0] ) << i_qbits;
	    dct_int_a[2][1] = ( dct[2][1] * dequant_mf[i_mf][2][1] ) << i_qbits;
	    dct_int_a[2][2] = ( dct[2][2] * dequant_mf[i_mf][2][2] ) << i_qbits;
	    dct_int_a[2][3] = ( dct[2][3] * dequant_mf[i_mf][2][3] ) << i_qbits;

	    dct_int_a[3][0] = ( dct[3][0] * dequant_mf[i_mf][3][0] ) << i_qbits;
	    dct_int_a[3][1] = ( dct[3][1] * dequant_mf[i_mf][3][1] ) << i_qbits;
	    dct_int_a[3][2] = ( dct[3][2] * dequant_mf[i_mf][3][2] ) << i_qbits;
	    dct_int_a[3][3] = ( dct[3][3] * dequant_mf[i_mf][3][3] ) << i_qbits;


    }

    {

        const int f = 1 << (-i_qbits-1);

	dct_int_b[0][0] = ( dct[0][0] * dequant_mf[i_mf][0][0] + f ) >> (-i_qbits);
	dct_int_b[0][1] = ( dct[0][1] * dequant_mf[i_mf][0][1] + f ) >> (-i_qbits);
	dct_int_b[0][2] = ( dct[0][2] * dequant_mf[i_mf][0][2] + f ) >> (-i_qbits);
	dct_int_b[0][3] = ( dct[0][3] * dequant_mf[i_mf][0][3] + f ) >> (-i_qbits);

	dct_int_b[1][0] = ( dct[1][0] * dequant_mf[i_mf][1][0] + f ) >> (-i_qbits);
	dct_int_b[1][1] = ( dct[1][1] * dequant_mf[i_mf][1][1] + f ) >> (-i_qbits);
	dct_int_b[1][2] = ( dct[1][2] * dequant_mf[i_mf][1][2] + f ) >> (-i_qbits);
	dct_int_b[1][3] = ( dct[1][3] * dequant_mf[i_mf][1][3] + f ) >> (-i_qbits);


	dct_int_b[2][0] = ( dct[2][0] * dequant_mf[i_mf][2][0] + f ) >> (-i_qbits);
	dct_int_b[2][1] = ( dct[2][1] * dequant_mf[i_mf][2][1] + f ) >> (-i_qbits);
	dct_int_b[2][2] = ( dct[2][2] * dequant_mf[i_mf][2][2] + f ) >> (-i_qbits);
	dct_int_b[2][3] = ( dct[2][3] * dequant_mf[i_mf][2][3] + f ) >> (-i_qbits);

	dct_int_b[3][0] = ( dct[3][0] * dequant_mf[i_mf][3][0] + f ) >> (-i_qbits);
	dct_int_b[3][1] = ( dct[3][1] * dequant_mf[i_mf][3][1] + f ) >> (-i_qbits);
	dct_int_b[3][2] = ( dct[3][2] * dequant_mf[i_mf][3][2] + f ) >> (-i_qbits);
	dct_int_b[3][3] = ( dct[3][3] * dequant_mf[i_mf][3][3] + f ) >> (-i_qbits);

    }
    uint16_t result;
    int i,j;
    result = ( i_qbits >> 15 );

    for ( i=0; i<4; i++ )
       for ( j=0; j<4; j++)
       dct[i][j] = ( dct_int_a[i][j]  & ~result  ) || (  dct_int_b[i][j] & result );

#else
    int dct_int[4][4];
    dct_int[0][0] = dct[0][0];
    dct_int[0][1] = dct[0][1];
    dct_int[0][2] = dct[0][2];
    dct_int[0][3] = dct[0][3];
    dct_int[1][0] = dct[1][0];
    dct_int[1][1] = dct[1][1];
    dct_int[1][2] = dct[1][2];
    dct_int[1][3] = dct[1][3];
    dct_int[2][0] = dct[2][0];
    dct_int[2][1] = dct[2][1];
    dct_int[2][2] = dct[2][2];
    dct_int[2][3] = dct[2][3];
    dct_int[3][0] = dct[3][0];
    dct_int[3][1] = dct[3][1];
    dct_int[3][2] = dct[3][2];
    dct_int[3][3] = dct[3][3];


     if( i_qbits >= 0 )
    {
	    
	    dct_int[0][0] = ( dct_int[0][0] * dequant_mf[i_mf][0][0] ) << i_qbits;
	    dct_int[0][1] = ( dct_int[0][1] * dequant_mf[i_mf][0][1] ) << i_qbits;
	    dct_int[0][2] = ( dct_int[0][2] * dequant_mf[i_mf][0][2] ) << i_qbits;
	    dct_int[0][3] = ( dct_int[0][3] * dequant_mf[i_mf][0][3] ) << i_qbits;

	    dct_int[1][0] = ( dct_int[1][0] * dequant_mf[i_mf][1][0] ) << i_qbits;
	    dct_int[1][1] = ( dct_int[1][1] * dequant_mf[i_mf][1][1] ) << i_qbits;
	    dct_int[1][2] = ( dct_int[1][2] * dequant_mf[i_mf][1][2] ) << i_qbits;
	    dct_int[1][3] = ( dct_int[1][3] * dequant_mf[i_mf][1][3] ) << i_qbits;

	    dct_int[2][0] = ( dct_int[2][0] * dequant_mf[i_mf][2][0] ) << i_qbits;
	    dct_int[2][1] = ( dct_int[2][1] * dequant_mf[i_mf][2][1] ) << i_qbits;
	    dct_int[2][2] = ( dct_int[2][2] * dequant_mf[i_mf][2][2] ) << i_qbits;
	    dct_int[2][3] = ( dct_int[2][3] * dequant_mf[i_mf][2][3] ) << i_qbits;

	    dct_int[3][0] = ( dct_int[3][0] * dequant_mf[i_mf][3][0] ) << i_qbits;
	    dct_int[3][1] = ( dct_int[3][1] * dequant_mf[i_mf][3][1] ) << i_qbits;
	    dct_int[3][2] = ( dct_int[3][2] * dequant_mf[i_mf][3][2] ) << i_qbits;
	    dct_int[3][3] = ( dct_int[3][3] * dequant_mf[i_mf][3][3] ) << i_qbits;


    }
    else
    {

        const int f = 1 << (-i_qbits-1);

	dct_int[0][0] = ( dct_int[0][0] * dequant_mf[i_mf][0][0] + f ) >> (-i_qbits);
	dct_int[0][1] = ( dct_int[0][1] * dequant_mf[i_mf][0][1] + f ) >> (-i_qbits);
	dct_int[0][2] = ( dct_int[0][2] * dequant_mf[i_mf][0][2] + f ) >> (-i_qbits);
	dct_int[0][3] = ( dct_int[0][3] * dequant_mf[i_mf][0][3] + f ) >> (-i_qbits);

	dct_int[1][0] = ( dct_int[1][0] * dequant_mf[i_mf][1][0] + f ) >> (-i_qbits);
	dct_int[1][1] = ( dct_int[1][1] * dequant_mf[i_mf][1][1] + f ) >> (-i_qbits);
	dct_int[1][2] = ( dct_int[1][2] * dequant_mf[i_mf][1][2] + f ) >> (-i_qbits);
	dct_int[1][3] = ( dct_int[1][3] * dequant_mf[i_mf][1][3] + f ) >> (-i_qbits);


	dct_int[2][0] = ( dct_int[2][0] * dequant_mf[i_mf][2][0] + f ) >> (-i_qbits);
	dct_int[2][1] = ( dct_int[2][1] * dequant_mf[i_mf][2][1] + f ) >> (-i_qbits);
	dct_int[2][2] = ( dct_int[2][2] * dequant_mf[i_mf][2][2] + f ) >> (-i_qbits);
	dct_int[2][3] = ( dct_int[2][3] * dequant_mf[i_mf][2][3] + f ) >> (-i_qbits);

	dct_int[3][0] = ( dct_int[3][0] * dequant_mf[i_mf][3][0] + f ) >> (-i_qbits);
	dct_int[3][1] = ( dct_int[3][1] * dequant_mf[i_mf][3][1] + f ) >> (-i_qbits);
	dct_int[3][2] = ( dct_int[3][2] * dequant_mf[i_mf][3][2] + f ) >> (-i_qbits);
	dct_int[3][3] = ( dct_int[3][3] * dequant_mf[i_mf][3][3] + f ) >> (-i_qbits);

    }    
 
     dct[0][0] =  dct_int[0][0]; 
     dct[0][1] =  dct_int[0][1]; 
     dct[0][2] =  dct_int[0][2]; 
     dct[0][3] =  dct_int[0][3]; 
     dct[1][0] =  dct_int[1][0]; 
     dct[1][1] =  dct_int[1][1]; 
     dct[1][2] =  dct_int[1][2]; 
     dct[1][3] =  dct_int[1][3]; 
     dct[2][0] =  dct_int[2][0]; 
     dct[2][1] =  dct_int[2][1]; 
     dct[2][2] =  dct_int[2][2]; 
     dct[2][3] =  dct_int[2][3]; 
     dct[3][0] =  dct_int[3][0]; 
     dct[3][1] =  dct_int[3][1]; 
     dct[3][2] =  dct_int[3][2]; 
     dct[3][3] =  dct_int[3][3]; 
#endif

}



#else

void dequant_4x4( int16_t dct[4][4], int dequant_mf[6][4][4], int i_qp )
{
    const int i_mf = i_qp%6;
    const int i_qbits = i_qp/6 - 4;
    int y;

    if( i_qbits >= 0 )
    {
        for( y = 0; y < 4; y++ )
        {
            DEQUANT_SHL( 0 );
            DEQUANT_SHL( 1 );
            DEQUANT_SHL( 2 );
            DEQUANT_SHL( 3 );
        }
    }
    else
    {
        const int f = 1 << (-i_qbits-1);
        for( y = 0; y < 4; y++ )
        {
            DEQUANT_SHR( 0 );
            DEQUANT_SHR( 1 );
            DEQUANT_SHR( 2 );
            DEQUANT_SHR( 3 );
        }
    }
}

#endif

static void dequant_8x8( int16_t dct[8][8], int dequant_mf[6][8][8], int i_qp )
{
    const int i_mf = i_qp%6;
    const int i_qbits = i_qp/6 - 6;
    int y;
    if( i_qbits >= 0 )
    {
        for( y = 0; y < 8; y++ )
        {
            DEQUANT_SHL( 0 );
            DEQUANT_SHL( 1 );
            DEQUANT_SHL( 2 );
            DEQUANT_SHL( 3 );
            DEQUANT_SHL( 4 );
            DEQUANT_SHL( 5 );
            DEQUANT_SHL( 6 );
            DEQUANT_SHL( 7 );
        }
    }
    else
    {
        const int f = 1 << (-i_qbits-1);
        for( y = 0; y < 8; y++ )
        {
            DEQUANT_SHR( 0 );
            DEQUANT_SHR( 1 );
            DEQUANT_SHR( 2 );
            DEQUANT_SHR( 3 );
            DEQUANT_SHR( 4 );
            DEQUANT_SHR( 5 );
            DEQUANT_SHR( 6 );
            DEQUANT_SHR( 7 );
        }
    }
}

void x264_mb_dequant_2x2_dc( int16_t dct[2][2], int dequant_mf[6][4][4], int i_qp )
{
    const int i_qbits = i_qp/6 - 5;

    if( i_qbits >= 0 )
    {
        const int i_dmf = dequant_mf[i_qp%6][0][0] << i_qbits;
        dct[0][0] *= i_dmf;
        dct[0][1] *= i_dmf;
        dct[1][0] *= i_dmf;
        dct[1][1] *= i_dmf;
    }
    else
    {
        const int i_dmf = dequant_mf[i_qp%6][0][0];
        // chroma DC is truncated, not rounded
        dct[0][0] = ( dct[0][0] * i_dmf ) >> (-i_qbits);
        dct[0][1] = ( dct[0][1] * i_dmf ) >> (-i_qbits);
        dct[1][0] = ( dct[1][0] * i_dmf ) >> (-i_qbits);
        dct[1][1] = ( dct[1][1] * i_dmf ) >> (-i_qbits);
    }
}
#ifdef VEC
void x264_mb_dequant_4x4_dc( int16_t dct[4][4], int dequant_mf[6][4][4], int i_qp )
{
    const int i_qbits = i_qp/6 - 6;
    int y;




#if 0

    int dct_int_a[4][4];
    int dct_int_b[4][4];

    {
        const int i_dmf = dequant_mf[i_qp%6][0][0] << i_qbits;

        dct_int_a[0][0] = dct[0][0] * i_dmf;
        dct_int_a[0][1] = dct[0][1] * i_dmf;
        dct_int_a[0][2] = dct[0][2] * i_dmf;
        dct_int_a[0][3] = dct[0][3] * i_dmf;

        dct_int_a[1][0] = dct[1][0] * i_dmf;
        dct_int_a[1][1] = dct[1][1] * i_dmf;
        dct_int_a[1][2] = dct[1][2] * i_dmf;
        dct_int_a[1][3] = dct[1][3] * i_dmf;

        dct_int_a[2][0] = dct[2][0] * i_dmf;
        dct_int_a[2][1] = dct[2][1] * i_dmf;
        dct_int_a[2][2] = dct[2][2] * i_dmf;
        dct_int_a[2][3] = dct[2][3] * i_dmf;

        dct_int_a[3][0] = dct[3][0] * i_dmf;
        dct_int_a[3][1] = dct[3][1] * i_dmf;
        dct_int_a[3][2] = dct[3][2] * i_dmf;
        dct_int_a[3][3] = dct[3][3] * i_dmf;

    }

    {
        const int i_dmf = dequant_mf[i_qp%6][0][0];
        const int f = 1 << (-i_qbits-1);

        dct_int_b[0][0] = ( dct[0][0] * i_dmf + f ) >> (-i_qbits);
        dct_int_b[0][1] = ( dct[0][1] * i_dmf + f ) >> (-i_qbits);
        dct_int_b[0][2] = ( dct[0][2] * i_dmf + f ) >> (-i_qbits);
        dct_int_b[0][3] = ( dct[0][3] * i_dmf + f ) >> (-i_qbits);


        dct_int_b[1][0] = ( dct[1][0] * i_dmf + f ) >> (-i_qbits);
        dct_int_b[1][1] = ( dct[1][1] * i_dmf + f ) >> (-i_qbits);
        dct_int_b[1][2] = ( dct[1][2] * i_dmf + f ) >> (-i_qbits);
        dct_int_b[1][3] = ( dct[1][3] * i_dmf + f ) >> (-i_qbits);


        dct_int_b[2][0] = ( dct[2][0] * i_dmf + f ) >> (-i_qbits);
        dct_int_b[2][1] = ( dct[2][1] * i_dmf + f ) >> (-i_qbits);
        dct_int_b[2][2] = ( dct[2][2] * i_dmf + f ) >> (-i_qbits);
        dct_int_b[2][3] = ( dct[2][3] * i_dmf + f ) >> (-i_qbits);


        dct_int_b[3][0] = ( dct[3][0] * i_dmf + f ) >> (-i_qbits);
        dct_int_b[3][1] = ( dct[3][1] * i_dmf + f ) >> (-i_qbits);
        dct_int_b[3][2] = ( dct[3][2] * i_dmf + f ) >> (-i_qbits);
        dct_int_b[3][3] = ( dct[3][3] * i_dmf + f ) >> (-i_qbits);


    }
    uint16_t result;
    int i,j;
    result = ( i_qbits >> 15 );

    for ( i=0; i<4; i++ )
       for ( j=0; j<4; j++)
       dct[i][j] = ( dct_int_a[i][j]  & ~result  ) || (  dct_int_b[i][j] & result );


#else

    int dct_int[4][4];

    dct_int[0][0] = dct[0][0];
    dct_int[0][1] = dct[0][1];
    dct_int[0][2] = dct[0][2];
    dct_int[0][3] = dct[0][3];
    dct_int[1][0] = dct[1][0];
    dct_int[1][1] = dct[1][1];
    dct_int[1][2] = dct[1][2];
    dct_int[1][3] = dct[1][3];
    dct_int[2][0] = dct[2][0];
    dct_int[2][1] = dct[2][1];
    dct_int[2][2] = dct[2][2];
    dct_int[2][3] = dct[2][3];
    dct_int[3][0] = dct[3][0];
    dct_int[3][1] = dct[3][1];
    dct_int[3][2] = dct[3][2];
    dct_int[3][3] = dct[3][3];




    if( i_qbits >= 0 )
    {
        const int i_dmf = dequant_mf[i_qp%6][0][0] << i_qbits;

        dct_int[0][0] *= i_dmf;
        dct_int[0][1] *= i_dmf;
        dct_int[0][2] *= i_dmf;
        dct_int[0][3] *= i_dmf;

        dct_int[1][0] *= i_dmf;
        dct_int[1][1] *= i_dmf;
        dct_int[1][2] *= i_dmf;
        dct_int[1][3] *= i_dmf;

        dct_int[2][0] *= i_dmf;
        dct_int[2][1] *= i_dmf;
        dct_int[2][2] *= i_dmf;
        dct_int[2][3] *= i_dmf;

        dct_int[3][0] *= i_dmf;
        dct_int[3][1] *= i_dmf;
        dct_int[3][2] *= i_dmf;
        dct_int[3][3] *= i_dmf;

    }
    else
    {
        const int i_dmf = dequant_mf[i_qp%6][0][0];
        const int f = 1 << (-i_qbits-1);

        dct_int[0][0] = ( dct_int[0][0] * i_dmf + f ) >> (-i_qbits);
        dct_int[0][1] = ( dct_int[0][1] * i_dmf + f ) >> (-i_qbits);
        dct_int[0][2] = ( dct_int[0][2] * i_dmf + f ) >> (-i_qbits);
        dct_int[0][3] = ( dct_int[0][3] * i_dmf + f ) >> (-i_qbits);


        dct_int[1][0] = ( dct_int[1][0] * i_dmf + f ) >> (-i_qbits);
        dct_int[1][1] = ( dct_int[1][1] * i_dmf + f ) >> (-i_qbits);
        dct_int[1][2] = ( dct_int[1][2] * i_dmf + f ) >> (-i_qbits);
        dct_int[1][3] = ( dct_int[1][3] * i_dmf + f ) >> (-i_qbits);


        dct_int[2][0] = ( dct_int[2][0] * i_dmf + f ) >> (-i_qbits);
        dct_int[2][1] = ( dct_int[2][1] * i_dmf + f ) >> (-i_qbits);
        dct_int[2][2] = ( dct_int[2][2] * i_dmf + f ) >> (-i_qbits);
        dct_int[2][3] = ( dct_int[2][3] * i_dmf + f ) >> (-i_qbits);


        dct_int[3][0] = ( dct_int[3][0] * i_dmf + f ) >> (-i_qbits);
        dct_int[3][1] = ( dct_int[3][1] * i_dmf + f ) >> (-i_qbits);
        dct_int[3][2] = ( dct_int[3][2] * i_dmf + f ) >> (-i_qbits);
        dct_int[3][3] = ( dct_int[3][3] * i_dmf + f ) >> (-i_qbits);




    }
     dct[0][0] =  dct_int[0][0]; 
     dct[0][1] =  dct_int[0][1]; 
     dct[0][2] =  dct_int[0][2]; 
     dct[0][3] =  dct_int[0][3]; 
     dct[1][0] =  dct_int[1][0]; 
     dct[1][1] =  dct_int[1][1]; 
     dct[1][2] =  dct_int[1][2]; 
     dct[1][3] =  dct_int[1][3]; 
     dct[2][0] =  dct_int[2][0]; 
     dct[2][1] =  dct_int[2][1]; 
     dct[2][2] =  dct_int[2][2]; 
     dct[2][3] =  dct_int[2][3]; 
     dct[3][0] =  dct_int[3][0]; 
     dct[3][1] =  dct_int[3][1]; 
     dct[3][2] =  dct_int[3][2]; 
     dct[3][3] =  dct_int[3][3]; 

#endif

}
#else

void x264_mb_dequant_4x4_dc( int16_t dct[4][4], int dequant_mf[6][4][4], int i_qp )
{
    const int i_qbits = i_qp/6 - 6;
    int y;


    if( i_qbits >= 0 )
    {
        const int i_dmf = dequant_mf[i_qp%6][0][0] << i_qbits;

        for( y = 0; y < 4; y++ )
        {
            dct[y][0] *= i_dmf;
            dct[y][1] *= i_dmf;
            dct[y][2] *= i_dmf;
            dct[y][3] *= i_dmf;
        }
    }
    else
    {
        const int i_dmf = dequant_mf[i_qp%6][0][0];
        const int f = 1 << (-i_qbits-1);

        for( y = 0; y < 4; y++ )
        {
            dct[y][0] = ( dct[y][0] * i_dmf + f ) >> (-i_qbits);
            dct[y][1] = ( dct[y][1] * i_dmf + f ) >> (-i_qbits);
            dct[y][2] = ( dct[y][2] * i_dmf + f ) >> (-i_qbits);
            dct[y][3] = ( dct[y][3] * i_dmf + f ) >> (-i_qbits);
        }
    }
}

#endif

static void x264_denoise_dct( int16_t *dct, uint32_t *sum, uint16_t *offset, int size )
{
    int i;
    for( i=1; i<size; i++ )
    {
        int level = dct[i];
        int sign = level>>15;
        level = (level+sign)^sign;
        sum[i] += level;
        level -= offset[i];
        dct[i] = level<0 ? 0 : (level^sign)-sign;
    }
}

