/*****************************************************************************
 * macroblock.c: h264 encoder library
 *****************************************************************************
 * Copyright (C) 2003-2008 x264 project
 *
 * Authors: Laurent Aimar <fenrir@via.ecp.fr>
 *          Loren Merritt <lorenm@u.washington.edu>
 *          Jason Garrett-Glaser <darkshikari@gmail.com>
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




#if 1
#include "common/spu/common_tpc.h"
#include "encoder/spu/macroblock_tpc.h"
#include "common/spu/macroblock_tpc.h"


#define ZIG(i,y,x) level[i] = dct[x][y];
static inline void zigzag_scan_2x2_dc( int16_t level[4], int16_t dct[2][2] )
{
    ZIG(0,0,0)
    ZIG(1,0,1)
    ZIG(2,1,0)
    ZIG(3,1,1)
}
#undef ZIG

/*****************************************************************************
 * x264_macroblock_probe_skip_tpc:
 *  Check if the current MB could be encoded as a [PB]_SKIP (it supposes you use
 *  the previous QP
 *****************************************************************************/
int x264_macroblock_probe_skip_tpc( x264_data_t *data, const int b_bidir, x264_mb_t *mb )
{
    DECLARE_ALIGNED_16( int16_t dct4x4[4][4][4] );
    DECLARE_ALIGNED_16( int16_t dct2x2[2][2] );
    DECLARE_ALIGNED_16( int16_t dctscan[16] );

    int i_qp = mb->i_qp;
    int mvp[2];
    int ch, thresh;

    int i8x8, i4x4;
    int i_decimate_mb;

    if( !b_bidir )
    {
        /* Get the MV */
        mvp[0] = x264_clip3( mb->cache.pskip_mv[0], mb->mv_min[0], mb->mv_max[0] );
        mvp[1] = x264_clip3( mb->cache.pskip_mv[1], mb->mv_min[1], mb->mv_max[1] );

        /* Motion compensation */
        mc_luma( mb->pic.p_fdec[0],    FDEC_STRIDE,
                       mb->pic.p_fref[0][0], mb->pic.i_stride[0],
                       mvp[0], mvp[1], 16, 16 );
    }

    for( i8x8 = 0, i_decimate_mb = 0; i8x8 < 4; i8x8++ )
    {
        int fenc_offset = (i8x8&1) * 8 + (i8x8>>1) * FENC_STRIDE * 8;
        int fdec_offset = (i8x8&1) * 8 + (i8x8>>1) * FDEC_STRIDE * 8;
        /* get luma diff */
       sub8x8_dct( dct4x4, mb->pic.p_fenc[0] + fenc_offset,
                                    mb->pic.p_fdec[0] + fdec_offset );
        /* encode one 4x4 block */
        for( i4x4 = 0; i4x4 < 4; i4x4++ )
        {
            quant_4x4( dct4x4[i4x4], quant4_mf[CQM_4PY][0], quant4_bias[CQM_4PY][0] );
            if( !array_non_zero(dct4x4[i4x4]) )
                continue;
            zigzag_scan_4x4_field( dctscan, dct4x4[i4x4] );
            i_decimate_mb += x264_mb_decimate_score( dctscan, 16 );
            if( i_decimate_mb >= 6 )
                return 0;
        }
    }

    /* encode chroma */
    i_qp = mb->i_chroma_qp;
    thresh = (x264_lambda2_tab[i_qp] + 32) >> 6;

    for( ch = 0; ch < 2; ch++ )
    {
        uint8_t  *p_src = mb->pic.p_fenc[1+ch];
        uint8_t  *p_dst = mb->pic.p_fdec[1+ch];

        if( !b_bidir )
        {
            mc_chroma( mb->pic.p_fdec[1+ch],       FDEC_STRIDE,
                             mb->pic.p_fref[0][0][4+ch], mb->pic.i_stride[1+ch],
                             mvp[0], mvp[1], 8, 8 );
        }

        /* there is almost never a termination during chroma, but we can't avoid the check entirely */
        /* so instead we check SSD and skip the actual check if the score is low enough. */
        if( ssdf[PIXEL_8x8]( p_dst, FDEC_STRIDE, p_src, FENC_STRIDE ) < thresh )
            continue;

        sub8x8_dct( dct4x4, p_src, p_dst );

        /* calculate dct DC */
        dct2x2[0][0] = dct4x4[0][0][0];
        dct2x2[0][1] = dct4x4[1][0][0];
        dct2x2[1][0] = dct4x4[2][0][0];
        dct2x2[1][1] = dct4x4[3][0][0];
        dct2x2dc( dct2x2 );
        quant_2x2_dc( dct2x2, quant4_mf[CQM_4PC][0][0]>>1, quant4_bias[CQM_4PC][0][0]<<1 );
        if( array_non_zero(dct2x2) )
            return 0;

        /* calculate dct coeffs */
        for( i4x4 = 0, i_decimate_mb = 0; i4x4 < 4; i4x4++ )
        {
            quant_4x4( dct4x4[i4x4], quant4_mf[CQM_4PC][0], quant4_bias[CQM_4PC][0] );
            if( !array_non_zero(dct4x4[i4x4]) )
                continue;
            zigzag_scan_4x4_field( dctscan, dct4x4[i4x4] );
            i_decimate_mb += x264_mb_decimate_score( dctscan+1, 15 );
            if( i_decimate_mb >= 7 )
                return 0;
        }
    }

    mb->b_skip_mc = 1;
    return 1;
}

static void x264_macroblock_encode_skip_tpc(  x264_data_t *data, x264_mb_t *mb )
{
    mb->i_cbp_luma = 0x00;
    mb->i_cbp_chroma = 0x00;
    memset( mb->cache.non_zero_count, 0, X264_SCAN8_SIZE );
    /* store cbp */
//    mb->cbp[mb->i_mb_xy] = 0;
}

/*****************************************************************************
 * x264_macroblock_encode_pskip:
 *  Encode an already marked skip block
 *****************************************************************************/
static void x264_macroblock_encode_pskip_tpc( x264_data_t *data, x264_mb_t *mb )
{
    const int mvx = x264_clip3( mb->cache.mv[0][x264_scan8[0]][0],
                                mb->mv_min[0], mb->mv_max[0] );
    const int mvy = x264_clip3( mb->cache.mv[0][x264_scan8[0]][1],
                                mb->mv_min[1], mb->mv_max[1] );

    /* don't do pskip motion compensation if it was already done in macroblock_analyse */
    if( !mb->b_skip_mc )
    {
        mc_luma( mb->pic.p_fdec[0],    FDEC_STRIDE,
                 mb->pic.p_fref[0][0], mb->pic.i_stride[0],
                 mvx, mvy, 16, 16 );

        mc_chroma( mb->pic.p_fdec[1],       FDEC_STRIDE,
                      mb->pic.p_fref[0][0][4], mb->pic.i_stride[1],
                      mvx, mvy, 8, 8 );

        mc_chroma( mb->pic.p_fdec[2],       FDEC_STRIDE,
                   mb->pic.p_fref[0][0][5], mb->pic.i_stride[2],
                   mvx, mvy, 8, 8 );
    }

    x264_macroblock_encode_skip_tpc( data, mb );
}




/* (ref: JVT-B118)
 * x264_mb_decimate_score: given dct coeffs it returns a score to see if we could empty this dct coeffs
 * to 0 (low score means set it to null)
 * Used in inter macroblock (luma and chroma)
 *  luma: for a 8x8 block: if score < 4 -> null
 *        for the complete mb: if score < 6 -> null
 *  chroma: for the complete mb: if score < 7 -> null
 */
int x264_mb_decimate_score( int16_t *dct, int i_max )
{
    static const int i_ds_table4[16] = {
        3,2,2,1,1,1,0,0,0,0,0,0,0,0,0,0 };
    static const int i_ds_table8[64] = {
        3,3,3,3,2,2,2,2,2,2,2,2,1,1,1,1,
        1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };

    const int *ds_table = (i_max == 64) ? i_ds_table8 : i_ds_table4;
    int i_score = 0;
    int idx = i_max - 1;

    while( idx >= 0 && dct[idx] == 0 )
        idx--;

    while( idx >= 0 )
    {
        int i_run;

        if( (unsigned)(dct[idx--] + 1) > 2 )
            return 9;

        i_run = 0;
        while( idx >= 0 && dct[idx] == 0 )
        {
            idx--;
            i_run++;
        }
        i_score += ds_table[i_run];
    }

    return i_score;
}

#endif
#if 1

static ALWAYS_INLINE void x264_quant_4x4_tpc( x264_data_t *data, int16_t dct[4][4], int i_qp, int i_ctxBlockCat, int b_intra, int idx, x264_mb_t *mb )
{
    int i_quant_cat = b_intra ? CQM_4IY : CQM_4PY;


        quant_4x4( dct, quant4_mf[i_quant_cat][0], quant4_bias[i_quant_cat][0] );



}

static ALWAYS_INLINE void x264_quant_8x8_tpc( x264_data_t *data, int16_t dct[8][8], int i_qp, int b_intra, int idx, x264_mb_t *mb )
{

    int i_quant_cat = b_intra ? CQM_8IY : CQM_8PY;

        quant_8x8( dct, quant8_mf[i_quant_cat][0], quant8_bias[i_quant_cat][0] );
}





void x264_mb_encode_8x8_chroma_tpc( x264_data_t *data, int b_inter, int i_qp, x264_mb_t *mb, x264_dct_t *dct )
{
    int i, ch;
    int b_decimate = 1; // b_inter && (mb->i_type == SLICE_TYPE_B || data->b_dct_decimate);

    for( ch = 0; ch < 2; ch++ )
    {
        uint8_t  *p_src = mb->pic.p_fenc[1+ch];
        uint8_t  *p_dst = mb->pic.p_fdec[1+ch];
        int i_decimate_score = 0;

        DECLARE_ALIGNED_16( int16_t dct2x2[2][2]  );
        DECLARE_ALIGNED_16( int16_t dct4x4[4][4][4] );



        sub8x8_dct( dct4x4, p_src, p_dst );
        /* calculate dct coeffs */
        for( i = 0; i < 4; i++ )
        {
            /* copy dc coeff */
            dct2x2[i>>1][i&1] = dct4x4[i][0][0];
            dct4x4[i][0][0] = 0;

            /* no trellis; it doesn't seem to help chroma noticeably */
            quant_4x4( dct4x4[i], quant4_mf[CQM_4IC+b_inter][0], quant4_bias[CQM_4IC+b_inter][0] );
            zigzag_scan_4x4_frame( dct->luma4x4[16+i+ch*4], dct4x4[i] );

            if( b_decimate )
                i_decimate_score += x264_mb_decimate_score( dct->luma4x4[16+i+ch*4]+1, 15 );
        }

        dct2x2dc( dct2x2 );
        quant_2x2_dc( dct2x2,quant4_mf[CQM_4IC+b_inter][0][0]>>1, quant4_bias[CQM_4IC+b_inter][0][0]<<1 );
        zigzag_scan_2x2_dc( dct->chroma_dc[ch], dct2x2 );

        /* output samples to fdec */
        dct2x2dc( dct2x2 );
        x264_mb_dequant_2x2_dc( dct2x2, dequant4_mf[CQM_4IC + b_inter], i_qp );  /* XXX not inversed */

        if( b_decimate && i_decimate_score < 7 )
        {
            /* Near null chroma 8x8 block so make it null (bits saving) */
            memset( &dct->luma4x4[16+ch*4], 0, 4 * sizeof( *dct->luma4x4 ) );
            if( !array_non_zero( dct2x2 ) )
                continue;
            memset( dct4x4, 0, sizeof( dct4x4 ) );
        }
        else
        {
            for( i = 0; i < 4; i++ )
                dequant_4x4( dct4x4[i], dequant4_mf[CQM_4IC + b_inter], i_qp );
        }
        dct4x4[0][0][0] = dct2x2[0][0];
        dct4x4[1][0][0] = dct2x2[0][1];
        dct4x4[2][0][0] = dct2x2[1][0];
        dct4x4[3][0][0] = dct2x2[1][1];
        add8x8_idct( p_dst, dct4x4 );
    }

    /* coded block pattern */
    mb->i_cbp_chroma = 0;
    for( i = 0; i < 8; i++ )
    {
        int nz = array_non_zero( dct->luma4x4[16+i] );
        mb->cache.non_zero_count[x264_scan8[16+i]] = nz;
        mb->i_cbp_chroma |= nz;
    }
    if( mb->i_cbp_chroma )
        mb->i_cbp_chroma = 2;    /* dc+ac (we can't do only ac) */
    else if( array_non_zero( dct->chroma_dc ) )
        mb->i_cbp_chroma = 1;    /* dc only */

}




/*****************************************************************************
 * x264_macroblock_encode:
 *****************************************************************************/
void x264_macroblock_encode_tpc( x264_data_t *data, x264_mb_t *mb, x264_dct_t *dct )
{

    int i_qp = mb->i_qp;
    int b_decimate = data->i_type == SLICE_TYPE_B || data->b_dct_decimate;
    int b_force_no_skip = 0;
    int i,j,idx;
    uint8_t nnz8x8[4] = {1,1,1,1};

    if( data->b_mbaff
        && mb->i_mb_xy == data->i_first_mb + mb->i_mb_stride
        && IS_SKIP(mb->type[data->i_first_mb]) )
    {
        /* The first skip is predicted to be a frame mb pair.
         * We don't yet support the aff part of mbaff, so force it to non-skip
         * so that we can pick the aff flag. */
        b_force_no_skip = 1;
        if( IS_SKIP(mb->i_type) )
        {
            if( mb->i_type == P_SKIP )
                mb->i_type = P_L0;
            else if( mb->i_type == B_SKIP )
                mb->i_type = B_DIRECT;
        }
    }
    if( mb->i_type == P_SKIP )
    {
        /* A bit special */
        x264_macroblock_encode_pskip_tpc( data, mb );
        return;
    }
    if( mb->i_type == B_SKIP )
    {
        /* don't do bskip motion compensation if it was already done in macroblock_analyse */
        if( !mb->b_skip_mc )
            x264_mb_mc_tpc( data, mb );
        x264_macroblock_encode_skip_tpc( data, mb );
        return;

    }
   /* Inter MB */
    {
        int i8x8, i4x4;
        int i_decimate_mb = 0;

        /* Don't repeat motion compensation if it was already done in non-RD transform analysis */
        if( !mb->b_skip_mc )
            x264_mb_mc_tpc( data, mb );


        {
            DECLARE_ALIGNED_16( int16_t dct4x4[16][4][4] );
            sub16x16_dct( dct4x4, mb->pic.p_fenc[0], mb->pic.p_fdec[0] );
            nr_count[0] += mb->b_noise_reduction * 16;

            for( i8x8 = 0; i8x8 < 4; i8x8++ )
            {
                int i_decimate_8x8;

                /* encode one 4x4 block */
                i_decimate_8x8 = 0;
                for( i4x4 = 0; i4x4 < 4; i4x4++ )
                {
                    idx = i8x8 * 4 + i4x4;

                    x264_quant_4x4_tpc( data, dct4x4[idx], i_qp, DCT_LUMA_4x4, 0, idx, mb );

                    zigzag_scan_4x4_frame( dct->luma4x4[idx], dct4x4[idx] );

                    if( b_decimate && i_decimate_8x8 <= 6 )
                        i_decimate_8x8 += x264_mb_decimate_score( dct->luma4x4[idx], 16 );
                }

                /* decimate this 8x8 block */
                i_decimate_mb += i_decimate_8x8;
                if( i_decimate_8x8 < 4 && b_decimate )
                    nnz8x8[i8x8] = 0;
            }


            if( i_decimate_mb < 6 && b_decimate )
                *(uint32_t*)nnz8x8 = 0;
            else
            {
                for( i8x8 = 0; i8x8 < 4; i8x8++ )
                    if( nnz8x8[i8x8] )
                    {
                        for( i = 0; i < 4; i++ )
                            dequant_4x4( dct4x4[i8x8*4+i], dequant4_mf[CQM_4PY], i_qp );
                        add8x8_idct( &mb->pic.p_fdec[0][(i8x8&1)*8 + (i8x8>>1)*8*FDEC_STRIDE], &dct4x4[i8x8*4] );
                    }
            }

        }

    }

    /* encode chroma */
    if( IS_INTRA( mb->i_type ) )
    {
        const int i_mode = mb->i_chroma_pred_mode;
        predict_8x8cf[i_mode]( mb->pic.p_fdec[1] );
        predict_8x8cf[i_mode]( mb->pic.p_fdec[2] );
    }

    /* encode the 8x8 blocks */
   x264_mb_encode_8x8_chroma_tpc( data, !IS_INTRA( mb->i_type ), mb->i_chroma_qp, mb, dct );

    /* coded block pattern and non_zero_count */
    mb->i_cbp_luma = 0x00;
    if( mb->i_type == I_16x16 )
    {
        for( i = 0; i < 16; i++ )
        {
            int nz = array_non_zero( dct->luma4x4[i] );
            mb->cache.non_zero_count[x264_scan8[i]] = nz;
            mb->i_cbp_luma |= nz;
        }
        mb->i_cbp_luma *= 0xf;
    }
    else
    {
        for( i = 0; i < 4; i++)
        {
            if(!nnz8x8[i])
            {
                *(uint16_t*)&mb->cache.non_zero_count[x264_scan8[0+i*4]] = 0;
                *(uint16_t*)&mb->cache.non_zero_count[x264_scan8[2+i*4]] = 0;
            }
            else if( mb->b_transform_8x8 )
            {
                *(uint16_t*)&mb->cache.non_zero_count[x264_scan8[0+4*i]] = nnz8x8[i] * 0x0101;
                *(uint16_t*)&mb->cache.non_zero_count[x264_scan8[2+4*i]] = nnz8x8[i] * 0x0101;
                mb->i_cbp_luma |= nnz8x8[i] << i;
            }
            else
            {
                int nz, cbp = 0;
                for( j = 0; j < 4; j++ )
                {
                    nz = array_non_zero( dct->luma4x4[j+4*i] );
                    mb->cache.non_zero_count[x264_scan8[j+4*i]] = nz;
                    cbp |= nz;
                }
                mb->i_cbp_luma |= cbp << i;
            }
        }
    }



    /* store cbp */
    //mb->cbp[mb->i_mb_xy] = (i_cbp_dc << 8) | (mb->i_cbp_chroma << 4) | mb->i_cbp_luma;

    /* Check for P_SKIP
     * XXX: in the me perhaps we should take x264_mb_predict_mv_pskip into account
     *      (if multiple mv give same result)*/
    if( !b_force_no_skip )
    {
        if( mb->i_type == P_L0 && mb->i_partition == D_16x16 &&
            !(mb->i_cbp_luma | mb->i_cbp_chroma) &&
            *(uint32_t*)mb->cache.mv[0][x264_scan8[0]] == *(uint32_t*)mb->cache.pskip_mv
            && mb->cache.ref[0][x264_scan8[0]] == 0 )
        {
            mb->i_type = P_SKIP;
        }

        /* Check for B_SKIP */
        if( mb->i_type == B_DIRECT && !(mb->i_cbp_luma | mb->i_cbp_chroma) )
        {
            mb->i_type = B_SKIP;
        }
    }


}
#endif




