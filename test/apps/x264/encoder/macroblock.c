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

#include "common/common.h"
#include "macroblock.h"
#include "common/macroblock_tpc.h"

#define ZIG(i,y,x) level[i] = dct[x][y];
static inline void zigzag_scan_2x2_dc( int16_t level[4], int16_t dct[2][2] )
{
    ZIG(0,0,0)
    ZIG(1,0,1)
    ZIG(2,1,0)
    ZIG(3,1,1)
}
#undef ZIG

/* (ref: JVT-B118)
 * x264_mb_decimate_score: given dct coeffs it returns a score to see if we could empty this dct coeffs
 * to 0 (low score means set it to null)
 * Used in inter macroblock (luma and chroma)
 *  luma: for a 8x8 block: if score < 4 -> null
 *        for the complete mb: if score < 6 -> null
 *  chroma: for the complete mb: if score < 7 -> null
 */
static int x264_mb_decimate_score( int16_t *dct, int i_max )
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

static ALWAYS_INLINE void x264_quant_4x4( x264_t *h, int16_t dct[4][4], int i_qp, int i_ctxBlockCat, int b_intra, int idx )
{
    int i_quant_cat = b_intra ? CQM_4IY : CQM_4PY;
    if( h->mb.b_trellis )
        x264_quant_4x4_trellis( h, dct, i_quant_cat, i_qp, i_ctxBlockCat, b_intra, idx );
    else
        h->quantf.quant_4x4( dct, h->quant4_mf[i_quant_cat][i_qp], h->quant4_bias[i_quant_cat][i_qp] );
}

static ALWAYS_INLINE void x264_quant_8x8( x264_t *h, int16_t dct[8][8], int i_qp, int b_intra, int idx )
{
    int i_quant_cat = b_intra ? CQM_8IY : CQM_8PY;
    if( h->mb.b_trellis )
        x264_quant_8x8_trellis( h, dct, i_quant_cat, i_qp, b_intra, idx );
    else
        h->quantf.quant_8x8( dct, h->quant8_mf[i_quant_cat][i_qp], h->quant8_bias[i_quant_cat][i_qp] );
}

void x264_mb_encode_i4x4( x264_t *h, int idx, int i_qp )
{
    uint8_t *p_src = &h->mb.pic.p_fenc[0][block_idx_xy_fenc[idx]];
    uint8_t *p_dst = &h->mb.pic.p_fdec[0][block_idx_xy_fdec[idx]];
    DECLARE_ALIGNED_16( int16_t dct4x4[4][4] );

    if( h->mb.b_lossless )
    {
        h->zigzagf.sub_4x4( h->dct.luma4x4[idx], p_src, p_dst );
        return;
    }

    h->dctf.sub4x4_dct( dct4x4, p_src, p_dst );

    x264_quant_4x4( h, dct4x4, i_qp, DCT_LUMA_4x4, 1, idx );

    if( array_non_zero( dct4x4 ) )
    {
        h->zigzagf.scan_4x4( h->dct.luma4x4[idx], dct4x4 );
        h->quantf.dequant_4x4( dct4x4, h->dequant4_mf[CQM_4IY], i_qp );

        /* output samples to fdec */
        h->dctf.add4x4_idct( p_dst, dct4x4 );
    }
    else
        memset( h->dct.luma4x4[idx], 0, sizeof(h->dct.luma4x4[idx]));
}

void x264_mb_encode_i8x8( x264_t *h, int idx, int i_qp )
{
    int x = 8 * (idx&1);
    int y = 8 * (idx>>1);
    uint8_t *p_src = &h->mb.pic.p_fenc[0][x+y*FENC_STRIDE];
    uint8_t *p_dst = &h->mb.pic.p_fdec[0][x+y*FDEC_STRIDE];
    DECLARE_ALIGNED_16( int16_t dct8x8[8][8] );

    h->dctf.sub8x8_dct8( dct8x8, p_src, p_dst );

    x264_quant_8x8( h, dct8x8, i_qp, 1, idx );

    h->zigzagf.scan_8x8( h->dct.luma8x8[idx], dct8x8 );
    h->quantf.dequant_8x8( dct8x8, h->dequant8_mf[CQM_8IY], i_qp );
    h->dctf.add8x8_idct8( p_dst, dct8x8 );
}

static void x264_mb_encode_i16x16( x264_t *h, int i_qp )
{
    uint8_t  *p_src = h->mb.pic.p_fenc[0];
    uint8_t  *p_dst = h->mb.pic.p_fdec[0];

    DECLARE_ALIGNED_16( int16_t dct4x4[16][4][4] );
    DECLARE_ALIGNED_16( int16_t dct_dc4x4[4][4] );

    int i;

    if( h->mb.b_lossless )
    {
        for( i = 0; i < 16; i++ )
        {
            int oe = block_idx_xy_fenc[i];
            int od = block_idx_xy_fdec[i];
            h->zigzagf.sub_4x4( h->dct.luma4x4[i], p_src+oe, p_dst+od );
            dct_dc4x4[0][block_idx_yx_1d[i]] = h->dct.luma4x4[i][0];
            h->dct.luma4x4[i][0] = 0;
        }
        h->zigzagf.scan_4x4( h->dct.luma16x16_dc, dct_dc4x4 );
        return;
    }

    h->dctf.sub16x16_dct( dct4x4, p_src, p_dst );
  
#if 0 
{
	int i,j,k;
	uint8_t *pix1 = p_src;
	for(i=0;i<16;i++){
		for (j=0; j<16;j++)
			printf(" 0x%x", pix1[i*16+j] );
		
		printf("\n");

	}
	printf("\n");
}


#endif

    for( i = 0; i < 16; i++ )
    {
        /* copy dc coeff */
        dct_dc4x4[0][block_idx_xy_1d[i]] = dct4x4[i][0][0];
        dct4x4[i][0][0] = 0;

        /* quant/scan/dequant */
        x264_quant_4x4( h, dct4x4[i], i_qp, DCT_LUMA_AC, 1, i );

        h->zigzagf.scan_4x4( h->dct.luma4x4[i], dct4x4[i] );
        h->quantf.dequant_4x4( dct4x4[i], h->dequant4_mf[CQM_4IY], i_qp );
    }

    h->dctf.dct4x4dc( dct_dc4x4 );
    h->quantf.quant_4x4_dc( dct_dc4x4, h->quant4_mf[CQM_4IY][i_qp][0]>>1, h->quant4_bias[CQM_4IY][i_qp][0]<<1 );
    h->zigzagf.scan_4x4( h->dct.luma16x16_dc, dct_dc4x4 );

    /* output samples to fdec */
    h->dctf.idct4x4dc( dct_dc4x4 );
    x264_mb_dequant_4x4_dc( dct_dc4x4, h->dequant4_mf[CQM_4IY], i_qp );  /* XXX not inversed */

    /* calculate dct coeffs */
    for( i = 0; i < 16; i++ )
    {
        /* copy dc coeff */
        dct4x4[i][0][0] = dct_dc4x4[0][block_idx_xy_1d[i]];
    }
    /* put pixels to fdec */
    h->dctf.add16x16_idct( p_dst, dct4x4 );
}

void x264_mb_encode_8x8_chroma( x264_t *h, int b_inter, int i_qp )
{
    int i, ch;
    int b_decimate = b_inter && (h->sh.i_type == SLICE_TYPE_B || h->param.analyse.b_dct_decimate);

    for( ch = 0; ch < 2; ch++ )
    {
        uint8_t  *p_src = h->mb.pic.p_fenc[1+ch];
        uint8_t  *p_dst = h->mb.pic.p_fdec[1+ch];
        int i_decimate_score = 0;

        DECLARE_ALIGNED_16( int16_t dct2x2[2][2]  );
        DECLARE_ALIGNED_16( int16_t dct4x4[4][4][4] );

        if( h->mb.b_lossless )
        {
            for( i = 0; i < 4; i++ )
            {
                int oe = block_idx_x[i]*4 + block_idx_y[i]*4*FENC_STRIDE;
                int od = block_idx_x[i]*4 + block_idx_y[i]*4*FDEC_STRIDE;
                h->zigzagf.sub_4x4( h->dct.luma4x4[16+i+ch*4], p_src+oe, p_dst+od );
                h->dct.chroma_dc[ch][i] = h->dct.luma4x4[16+i+ch*4][0];
                h->dct.luma4x4[16+i+ch*4][0] = 0;
            }
            continue;
        }

        h->dctf.sub8x8_dct( dct4x4, p_src, p_dst );
        /* calculate dct coeffs */
        for( i = 0; i < 4; i++ )
        {
            /* copy dc coeff */
            dct2x2[i>>1][i&1] = dct4x4[i][0][0];
            dct4x4[i][0][0] = 0;

            /* no trellis; it doesn't seem to help chroma noticeably */
            h->quantf.quant_4x4( dct4x4[i], h->quant4_mf[CQM_4IC+b_inter][i_qp], h->quant4_bias[CQM_4IC+b_inter][i_qp] );
            h->zigzagf.scan_4x4( h->dct.luma4x4[16+i+ch*4], dct4x4[i] );

            if( b_decimate )
                i_decimate_score += x264_mb_decimate_score( h->dct.luma4x4[16+i+ch*4]+1, 15 );
        }

        h->dctf.dct2x2dc( dct2x2 );
        h->quantf.quant_2x2_dc( dct2x2, h->quant4_mf[CQM_4IC+b_inter][i_qp][0]>>1, h->quant4_bias[CQM_4IC+b_inter][i_qp][0]<<1 );
        zigzag_scan_2x2_dc( h->dct.chroma_dc[ch], dct2x2 );

        /* output samples to fdec */
        h->dctf.idct2x2dc( dct2x2 );
        x264_mb_dequant_2x2_dc( dct2x2, h->dequant4_mf[CQM_4IC + b_inter], i_qp );  /* XXX not inversed */

        if( b_decimate && i_decimate_score < 7 )
        {
            /* Near null chroma 8x8 block so make it null (bits saving) */
            memset( &h->dct.luma4x4[16+ch*4], 0, 4 * sizeof( *h->dct.luma4x4 ) );
            if( !array_non_zero( dct2x2 ) )
                continue;
            memset( dct4x4, 0, sizeof( dct4x4 ) );
        }
        else
        {
            for( i = 0; i < 4; i++ )
                h->quantf.dequant_4x4( dct4x4[i], h->dequant4_mf[CQM_4IC + b_inter], i_qp );
        }
        dct4x4[0][0][0] = dct2x2[0][0];
        dct4x4[1][0][0] = dct2x2[0][1];
        dct4x4[2][0][0] = dct2x2[1][0];
        dct4x4[3][0][0] = dct2x2[1][1];
        h->dctf.add8x8_idct( p_dst, dct4x4 );
    }

    /* coded block pattern */
    h->mb.i_cbp_chroma = 0;
    for( i = 0; i < 8; i++ )
    {
        int nz = array_non_zero( h->dct.luma4x4[16+i] );
        h->mb.cache.non_zero_count[x264_scan8[16+i]] = nz;
        h->mb.i_cbp_chroma |= nz;
    }
    if( h->mb.i_cbp_chroma )
        h->mb.i_cbp_chroma = 2;    /* dc+ac (we can't do only ac) */
    else if( array_non_zero( h->dct.chroma_dc ) )
        h->mb.i_cbp_chroma = 1;    /* dc only */
}

static void x264_macroblock_encode_skip( x264_t *h )
{
    h->mb.i_cbp_luma = 0x00;
    h->mb.i_cbp_chroma = 0x00;
    memset( h->mb.cache.non_zero_count, 0, X264_SCAN8_SIZE );
    /* store cbp */
    h->mb.cbp[h->mb.i_mb_xy] = 0;
}

/*****************************************************************************
 * x264_macroblock_encode_pskip:
 *  Encode an already marked skip block
 *****************************************************************************/
static void x264_macroblock_encode_pskip( x264_t *h )
{
    const int mvx = x264_clip3( h->mb.cache.mv[0][x264_scan8[0]][0],
                                h->mb.mv_min[0], h->mb.mv_max[0] );
    const int mvy = x264_clip3( h->mb.cache.mv[0][x264_scan8[0]][1],
                                h->mb.mv_min[1], h->mb.mv_max[1] );

    /* don't do pskip motion compensation if it was already done in macroblock_analyse */
    if( !h->mb.b_skip_mc )
    {
        h->mc.mc_luma( h->mb.pic.p_fdec[0],    FDEC_STRIDE,
                       h->mb.pic.p_fref[0][0], h->mb.pic.i_stride[0],
                       mvx, mvy, 16, 16 );

        h->mc.mc_chroma( h->mb.pic.p_fdec[1],       FDEC_STRIDE,
                         h->mb.pic.p_fref[0][0][4], h->mb.pic.i_stride[1],
                         mvx, mvy, 8, 8 );

        h->mc.mc_chroma( h->mb.pic.p_fdec[2],       FDEC_STRIDE,
                         h->mb.pic.p_fref[0][0][5], h->mb.pic.i_stride[2],
                         mvx, mvy, 8, 8 );
    }

    x264_macroblock_encode_skip( h );
}

/*****************************************************************************
 * x264_macroblock_encode:
 *****************************************************************************/
void x264_macroblock_encode( x264_t *h )
{
    int i_cbp_dc = 0;
    int i_qp = h->mb.i_qp;
    int b_decimate = h->sh.i_type == SLICE_TYPE_B || h->param.analyse.b_dct_decimate;
    int b_force_no_skip = 0;
    int i,j,idx;
    uint8_t nnz8x8[4] = {1,1,1,1};

    if( h->sh.b_mbaff
        && h->mb.i_mb_xy == h->sh.i_first_mb + h->mb.i_mb_stride
        && IS_SKIP(h->mb.type[h->sh.i_first_mb]) )
    {
        /* The first skip is predicted to be a frame mb pair.
         * We don't yet support the aff part of mbaff, so force it to non-skip
         * so that we can pick the aff flag. */
        b_force_no_skip = 1;
        if( IS_SKIP(h->mb.i_type) )
        {
            if( h->mb.i_type == P_SKIP )
                h->mb.i_type = P_L0;
            else if( h->mb.i_type == B_SKIP )
                h->mb.i_type = B_DIRECT;
        }
    }

    if( h->mb.i_type == P_SKIP )
    {
        /* A bit special */
        x264_macroblock_encode_pskip( h );
        return;
    }
    if( h->mb.i_type == B_SKIP )
    {
        /* don't do bskip motion compensation if it was already done in macroblock_analyse */
        if( !h->mb.b_skip_mc )
            x264_mb_mc( h );
        x264_macroblock_encode_skip( h );
        return;
    }

    if( h->mb.i_type == I_16x16 )
    {
        const int i_mode = h->mb.i_intra16x16_pred_mode;
        h->mb.b_transform_8x8 = 0;
        /* do the right prediction */
        h->predict_16x16[i_mode]( h->mb.pic.p_fdec[0] );

        /* encode the 16x16 macroblock */
        x264_mb_encode_i16x16( h, i_qp );
    }
    else if( h->mb.i_type == I_8x8 )
    {
        DECLARE_ALIGNED_16( uint8_t edge[33] );
        h->mb.b_transform_8x8 = 1;
        /* If we already encoded 3 of the 4 i8x8 blocks, we don't have to do them again. */
        if( h->mb.i_skip_intra )
        {
            h->mc.copy[PIXEL_16x16]( h->mb.pic.p_fdec[0], FDEC_STRIDE, h->mb.pic.i8x8_fdec_buf, 16, 16 );
            /* In RD mode, restore the now-overwritten DCT data. */
            if( h->mb.i_skip_intra == 2 )
                h->mc.memcpy_aligned( h->dct.luma8x8, h->mb.pic.i8x8_dct_buf, sizeof(h->mb.pic.i8x8_dct_buf) );
        }
        for( i = h->mb.i_skip_intra ? 3 : 0 ; i < 4; i++ )
        {
            uint8_t  *p_dst = &h->mb.pic.p_fdec[0][8 * (i&1) + 8 * (i>>1) * FDEC_STRIDE];
            int      i_mode = h->mb.cache.intra4x4_pred_mode[x264_scan8[4*i]];

            x264_predict_8x8_filter( p_dst, edge, h->mb.i_neighbour8[i], x264_pred_i4x4_neighbors[i_mode] );
            h->predict_8x8[i_mode]( p_dst, edge );
            x264_mb_encode_i8x8( h, i, i_qp );
        }
        for( i = 0; i < 4; i++ )
            nnz8x8[i] = array_non_zero( h->dct.luma8x8[i] );
    }
    else if( h->mb.i_type == I_4x4 )
    {
        h->mb.b_transform_8x8 = 0;
        /* If we already encoded 15 of the 16 i4x4 blocks, we don't have to do them again. */
        if( h->mb.i_skip_intra )
        {
            h->mc.copy[PIXEL_16x16]( h->mb.pic.p_fdec[0], FDEC_STRIDE, h->mb.pic.i4x4_fdec_buf, 16, 16 );
            /* In RD mode, restore the now-overwritten DCT data. */
            if( h->mb.i_skip_intra == 2 )
                h->mc.memcpy_aligned( h->dct.luma4x4, h->mb.pic.i4x4_dct_buf, sizeof(h->mb.pic.i4x4_dct_buf) );
        }
        for( i = h->mb.i_skip_intra ? 15 : 0 ; i < 16; i++ )
        {
            uint8_t  *p_dst = &h->mb.pic.p_fdec[0][block_idx_xy_fdec[i]];
            int      i_mode = h->mb.cache.intra4x4_pred_mode[x264_scan8[i]];

            if( (h->mb.i_neighbour4[i] & (MB_TOPRIGHT|MB_TOP)) == MB_TOP )
                /* emulate missing topright samples */
                *(uint32_t*) &p_dst[4-FDEC_STRIDE] = p_dst[3-FDEC_STRIDE] * 0x01010101U;

            h->predict_4x4[i_mode]( p_dst );
            x264_mb_encode_i4x4( h, i, i_qp );
        }
    }
    else    /* Inter MB */
    {
        int i8x8, i4x4;
        int i_decimate_mb = 0;

        /* Don't repeat motion compensation if it was already done in non-RD transform analysis */
        if( !h->mb.b_skip_mc )
            x264_mb_mc( h );

        if( h->mb.b_lossless )
        {
            for( i4x4 = 0; i4x4 < 16; i4x4++ )
            {
                h->zigzagf.sub_4x4( h->dct.luma4x4[i4x4],
                                    h->mb.pic.p_fenc[0]+block_idx_xy_fenc[i4x4],
                                    h->mb.pic.p_fdec[0]+block_idx_xy_fdec[i4x4] );
            }
        }
        else if( h->mb.b_transform_8x8 )
        {
            DECLARE_ALIGNED_16( int16_t dct8x8[4][8][8] );
            b_decimate &= !h->mb.b_trellis; // 8x8 trellis is inherently optimal decimation
            h->dctf.sub16x16_dct8( dct8x8, h->mb.pic.p_fenc[0], h->mb.pic.p_fdec[0] );
            h->nr_count[1] += h->mb.b_noise_reduction * 4;

            for( idx = 0; idx < 4; idx++ )
            {
                if( h->mb.b_noise_reduction )
                    h->quantf.denoise_dct( *dct8x8[idx], h->nr_residual_sum[1], h->nr_offset[1], 64 );
                x264_quant_8x8( h, dct8x8[idx], i_qp, 0, idx );

                h->zigzagf.scan_8x8( h->dct.luma8x8[idx], dct8x8[idx] );

                if( b_decimate )
                {
                    int i_decimate_8x8 = x264_mb_decimate_score( h->dct.luma8x8[idx], 64 );
                    i_decimate_mb += i_decimate_8x8;
                    if( i_decimate_8x8 < 4 )
                        nnz8x8[idx] = 0;
                }
                else
                    nnz8x8[idx] = array_non_zero( dct8x8[idx] );
            }

            if( i_decimate_mb < 6 && b_decimate )
                *(uint32_t*)nnz8x8 = 0;
            else
            {
                for( idx = 0; idx < 4; idx++ )
                    if( nnz8x8[idx] )
                    {
                        h->quantf.dequant_8x8( dct8x8[idx], h->dequant8_mf[CQM_8PY], i_qp );
                        h->dctf.add8x8_idct8( &h->mb.pic.p_fdec[0][(idx&1)*8 + (idx>>1)*8*FDEC_STRIDE], dct8x8[idx] );
                    }
            }
        }
        else
        {
            DECLARE_ALIGNED_16( int16_t dct4x4[16][4][4] );
            h->dctf.sub16x16_dct( dct4x4, h->mb.pic.p_fenc[0], h->mb.pic.p_fdec[0] );
            h->nr_count[0] += h->mb.b_noise_reduction * 16;

            for( i8x8 = 0; i8x8 < 4; i8x8++ )
            {
                int i_decimate_8x8;

                /* encode one 4x4 block */
                i_decimate_8x8 = 0;
                for( i4x4 = 0; i4x4 < 4; i4x4++ )
                {
                    idx = i8x8 * 4 + i4x4;

                    if( h->mb.b_noise_reduction )
                        h->quantf.denoise_dct( *dct4x4[idx], h->nr_residual_sum[0], h->nr_offset[0], 16 );
                    x264_quant_4x4( h, dct4x4[idx], i_qp, DCT_LUMA_4x4, 0, idx );

                    h->zigzagf.scan_4x4( h->dct.luma4x4[idx], dct4x4[idx] );

                    if( b_decimate && i_decimate_8x8 <= 6 )
                        i_decimate_8x8 += x264_mb_decimate_score( h->dct.luma4x4[idx], 16 );
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
                            h->quantf.dequant_4x4( dct4x4[i8x8*4+i], h->dequant4_mf[CQM_4PY], i_qp );
                        h->dctf.add8x8_idct( &h->mb.pic.p_fdec[0][(i8x8&1)*8 + (i8x8>>1)*8*FDEC_STRIDE], &dct4x4[i8x8*4] );
                    }
            }
        }
    }

    /* encode chroma */
    if( IS_INTRA( h->mb.i_type ) )
    {
        const int i_mode = h->mb.i_chroma_pred_mode;

        h->predict_8x8c[i_mode]( h->mb.pic.p_fdec[1] );
        h->predict_8x8c[i_mode]( h->mb.pic.p_fdec[2] );
    }

    /* encode the 8x8 blocks */
    x264_mb_encode_8x8_chroma( h, !IS_INTRA( h->mb.i_type ), h->mb.i_chroma_qp );

    /* coded block pattern and non_zero_count */
    h->mb.i_cbp_luma = 0x00;
    if( h->mb.i_type == I_16x16 )
    {
        for( i = 0; i < 16; i++ )
        {
            int nz = array_non_zero( h->dct.luma4x4[i] );
            h->mb.cache.non_zero_count[x264_scan8[i]] = nz;
            h->mb.i_cbp_luma |= nz;
        }
        h->mb.i_cbp_luma *= 0xf;
    }
    else
    {
        for( i = 0; i < 4; i++)
        {
            if(!nnz8x8[i])
            {
                *(uint16_t*)&h->mb.cache.non_zero_count[x264_scan8[0+i*4]] = 0;
                *(uint16_t*)&h->mb.cache.non_zero_count[x264_scan8[2+i*4]] = 0;
            }
            else if( h->mb.b_transform_8x8 )
            {
                *(uint16_t*)&h->mb.cache.non_zero_count[x264_scan8[0+4*i]] = nnz8x8[i] * 0x0101;
                *(uint16_t*)&h->mb.cache.non_zero_count[x264_scan8[2+4*i]] = nnz8x8[i] * 0x0101;
                h->mb.i_cbp_luma |= nnz8x8[i] << i;
            }
            else
            {
                int nz, cbp = 0;
                for( j = 0; j < 4; j++ )
                {
                    nz = array_non_zero( h->dct.luma4x4[j+4*i] );
                    h->mb.cache.non_zero_count[x264_scan8[j+4*i]] = nz;
                    cbp |= nz;
                }
                h->mb.i_cbp_luma |= cbp << i;
            }
        }
    }

    if( h->param.b_cabac )
    {
        i_cbp_dc = ( h->mb.i_type == I_16x16 && array_non_zero( h->dct.luma16x16_dc ) )
                 | array_non_zero( h->dct.chroma_dc[0] ) << 1
                 | array_non_zero( h->dct.chroma_dc[1] ) << 2;
    }

    /* store cbp */
    h->mb.cbp[h->mb.i_mb_xy] = (i_cbp_dc << 8) | (h->mb.i_cbp_chroma << 4) | h->mb.i_cbp_luma;

    /* Check for P_SKIP
     * XXX: in the me perhaps we should take x264_mb_predict_mv_pskip into account
     *      (if multiple mv give same result)*/
    if( !b_force_no_skip )
    {
        if( h->mb.i_type == P_L0 && h->mb.i_partition == D_16x16 &&
            !(h->mb.i_cbp_luma | h->mb.i_cbp_chroma) &&
            *(uint32_t*)h->mb.cache.mv[0][x264_scan8[0]] == *(uint32_t*)h->mb.cache.pskip_mv
            && h->mb.cache.ref[0][x264_scan8[0]] == 0 )
        {
            h->mb.i_type = P_SKIP;
        }

        /* Check for B_SKIP */
        if( h->mb.i_type == B_DIRECT && !(h->mb.i_cbp_luma | h->mb.i_cbp_chroma) )
        {
            h->mb.i_type = B_SKIP;
        }
    }
}

/*****************************************************************************
 * x264_macroblock_probe_skip:
 *  Check if the current MB could be encoded as a [PB]_SKIP (it supposes you use
 *  the previous QP
 *****************************************************************************/
int x264_macroblock_probe_skip( x264_t *h, const int b_bidir )
{
    DECLARE_ALIGNED_16( int16_t dct4x4[4][4][4] );
    DECLARE_ALIGNED_16( int16_t dct2x2[2][2] );
    DECLARE_ALIGNED_16( int16_t dctscan[16] );

    int i_qp = h->mb.i_qp;
    int mvp[2] = {0,0};
    int ch, thresh;

    int i8x8, i4x4;
    int i_decimate_mb;

    if( !b_bidir )
    {
        /* Get the MV */
        mvp[0] = x264_clip3( h->mb.cache.pskip_mv[0], h->mb.mv_min[0], h->mb.mv_max[0] );
        mvp[1] = x264_clip3( h->mb.cache.pskip_mv[1], h->mb.mv_min[1], h->mb.mv_max[1] );

        /* Motion compensation */
        h->mc.mc_luma( h->mb.pic.p_fdec[0],    FDEC_STRIDE,
                       h->mb.pic.p_fref[0][0], h->mb.pic.i_stride[0],
                       mvp[0], mvp[1], 16, 16 );
    }

    for( i8x8 = 0, i_decimate_mb = 0; i8x8 < 4; i8x8++ )
    {
        int fenc_offset = (i8x8&1) * 8 + (i8x8>>1) * FENC_STRIDE * 8;
        int fdec_offset = (i8x8&1) * 8 + (i8x8>>1) * FDEC_STRIDE * 8;
        /* get luma diff */
        h->dctf.sub8x8_dct( dct4x4, h->mb.pic.p_fenc[0] + fenc_offset,
                                    h->mb.pic.p_fdec[0] + fdec_offset );
        /* encode one 4x4 block */
        for( i4x4 = 0; i4x4 < 4; i4x4++ )
        {
            h->quantf.quant_4x4( dct4x4[i4x4], h->quant4_mf[CQM_4PY][i_qp], h->quant4_bias[CQM_4PY][i_qp] );
            if( !array_non_zero(dct4x4[i4x4]) )
                continue;
            h->zigzagf.scan_4x4( dctscan, dct4x4[i4x4] );
            i_decimate_mb += x264_mb_decimate_score( dctscan, 16 );
            if( i_decimate_mb >= 6 )
                return 0;
        }
    }

    /* encode chroma */
    i_qp = h->mb.i_chroma_qp;
    thresh = (x264_lambda2_tab[i_qp] + 32) >> 6;

    for( ch = 0; ch < 2; ch++ )
    {
        uint8_t  *p_src = h->mb.pic.p_fenc[1+ch];
        uint8_t  *p_dst = h->mb.pic.p_fdec[1+ch];

        if( !b_bidir )
        {
            h->mc.mc_chroma( h->mb.pic.p_fdec[1+ch],       FDEC_STRIDE,
                             h->mb.pic.p_fref[0][0][4+ch], h->mb.pic.i_stride[1+ch],
                             mvp[0], mvp[1], 8, 8 );
        }

        /* there is almost never a termination during chroma, but we can't avoid the check entirely */
        /* so instead we check SSD and skip the actual check if the score is low enough. */
        if( h->pixf.ssd[PIXEL_8x8]( p_dst, FDEC_STRIDE, p_src, FENC_STRIDE ) < thresh )
            continue;

        h->dctf.sub8x8_dct( dct4x4, p_src, p_dst );

        /* calculate dct DC */
        dct2x2[0][0] = dct4x4[0][0][0];
        dct2x2[0][1] = dct4x4[1][0][0];
        dct2x2[1][0] = dct4x4[2][0][0];
        dct2x2[1][1] = dct4x4[3][0][0];
        h->dctf.dct2x2dc( dct2x2 );
        h->quantf.quant_2x2_dc( dct2x2, h->quant4_mf[CQM_4PC][i_qp][0]>>1, h->quant4_bias[CQM_4PC][i_qp][0]<<1 );
        if( array_non_zero(dct2x2) )
            return 0;

        /* calculate dct coeffs */
        for( i4x4 = 0, i_decimate_mb = 0; i4x4 < 4; i4x4++ )
        {
            h->quantf.quant_4x4( dct4x4[i4x4], h->quant4_mf[CQM_4PC][i_qp], h->quant4_bias[CQM_4PC][i_qp] );
            if( !array_non_zero(dct4x4[i4x4]) )
                continue;
            h->zigzagf.scan_4x4( dctscan, dct4x4[i4x4] );
            i_decimate_mb += x264_mb_decimate_score( dctscan+1, 15 );
            if( i_decimate_mb >= 7 )
                return 0;
        }
    }

    h->mb.b_skip_mc = 1;
    return 1;
}

/****************************************************************************
 * DCT-domain noise reduction / adaptive deadzone
 * from libavcodec
 ****************************************************************************/

void x264_noise_reduction_update( x264_t *h )
{
    int cat, i;
    for( cat = 0; cat < 2; cat++ )
    {
        int size = cat ? 64 : 16;
        const uint16_t *weight = cat ? x264_dct8_weight2_tab : x264_dct4_weight2_tab;

        if( h->nr_count[cat] > (cat ? (1<<16) : (1<<18)) )
        {
            for( i = 0; i < size; i++ )
                h->nr_residual_sum[cat][i] >>= 1;
            h->nr_count[cat] >>= 1;
        }

        for( i = 0; i < size; i++ )
            h->nr_offset[cat][i] =
                ((uint64_t)h->param.analyse.i_noise_reduction * h->nr_count[cat]
                 + h->nr_residual_sum[cat][i]/2)
              / ((uint64_t)h->nr_residual_sum[cat][i] * weight[i]/256 + 1);
    }
}

/*****************************************************************************
 * RD only; 4 calls to this do not make up for one macroblock_encode.
 * doesn't transform chroma dc.
 *****************************************************************************/
void x264_macroblock_encode_p8x8( x264_t *h, int i8 )
{
    int i_qp = h->mb.i_qp;
    uint8_t *p_fenc = h->mb.pic.p_fenc[0] + (i8&1)*8 + (i8>>1)*8*FENC_STRIDE;
    uint8_t *p_fdec = h->mb.pic.p_fdec[0] + (i8&1)*8 + (i8>>1)*8*FDEC_STRIDE;
    int b_decimate = h->sh.i_type == SLICE_TYPE_B || h->param.analyse.b_dct_decimate;
    int nnz8x8 = 0;
    int ch;

    x264_mb_mc_8x8( h, i8 );

    if( h->mb.b_lossless )
    {
        int i4;
        for( i4 = i8*4; i4 < i8*4+4; i4++ )
        {
            h->zigzagf.sub_4x4( h->dct.luma4x4[i4],
                                h->mb.pic.p_fenc[0]+block_idx_xy_fenc[i4],
                                h->mb.pic.p_fdec[0]+block_idx_xy_fdec[i4] );
            nnz8x8 |= array_non_zero( h->dct.luma4x4[i4] );
        }
        for( ch = 0; ch < 2; ch++ )
        {
            p_fenc = h->mb.pic.p_fenc[1+ch] + (i8&1)*4 + (i8>>1)*4*FENC_STRIDE;
            p_fdec = h->mb.pic.p_fdec[1+ch] + (i8&1)*4 + (i8>>1)*4*FDEC_STRIDE;
            h->zigzagf.sub_4x4( h->dct.luma4x4[16+i8+ch*4], p_fenc, p_fdec );
            h->dct.luma4x4[16+i8+ch*4][0] = 0;
        }
    }
    else
    {
        if( h->mb.b_transform_8x8 )
        {
            DECLARE_ALIGNED_16( int16_t dct8x8[8][8] );
            h->dctf.sub8x8_dct8( dct8x8, p_fenc, p_fdec );
            x264_quant_8x8( h, dct8x8, i_qp, 0, i8 );
            h->zigzagf.scan_8x8( h->dct.luma8x8[i8], dct8x8 );

            if( b_decimate && !h->mb.b_trellis )
                nnz8x8 = 4 <= x264_mb_decimate_score( h->dct.luma8x8[i8], 64 );
            else
                nnz8x8 = array_non_zero( dct8x8 );

            if( nnz8x8 )
            {
                h->quantf.dequant_8x8( dct8x8, h->dequant8_mf[CQM_8PY], i_qp );
                h->dctf.add8x8_idct8( p_fdec, dct8x8 );
            }
        }
        else
        {
            int i4;
            DECLARE_ALIGNED_16( int16_t dct4x4[4][4][4] );
            h->dctf.sub8x8_dct( dct4x4, p_fenc, p_fdec );
            for( i4 = 0; i4 < 4; i4++ )
                x264_quant_4x4( h, dct4x4[i4], i_qp, DCT_LUMA_4x4, 0, i8*4+i4 );

            for( i4 = 0; i4 < 4; i4++ )
                h->zigzagf.scan_4x4( h->dct.luma4x4[i8*4+i4], dct4x4[i4] );

            if( b_decimate )
            {
                int i_decimate_8x8 = 0;
                for( i4 = 0; i4 < 4 && i_decimate_8x8 < 4; i4++ )
                    i_decimate_8x8 += x264_mb_decimate_score( h->dct.luma4x4[i8*4+i4], 16 );
                nnz8x8 = 4 <= i_decimate_8x8;
            }
            else
                nnz8x8 = array_non_zero( dct4x4 );

            if( nnz8x8 )
            {
                for( i4 = 0; i4 < 4; i4++ )
                    h->quantf.dequant_4x4( dct4x4[i4], h->dequant4_mf[CQM_4PY], i_qp );
                h->dctf.add8x8_idct( p_fdec, dct4x4 );
            }
        }

        i_qp = h->mb.i_chroma_qp;

        for( ch = 0; ch < 2; ch++ )
        {
            DECLARE_ALIGNED_16( int16_t dct4x4[4][4] );
            p_fenc = h->mb.pic.p_fenc[1+ch] + (i8&1)*4 + (i8>>1)*4*FENC_STRIDE;
            p_fdec = h->mb.pic.p_fdec[1+ch] + (i8&1)*4 + (i8>>1)*4*FDEC_STRIDE;

            h->dctf.sub4x4_dct( dct4x4, p_fenc, p_fdec );
            h->quantf.quant_4x4( dct4x4, h->quant4_mf[CQM_4PC][i_qp], h->quant4_bias[CQM_4PC][i_qp] );
            h->zigzagf.scan_4x4( h->dct.luma4x4[16+i8+ch*4], dct4x4 );
            h->dct.luma4x4[16+i8+ch*4][0] = 0;
            if( array_non_zero( dct4x4 ) )
            {
                h->quantf.dequant_4x4( dct4x4, h->dequant4_mf[CQM_4PC], i_qp );
                h->dctf.add4x4_idct( p_fdec, dct4x4 );
            }
        }
    }
    h->mb.i_cbp_luma &= ~(1 << i8);
    h->mb.i_cbp_luma |= nnz8x8 << i8;
    h->mb.i_cbp_chroma = 0x02;
}

#ifdef TPC

static ALWAYS_INLINE void x264_quant_4x4_tpc( x264_t *h, int16_t dct[4][4], int i_qp, int i_ctxBlockCat, int b_intra, int idx, x264_mb_t *mb )
{
    int i_quant_cat = b_intra ? CQM_4IY : CQM_4PY;
    if( mb->b_trellis )
        x264_quant_4x4_trellis_tpc( h, dct, i_quant_cat, i_qp, i_ctxBlockCat, b_intra, idx, mb );
    else
        h->quantf.quant_4x4( dct, h->quant4_mf[i_quant_cat][i_qp], h->quant4_bias[i_quant_cat][i_qp] );
}

static ALWAYS_INLINE void x264_quant_8x8_tpc( x264_t *h, int16_t dct[8][8], int i_qp, int b_intra, int idx, x264_mb_t *mb )
{
    int i_quant_cat = b_intra ? CQM_8IY : CQM_8PY;
    if( mb->b_trellis )
        x264_quant_8x8_trellis_tpc( h, dct, i_quant_cat, i_qp, b_intra, idx, mb );
    else
        h->quantf.quant_8x8( dct, h->quant8_mf[i_quant_cat][i_qp], h->quant8_bias[i_quant_cat][i_qp] );
}




void x264_mb_encode_i4x4_tpc( x264_t *h, int idx, int i_qp, x264_mb_t *mb, x264_dct_t *dct )
{
    uint8_t *p_src = &mb->pic.p_fenc[0][block_idx_xy_fenc[idx]];
    uint8_t *p_dst = &mb->pic.p_fdec[0][block_idx_xy_fdec[idx]];
    DECLARE_ALIGNED_16( int16_t dct4x4[4][4] );

    if( mb->b_lossless )
    {
        h->zigzagf.sub_4x4( dct->luma4x4[idx], p_src, p_dst );
        return;
    }

    h->dctf.sub4x4_dct( dct4x4, p_src, p_dst );

    x264_quant_4x4_tpc( h, dct4x4, i_qp, DCT_LUMA_4x4, 1, idx, mb );

    if( array_non_zero( dct4x4 ) )
    {
        h->zigzagf.scan_4x4( dct->luma4x4[idx], dct4x4 );
        h->quantf.dequant_4x4( dct4x4, h->dequant4_mf[CQM_4IY], i_qp );

        /* output samples to fdec */
        h->dctf.add4x4_idct( p_dst, dct4x4 );
    }
    else
        memset( dct->luma4x4[idx], 0, sizeof(dct->luma4x4[idx]));
}

void x264_mb_encode_i8x8_tpc( x264_t *h, int idx, int i_qp, x264_mb_t *mb, x264_dct_t *dct )
{
    int x = 8 * (idx&1);
    int y = 8 * (idx>>1);
    uint8_t *p_src = &mb->pic.p_fenc[0][x+y*FENC_STRIDE];
    uint8_t *p_dst = &mb->pic.p_fdec[0][x+y*FDEC_STRIDE];
    DECLARE_ALIGNED_16( int16_t dct8x8[8][8] );

    h->dctf.sub8x8_dct8( dct8x8, p_src, p_dst );

    x264_quant_8x8_tpc( h, dct8x8, i_qp, 1, idx, mb );

    h->zigzagf.scan_8x8( dct->luma8x8[idx], dct8x8 );
    h->quantf.dequant_8x8( dct8x8, h->dequant8_mf[CQM_8IY], i_qp );
    h->dctf.add8x8_idct8( p_dst, dct8x8 );
}


static void x264_mb_encode_i16x16_tpc( x264_t *h, int i_qp, x264_mb_t *mb, x264_dct_t *dct )
{
    uint8_t  *p_src = mb->pic.p_fenc[0];
    uint8_t  *p_dst = mb->pic.p_fdec[0];

    DECLARE_ALIGNED_16( int16_t dct4x4[16][4][4] );
    DECLARE_ALIGNED_16( int16_t dct_dc4x4[4][4] );

    int i;

    if( mb->b_lossless )
    {
        for( i = 0; i < 16; i++ )
        {
            int oe = block_idx_xy_fenc[i];
            int od = block_idx_xy_fdec[i];
            h->zigzagf.sub_4x4( dct->luma4x4[i], p_src+oe, p_dst+od );
            dct_dc4x4[0][block_idx_yx_1d[i]] = dct->luma4x4[i][0];
            dct->luma4x4[i][0] = 0;
        }
        h->zigzagf.scan_4x4( dct->luma16x16_dc, dct_dc4x4 );
        return;
    }

    h->dctf.sub16x16_dct( dct4x4, p_src, p_dst );
    for( i = 0; i < 16; i++ )
    {
        /* copy dc coeff */
        dct_dc4x4[0][block_idx_xy_1d[i]] = dct4x4[i][0][0];
        dct4x4[i][0][0] = 0;

        /* quant/scan/dequant */
        x264_quant_4x4_tpc( h, dct4x4[i], i_qp, DCT_LUMA_AC, 1, i, mb );

        h->zigzagf.scan_4x4( dct->luma4x4[i], dct4x4[i] );
        h->quantf.dequant_4x4( dct4x4[i], h->dequant4_mf[CQM_4IY], i_qp );
    }

    h->dctf.dct4x4dc( dct_dc4x4 );
    h->quantf.quant_4x4_dc( dct_dc4x4, h->quant4_mf[CQM_4IY][i_qp][0]>>1, h->quant4_bias[CQM_4IY][i_qp][0]<<1 );
    h->zigzagf.scan_4x4( dct->luma16x16_dc, dct_dc4x4 );

    /* output samples to fdec */
    h->dctf.idct4x4dc( dct_dc4x4 );
    x264_mb_dequant_4x4_dc( dct_dc4x4, h->dequant4_mf[CQM_4IY], i_qp );  /* XXX not inversed */

    /* calculate dct coeffs */
    for( i = 0; i < 16; i++ )
    {
        /* copy dc coeff */
        dct4x4[i][0][0] = dct_dc4x4[0][block_idx_xy_1d[i]];
    }
    /* put pixels to fdec */
    h->dctf.add16x16_idct( p_dst, dct4x4 );
}

void x264_mb_encode_8x8_chroma_tpc( x264_t *h, int b_inter, int i_qp, x264_mb_t *mb, x264_dct_t *dct )
{
    int i, ch;
    int b_decimate = b_inter && (h->sh.i_type == SLICE_TYPE_B || h->param.analyse.b_dct_decimate);

    for( ch = 0; ch < 2; ch++ )
    {
        uint8_t  *p_src = mb->pic.p_fenc[1+ch];
        uint8_t  *p_dst = mb->pic.p_fdec[1+ch];
        int i_decimate_score = 0;

        DECLARE_ALIGNED_16( int16_t dct2x2[2][2]  );
        DECLARE_ALIGNED_16( int16_t dct4x4[4][4][4] );

        if( mb->b_lossless )
        {
            for( i = 0; i < 4; i++ )
            {
                int oe = block_idx_x[i]*4 + block_idx_y[i]*4*FENC_STRIDE;
                int od = block_idx_x[i]*4 + block_idx_y[i]*4*FDEC_STRIDE;
                h->zigzagf.sub_4x4( dct->luma4x4[16+i+ch*4], p_src+oe, p_dst+od );
                dct->chroma_dc[ch][i] = dct->luma4x4[16+i+ch*4][0];
                dct->luma4x4[16+i+ch*4][0] = 0;
            }
            continue;
        }

        h->dctf.sub8x8_dct( dct4x4, p_src, p_dst );
        /* calculate dct coeffs */
        for( i = 0; i < 4; i++ )
        {
            /* copy dc coeff */
            dct2x2[i>>1][i&1] = dct4x4[i][0][0];
            dct4x4[i][0][0] = 0;

            /* no trellis; it doesn't seem to help chroma noticeably */
            h->quantf.quant_4x4( dct4x4[i], h->quant4_mf[CQM_4IC+b_inter][i_qp], h->quant4_bias[CQM_4IC+b_inter][i_qp] );
            h->zigzagf.scan_4x4( dct->luma4x4[16+i+ch*4], dct4x4[i] );

            if( b_decimate )
                i_decimate_score += x264_mb_decimate_score( dct->luma4x4[16+i+ch*4]+1, 15 );
        }

        h->dctf.dct2x2dc( dct2x2 );
        h->quantf.quant_2x2_dc( dct2x2, h->quant4_mf[CQM_4IC+b_inter][i_qp][0]>>1, h->quant4_bias[CQM_4IC+b_inter][i_qp][0]<<1 );
        zigzag_scan_2x2_dc( dct->chroma_dc[ch], dct2x2 );

        /* output samples to fdec */
        h->dctf.idct2x2dc( dct2x2 );
        x264_mb_dequant_2x2_dc( dct2x2, h->dequant4_mf[CQM_4IC + b_inter], i_qp );  /* XXX not inversed */

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
                h->quantf.dequant_4x4( dct4x4[i], h->dequant4_mf[CQM_4IC + b_inter], i_qp );
        }
        dct4x4[0][0][0] = dct2x2[0][0];
        dct4x4[1][0][0] = dct2x2[0][1];
        dct4x4[2][0][0] = dct2x2[1][0];
        dct4x4[3][0][0] = dct2x2[1][1];
        h->dctf.add8x8_idct( p_dst, dct4x4 );
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
 * x264_macroblock_probe_skip_tpc:
 *  Check if the current MB could be encoded as a [PB]_SKIP (it supposes you use
 *  the previous QP
 *****************************************************************************/
int x264_macroblock_probe_skip_tpc( x264_t *h, const int b_bidir, x264_mb_t *mb )
{
    DECLARE_ALIGNED_16( int16_t dct4x4[4][4][4] );
    DECLARE_ALIGNED_16( int16_t dct2x2[2][2] );
    DECLARE_ALIGNED_16( int16_t dctscan[16] );

    int i_qp = mb->i_qp;
    int mvp[2]={0,0};
    int ch, thresh;
    int i8x8, i4x4;
    int i_decimate_mb;

    if( !b_bidir )
    {
        /* Get the MV */
        mvp[0] = x264_clip3( mb->cache.pskip_mv[0], mb->mv_min[0], mb->mv_max[0] );
        mvp[1] = x264_clip3( mb->cache.pskip_mv[1], mb->mv_min[1], mb->mv_max[1] );

        /* Motion compensation */
        h->mc.mc_luma( mb->pic.p_fdec[0],    FDEC_STRIDE,
                       mb->pic.p_fref[0][0], mb->pic.i_stride[0],
                       mvp[0], mvp[1], 16, 16 );
    }

    for( i8x8 = 0, i_decimate_mb = 0; i8x8 < 4; i8x8++ )
    {
        int fenc_offset = (i8x8&1) * 8 + (i8x8>>1) * FENC_STRIDE * 8;
        int fdec_offset = (i8x8&1) * 8 + (i8x8>>1) * FDEC_STRIDE * 8;
        /* get luma diff */
        h->dctf.sub8x8_dct( dct4x4, mb->pic.p_fenc[0] + fenc_offset,
                                    mb->pic.p_fdec[0] + fdec_offset );
        /* encode one 4x4 block */
        for( i4x4 = 0; i4x4 < 4; i4x4++ )
        {
            h->quantf.quant_4x4( dct4x4[i4x4], h->quant4_mf[CQM_4PY][i_qp], h->quant4_bias[CQM_4PY][i_qp] );
            if( !array_non_zero(dct4x4[i4x4]) )
                continue;
            h->zigzagf.scan_4x4( dctscan, dct4x4[i4x4] );
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
            h->mc.mc_chroma( mb->pic.p_fdec[1+ch],       FDEC_STRIDE,
                             mb->pic.p_fref[0][0][4+ch], mb->pic.i_stride[1+ch],
                             mvp[0], mvp[1], 8, 8 );
        }

        /* there is almost never a termination during chroma, but we can't avoid the check entirely */
        /* so instead we check SSD and skip the actual check if the score is low enough. */
        if( h->pixf.ssd[PIXEL_8x8]( p_dst, FDEC_STRIDE, p_src, FENC_STRIDE ) < thresh )
            continue;

        h->dctf.sub8x8_dct( dct4x4, p_src, p_dst );

        /* calculate dct DC */
        dct2x2[0][0] = dct4x4[0][0][0];
        dct2x2[0][1] = dct4x4[1][0][0];
        dct2x2[1][0] = dct4x4[2][0][0];
        dct2x2[1][1] = dct4x4[3][0][0];
        h->dctf.dct2x2dc( dct2x2 );
        h->quantf.quant_2x2_dc( dct2x2, h->quant4_mf[CQM_4PC][i_qp][0]>>1, h->quant4_bias[CQM_4PC][i_qp][0]<<1 );
        if( array_non_zero(dct2x2) )
            return 0;

        /* calculate dct coeffs */
        for( i4x4 = 0, i_decimate_mb = 0; i4x4 < 4; i4x4++ )
        {
            h->quantf.quant_4x4( dct4x4[i4x4], h->quant4_mf[CQM_4PC][i_qp], h->quant4_bias[CQM_4PC][i_qp] );
            if( !array_non_zero(dct4x4[i4x4]) )
                continue;
            h->zigzagf.scan_4x4( dctscan, dct4x4[i4x4] );
            i_decimate_mb += x264_mb_decimate_score( dctscan+1, 15 );
            if( i_decimate_mb >= 7 )
                return 0;
        }
    }

    mb->b_skip_mc = 1;
    return 1;
}

static void x264_macroblock_encode_skip_tpc( x264_t *h, x264_mb_t *mb )
{
    mb->i_cbp_luma = 0x00;
    mb->i_cbp_chroma = 0x00;
    memset( mb->cache.non_zero_count, 0, X264_SCAN8_SIZE );
    /* store cbp */
    mb->cbp[mb->i_mb_xy] = 0;
}

/*****************************************************************************
 * x264_macroblock_encode_pskip:
 *  Encode an already marked skip block
 *****************************************************************************/
static void x264_macroblock_encode_pskip_tpc( x264_t *h, x264_mb_t *mb )
{
    const int mvx = x264_clip3( mb->cache.mv[0][x264_scan8[0]][0],
                                mb->mv_min[0], mb->mv_max[0] );
    const int mvy = x264_clip3( mb->cache.mv[0][x264_scan8[0]][1],
                                mb->mv_min[1], mb->mv_max[1] );

    /* don't do pskip motion compensation if it was already done in macroblock_analyse */
    if( !mb->b_skip_mc )
    {
        h->mc.mc_luma( mb->pic.p_fdec[0],    FDEC_STRIDE,
                       mb->pic.p_fref[0][0], mb->pic.i_stride[0],
                       mvx, mvy, 16, 16 );

        h->mc.mc_chroma( mb->pic.p_fdec[1],       FDEC_STRIDE,
                         mb->pic.p_fref[0][0][4], mb->pic.i_stride[1],
                         mvx, mvy, 8, 8 );

        h->mc.mc_chroma( mb->pic.p_fdec[2],       FDEC_STRIDE,
                         mb->pic.p_fref[0][0][5], mb->pic.i_stride[2],
                         mvx, mvy, 8, 8 );
    }

    x264_macroblock_encode_skip_tpc( h, mb );
}
/*****************************************************************************
 * x264_macroblock_encode:
 *****************************************************************************/
void x264_macroblock_encode_tpc( x264_t *h, x264_mb_t *mb, x264_dct_t *dct )
{
    int i_cbp_dc = 0;
    int i_qp = mb->i_qp;
    int b_decimate = h->sh.i_type == SLICE_TYPE_B || h->param.analyse.b_dct_decimate;
    int b_force_no_skip = 0;
    int i,j,idx;
    uint8_t nnz8x8[4] = {1,1,1,1};

    if( h->sh.b_mbaff
        && mb->i_mb_xy == h->sh.i_first_mb + mb->i_mb_stride
        && IS_SKIP(mb->type[h->sh.i_first_mb]) )
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
        x264_macroblock_encode_pskip_tpc( h, mb );
        return;
    }
    if( mb->i_type == B_SKIP )
    {
        /* don't do bskip motion compensation if it was already done in macroblock_analyse */
        if( !mb->b_skip_mc )
            x264_mb_mc_tpc( h, mb );
        x264_macroblock_encode_skip_tpc( h, mb );
        return;
    }

    if( mb->i_type == I_16x16 )
    {
        
        const int i_mode = mb->i_intra16x16_pred_mode;
	mb->b_transform_8x8 = 0;
        /* do the right prediction */
        h->predict_16x16[i_mode]( mb->pic.p_fdec[0] );

        /* encode the 16x16 macroblock */
        x264_mb_encode_i16x16_tpc( h, i_qp, mb,dct );
    }
    else if( mb->i_type == I_8x8 )
    {
	assert(  mb->i_type != I_8x8  ); // not yet!
        DECLARE_ALIGNED_16( uint8_t edge[33] );
        mb->b_transform_8x8 = 1;
        /* If we already encoded 3 of the 4 i8x8 blocks, we don't have to do them again. */
        if( mb->i_skip_intra )
        {
            h->mc.copy[PIXEL_16x16]( mb->pic.p_fdec[0], FDEC_STRIDE, mb->pic.i8x8_fdec_buf, 16, 16 );
            /* In RD mode, restore the now-overwritten DCT data. */
            if( mb->i_skip_intra == 2 )
                h->mc.memcpy_aligned( dct->luma8x8, mb->pic.i8x8_dct_buf, sizeof(mb->pic.i8x8_dct_buf) );
        }
        for( i = mb->i_skip_intra ? 3 : 0 ; i < 4; i++ )
        {
            uint8_t  *p_dst = &mb->pic.p_fdec[0][8 * (i&1) + 8 * (i>>1) * FDEC_STRIDE];
            int      i_mode = mb->cache.intra4x4_pred_mode[x264_scan8[4*i]];

            x264_predict_8x8_filter( p_dst, edge, mb->i_neighbour8[i], x264_pred_i4x4_neighbors[i_mode] );
            h->predict_8x8[i_mode]( p_dst, edge );
            x264_mb_encode_i8x8_tpc( h, i, i_qp, mb, dct );
        }
        for( i = 0; i < 4; i++ )
            nnz8x8[i] = array_non_zero( dct->luma8x8[i] );
    }
    else if( mb->i_type == I_4x4 )
    {
        assert(  mb->i_type != I_4x4  ); // Not yet
        mb->b_transform_8x8 = 0;
        /* If we already encoded 15 of the 16 i4x4 blocks, we don't have to do them again. */
        if( mb->i_skip_intra )
        {
            h->mc.copy[PIXEL_16x16]( mb->pic.p_fdec[0], FDEC_STRIDE, mb->pic.i4x4_fdec_buf, 16, 16 );
            /* In RD mode, restore the now-overwritten DCT data. */
            if( mb->i_skip_intra == 2 )
                h->mc.memcpy_aligned( dct->luma4x4, mb->pic.i4x4_dct_buf, sizeof(mb->pic.i4x4_dct_buf) );
        }
        for( i = mb->i_skip_intra ? 15 : 0 ; i < 16; i++ )
        {
            uint8_t  *p_dst = &mb->pic.p_fdec[0][block_idx_xy_fdec[i]];
            int      i_mode = mb->cache.intra4x4_pred_mode[x264_scan8[i]];

            if( (mb->i_neighbour4[i] & (MB_TOPRIGHT|MB_TOP)) == MB_TOP )
                /* emulate missing topright samples */
                *(uint32_t*) &p_dst[4-FDEC_STRIDE] = p_dst[3-FDEC_STRIDE] * 0x01010101U;

            h->predict_4x4[i_mode]( p_dst );
            x264_mb_encode_i4x4_tpc( h, i, i_qp, mb, dct );
        }
    }
    else    /* Inter MB */
    {
        int i8x8, i4x4;
        int i_decimate_mb = 0;

        /* Don't repeat motion compensation if it was already done in non-RD transform analysis */
        if( !mb->b_skip_mc )
            x264_mb_mc_tpc( h, mb );

        if( mb->b_lossless )
        {
            assert( !(mb->b_lossless));
            for( i4x4 = 0; i4x4 < 16; i4x4++ )
            {
                h->zigzagf.sub_4x4( dct->luma4x4[i4x4],
                                    mb->pic.p_fenc[0]+block_idx_xy_fenc[i4x4],
                                    mb->pic.p_fdec[0]+block_idx_xy_fdec[i4x4] );
            }
        }
        else if( mb->b_transform_8x8 )
        {
	    assert( !( mb->b_transform_8x8 ) );
            DECLARE_ALIGNED_16( int16_t dct8x8[4][8][8] );
            b_decimate &= !mb->b_trellis; // 8x8 trellis is inherently optimal decimation
            h->dctf.sub16x16_dct8( dct8x8, mb->pic.p_fenc[0], mb->pic.p_fdec[0] );
            h->nr_count[1] += mb->b_noise_reduction * 4;

            for( idx = 0; idx < 4; idx++ )
            {
                if( mb->b_noise_reduction )
                    h->quantf.denoise_dct( *dct8x8[idx], h->nr_residual_sum[1], h->nr_offset[1], 64 );
                x264_quant_8x8_tpc( h, dct8x8[idx], i_qp, 0, idx, mb );

                h->zigzagf.scan_8x8( dct->luma8x8[idx], dct8x8[idx] );

                if( b_decimate )
                {
                    int i_decimate_8x8 = x264_mb_decimate_score( dct->luma8x8[idx], 64 );
                    i_decimate_mb += i_decimate_8x8;
                    if( i_decimate_8x8 < 4 )
                        nnz8x8[idx] = 0;
                }
                else
                    nnz8x8[idx] = array_non_zero( dct8x8[idx] );
            }

            if( i_decimate_mb < 6 && b_decimate )
                *(uint32_t*)nnz8x8 = 0;
            else
            {
                for( idx = 0; idx < 4; idx++ )
                    if( nnz8x8[idx] )
                    {
                        h->quantf.dequant_8x8( dct8x8[idx], h->dequant8_mf[CQM_8PY], i_qp );
                        h->dctf.add8x8_idct8( &mb->pic.p_fdec[0][(idx&1)*8 + (idx>>1)*8*FDEC_STRIDE], dct8x8[idx] );
                    }
            }
        }
        else
        {
            DECLARE_ALIGNED_16( int16_t dct4x4[16][4][4] );
            h->dctf.sub16x16_dct( dct4x4, mb->pic.p_fenc[0], mb->pic.p_fdec[0] );
            h->nr_count[0] += mb->b_noise_reduction * 16;
            for( i8x8 = 0; i8x8 < 4; i8x8++ )
            {
                int i_decimate_8x8;

                /* encode one 4x4 block */
                i_decimate_8x8 = 0;
                for( i4x4 = 0; i4x4 < 4; i4x4++ )
                {
                    idx = i8x8 * 4 + i4x4;

                    if( mb->b_noise_reduction )
                        h->quantf.denoise_dct( *dct4x4[idx], h->nr_residual_sum[0], h->nr_offset[0], 16 );
                    x264_quant_4x4_tpc( h, dct4x4[idx], i_qp, DCT_LUMA_4x4, 0, idx, mb );

                  h->zigzagf.scan_4x4( dct->luma4x4[idx], dct4x4[idx] );

                    if( b_decimate && i_decimate_8x8 <= 6 )
                        i_decimate_8x8 += x264_mb_decimate_score( dct->luma4x4[idx], 16 );
                }

                /* decimate this 8x8 block */
                i_decimate_mb += i_decimate_8x8;
                if( i_decimate_8x8 < 4 && b_decimate )
                    nnz8x8[i8x8] = 0;
            }
#if 0
{
	puts(" PPU  ");
	int i,j,k;
	char *tmp = mb->pic.p_fdec[0];
	for (i=0;i<384;i++){
		printf(" 0x%x", tmp[i] );
		if ( !(i%16) ) printf("\n");
	}
	puts("\n  ---  ");
}
#endif

            if( i_decimate_mb < 6 && b_decimate )
                *(uint32_t*)nnz8x8 = 0;
            else
            {
                for( i8x8 = 0; i8x8 < 4; i8x8++ )
                    if( nnz8x8[i8x8] )
                    {
                        for( i = 0; i < 4; i++ )
                            h->quantf.dequant_4x4( dct4x4[i8x8*4+i], h->dequant4_mf[CQM_4PY], i_qp );
                        h->dctf.add8x8_idct( &mb->pic.p_fdec[0][(i8x8&1)*8 + (i8x8>>1)*8*FDEC_STRIDE], &dct4x4[i8x8*4] );
                    }
            }



        }
    }

    /* encode chroma */
    if( IS_INTRA( mb->i_type ) )
    {
        const int i_mode = mb->i_chroma_pred_mode;

        h->predict_8x8c[i_mode]( mb->pic.p_fdec[1] );
        h->predict_8x8c[i_mode]( mb->pic.p_fdec[2] );
    }

    /* encode the 8x8 blocks */
    x264_mb_encode_8x8_chroma_tpc( h, !IS_INTRA( mb->i_type ), mb->i_chroma_qp, mb, dct );

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

    if( h->param.b_cabac )
    {
        i_cbp_dc = ( mb->i_type == I_16x16 && array_non_zero( dct->luma16x16_dc ) )
                 | array_non_zero( dct->chroma_dc[0] ) << 1
                 | array_non_zero( dct->chroma_dc[1] ) << 2;
    }

    /* store cbp */
    mb->cbp[mb->i_mb_xy] = (i_cbp_dc << 8) | (mb->i_cbp_chroma << 4) | mb->i_cbp_luma;

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

/*****************************************************************************
 * x264_macroblock_encode2: FOR TESTS ONLY
 *****************************************************************************/

void x264_macroblock_encode_tpc2( x264_t *h, x264_mb_t *mb, x264_dct_t *dct )
{
    int i_cbp_dc = 0;
    int b_force_no_skip = 0;
    int i,j;
    uint8_t nnz8x8[4] = {1,1,1,1};

    if( h->sh.b_mbaff
        && mb->i_mb_xy == h->sh.i_first_mb + mb->i_mb_stride
        && IS_SKIP(mb->type[h->sh.i_first_mb]) )
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
        x264_macroblock_encode_pskip_tpc( h, mb );
        return;
    }
    if( mb->i_type == B_SKIP )
    {
        /* don't do bskip motion compensation if it was already done in macroblock_analyse */
        if( !mb->b_skip_mc )
            x264_mb_mc_tpc( h, mb );
        x264_macroblock_encode_skip_tpc( h, mb );
        return;
    }

#if 0
    if( mb->i_type == I_16x16 )
    {
        
        const int i_mode = mb->i_intra16x16_pred_mode;
	mb->b_transform_8x8 = 0;
        /* do the right prediction */
        h->predict_16x16[i_mode]( mb->pic.p_fdec[0] );

        /* encode the 16x16 macroblock */
        x264_mb_encode_i16x16_tpc( h, i_qp, mb,dct );
    }
    else    /* Inter MB */
    {
        int i8x8, i4x4;
        int i_decimate_mb = 0;

        /* Don't repeat motion compensation if it was already done in non-RD transform analysis */
        if( !mb->b_skip_mc )
            x264_mb_mc_tpc( h, mb );

        {
            DECLARE_ALIGNED_16( int16_t dct4x4[16][4][4] );
            h->dctf.sub16x16_dct( dct4x4, mb->pic.p_fenc[0], mb->pic.p_fdec[0] );
            h->nr_count[0] += mb->b_noise_reduction * 16;
            for( i8x8 = 0; i8x8 < 4; i8x8++ )
            {
                int i_decimate_8x8;

                /* encode one 4x4 block */
                i_decimate_8x8 = 0;
                for( i4x4 = 0; i4x4 < 4; i4x4++ )
                {
                    idx = i8x8 * 4 + i4x4;

                    if( mb->b_noise_reduction )
                        h->quantf.denoise_dct( *dct4x4[idx], h->nr_residual_sum[0], h->nr_offset[0], 16 );
                    x264_quant_4x4_tpc( h, dct4x4[idx], i_qp, DCT_LUMA_4x4, 0, idx, mb );

                  h->zigzagf.scan_4x4( dct->luma4x4[idx], dct4x4[idx] );

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
                            h->quantf.dequant_4x4( dct4x4[i8x8*4+i], h->dequant4_mf[CQM_4PY], i_qp );
                        h->dctf.add8x8_idct( &mb->pic.p_fdec[0][(i8x8&1)*8 + (i8x8>>1)*8*FDEC_STRIDE], &dct4x4[i8x8*4] );
                    }
            }

        }


    }
#endif
    /* encode chroma */
    if( IS_INTRA( mb->i_type ) )
    {
        const int i_mode = mb->i_chroma_pred_mode;
        h->predict_8x8c[i_mode]( mb->pic.p_fdec[1] );
        h->predict_8x8c[i_mode]( mb->pic.p_fdec[2] );
    }

    /* encode the 8x8 blocks */
    x264_mb_encode_8x8_chroma_tpc( h, !IS_INTRA( mb->i_type ), mb->i_chroma_qp, mb, dct );
#if 1
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

#endif
    /* store cbp */
    mb->cbp[mb->i_mb_xy] = (i_cbp_dc << 8) | (mb->i_cbp_chroma << 4) | mb->i_cbp_luma;

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

