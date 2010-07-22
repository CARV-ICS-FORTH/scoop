/*****************************************************************************
 * cavlc.c: h264 encoder library
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

#include "common/common.h"
#include "macroblock.h"
#include "common/macroblock_tpc.h"

static const uint8_t intra4x4_cbp_to_golomb[48]=
{
  3, 29, 30, 17, 31, 18, 37,  8, 32, 38, 19,  9, 20, 10, 11,  2,
 16, 33, 34, 21, 35, 22, 39,  4, 36, 40, 23,  5, 24,  6,  7,  1,
 41, 42, 43, 25, 44, 26, 46, 12, 45, 47, 27, 13, 28, 14, 15,  0
};
static const uint8_t inter_cbp_to_golomb[48]=
{
  0,  2,  3,  7,  4,  8, 17, 13,  5, 18,  9, 14, 10, 15, 16, 11,
  1, 32, 33, 36, 34, 37, 44, 40, 35, 45, 38, 41, 39, 42, 43, 19,
  6, 24, 25, 20, 26, 21, 46, 28, 27, 47, 22, 29, 23, 30, 31, 12
};
static const uint8_t mb_type_b_to_golomb[3][9]=
{
    { 4,  8, 12, 10,  6, 14, 16, 18, 20 }, /* D_16x8 */
    { 5,  9, 13, 11,  7, 15, 17, 19, 21 }, /* D_8x16 */
    { 1, -1, -1, -1,  2, -1, -1, -1,  3 }  /* D_16x16 */
};
static const uint8_t sub_mb_type_p_to_golomb[4]=
{
    3, 1, 2, 0
};
static const uint8_t sub_mb_type_b_to_golomb[13]=
{
    10,  4,  5,  1, 11,  6,  7,  2, 12,  8,  9,  3,  0
};

#define BLOCK_INDEX_CHROMA_DC   (-1)
#define BLOCK_INDEX_LUMA_DC     (-2)

static inline void bs_write_vlc( bs_t *s, vlc_t v )
{
//   fprintf(stderr," %d %d SPU!!\n", v.i_size, v.i_bits);
    bs_write( s, v.i_size, v.i_bits );
}

/****************************************************************************
 * block_residual_write_cavlc:
 ****************************************************************************/
static void block_residual_write_cavlc( x264_t *h, bs_t *s, int i_idx, int16_t *l, int i_count )
{
    int level[16], run[16];
    int i_total, i_trailing;
    int i_total_zero;
    int i_last;
    unsigned int i_sign;
    int i;
    int i_suffix_length;

    /* first find i_last */
    for( i_last = i_count-1; i_last >= 3; i_last -= 4 )
        if( *(uint64_t*)(l+i_last-3) )
            break;
    while( i_last >= 0 && l[i_last] == 0 )
        i_last--;

    i_sign = 0;
    i_total = 0;
    i_trailing = 0;
    i_total_zero = i_last + 1;

    if( i_last >= 0 )
    {
        int idx = 0;

        /* level and run and total */
        while( i_last >= 0 )
        {
            int r = 0;
            level[idx] = l[i_last];
            while( --i_last >= 0 && l[i_last] == 0 )
                r++;
            run[idx++] = r;
        }

        i_total = idx;
        i_total_zero -= idx;

        i_trailing = X264_MIN(3, idx);
        for( idx = 0; idx < i_trailing; idx++ )
        {
            if( (unsigned)(level[idx]+1) > 2 )
            {
                i_trailing = idx;
                break;
            }
            i_sign <<= 1;
            i_sign |= level[idx] < 0;
        }
    }

    /* total/trailing */
    if( i_idx == BLOCK_INDEX_CHROMA_DC )
        bs_write_vlc( s, x264_coeff_token[4][i_total*4+i_trailing] );
    else
    {
        /* x264_mb_predict_non_zero_code return 0 <-> (16+16+1)>>1 = 16 */
        static const int ct_index[17] = {0,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,3 };
        int nC = x264_mb_predict_non_zero_code( h, i_idx == BLOCK_INDEX_LUMA_DC ? 0 : i_idx );
        bs_write_vlc( s, x264_coeff_token[ct_index[nC]][i_total*4+i_trailing] );
    }

    if( i_total <= 0 )
        return;

    i_suffix_length = i_total > 10 && i_trailing < 3 ? 1 : 0;
    if( i_trailing > 0 )
        bs_write( s, i_trailing, i_sign );
    for( i = i_trailing; i < i_total; i++ )
    {
        int mask = level[i] >> 15;
        int abs_level = (level[i]^mask)-mask;
        int i_level_code = abs_level*2-mask-2;

        if( i == i_trailing && i_trailing < 3 )
            i_level_code -= 2; /* as level[i] can't be 1 for the first one if i_trailing < 3 */

        if( ( i_level_code >> i_suffix_length ) < 14 )
            bs_write( s, (i_level_code >> i_suffix_length) + 1 + i_suffix_length,
                     (1<<i_suffix_length) + (i_level_code & ((1<<i_suffix_length)-1)) );
        else if( i_suffix_length == 0 && i_level_code < 30 )
            bs_write( s, 19, (1<<4) + (i_level_code - 14) );
        else if( i_suffix_length > 0 && ( i_level_code >> i_suffix_length ) == 14 )
            bs_write( s, 15 + i_suffix_length,
                      (1<<i_suffix_length) + (i_level_code & ((1<<i_suffix_length)-1)) );
        else
        {
            int i_level_prefix = 15;
            i_level_code -= 15 << i_suffix_length;
            if( i_suffix_length == 0 )
                i_level_code -= 15;

            /* If the prefix size exceeds 15, High Profile is required. */
            if( i_level_code >= 1<<12 )
            {
                if( h->sps->i_profile_idc >= PROFILE_HIGH )
                {
                    while( i_level_code > 1<<(i_level_prefix-3) )
                    {
                        i_level_code -= 1<<(i_level_prefix-3);
                        i_level_prefix++;
                    }
                }
                else
                {
#ifdef RDO_SKIP_BS
                    /* Weight highly against overflows. */
                    s->i_bits_encoded += 1000000;
#else
                    x264_log(h, X264_LOG_WARNING, "OVERFLOW levelcode=%d is only allowed in High Profile", i_level_code );
                    /* clip level, preserving sign */
                    i_level_code = (1<<12) - 2 + (i_level_code & 1);
#endif
                }
            }
            bs_write( s, i_level_prefix + 1, 1 );
            bs_write( s, i_level_prefix - 3, i_level_code & ((1<<(i_level_prefix-3))-1) );
        }

        if( i_suffix_length == 0 )
            i_suffix_length++;
        if( abs_level > (3 << (i_suffix_length-1)) && i_suffix_length < 6 )
            i_suffix_length++;
    }

    if( i_total < i_count )
    {
        if( i_idx == BLOCK_INDEX_CHROMA_DC )
            bs_write_vlc( s, x264_total_zeros_dc[i_total-1][i_total_zero] );
        else
            bs_write_vlc( s, x264_total_zeros[i_total-1][i_total_zero] );
    }

    for( i = 0; i < i_total-1 && i_total_zero > 0; i++ )
    {
        int i_zl = X264_MIN( i_total_zero - 1, 6 );
        bs_write_vlc( s, x264_run_before[i_zl][run[i]] );
        i_total_zero -= run[i];
    }
}

static void cavlc_qp_delta( x264_t *h, bs_t *s )
{
    int i_dqp = h->mb.i_qp - h->mb.i_last_qp;

    /* Avoid writing a delta quant if we have an empty i16x16 block, e.g. in a completely flat background area */
    if( h->mb.i_type == I_16x16 && !(h->mb.i_cbp_luma | h->mb.i_cbp_chroma)
        && !array_non_zero(h->dct.luma16x16_dc) )
    {
#ifndef RDO_SKIP_BS
        h->mb.i_qp = h->mb.i_last_qp;
#endif
        i_dqp = 0;
    }

    if( i_dqp )
    {
        if( i_dqp < -26 )
            i_dqp += 52;
        else if( i_dqp > 25 )
            i_dqp -= 52;
    }
    bs_write_se( s, i_dqp );
}

static void cavlc_mb_mvd( x264_t *h, bs_t *s, int i_list, int idx, int width )
{
    DECLARE_ALIGNED_4( int16_t mvp[2] );
    x264_mb_predict_mv( h, i_list, idx, width, mvp );
    bs_write_se( s, h->mb.cache.mv[i_list][x264_scan8[idx]][0] - mvp[0] );
    bs_write_se( s, h->mb.cache.mv[i_list][x264_scan8[idx]][1] - mvp[1] );
}



static void cavlc_mb8x8_mvd( x264_t *h, bs_t *s, int i_list, int i )
{
    if( !x264_mb_partition_listX_table[i_list][ h->mb.i_sub_partition[i] ] )
        return;

    switch( h->mb.i_sub_partition[i] )
    {
        case D_L0_8x8:
        case D_L1_8x8:
        case D_BI_8x8:
            cavlc_mb_mvd( h, s, i_list, 4*i, 2 );
            break;
        case D_L0_8x4:
        case D_L1_8x4:
        case D_BI_8x4:
            cavlc_mb_mvd( h, s, i_list, 4*i+0, 2 );
            cavlc_mb_mvd( h, s, i_list, 4*i+2, 2 );
            break;
        case D_L0_4x8:
        case D_L1_4x8:
        case D_BI_4x8:
            cavlc_mb_mvd( h, s, i_list, 4*i+0, 1 );
            cavlc_mb_mvd( h, s, i_list, 4*i+1, 1 );
            break;
        case D_L0_4x4:
        case D_L1_4x4:
        case D_BI_4x4:
            cavlc_mb_mvd( h, s, i_list, 4*i+0, 1 );
            cavlc_mb_mvd( h, s, i_list, 4*i+1, 1 );
            cavlc_mb_mvd( h, s, i_list, 4*i+2, 1 );
            cavlc_mb_mvd( h, s, i_list, 4*i+3, 1 );
            break;
    }
}

static inline void x264_macroblock_luma_write_cavlc( x264_t *h, bs_t *s, int i8start, int i8end )
{
    int i8, i4, i;
    if( h->mb.b_transform_8x8 )
    {
        /* shuffle 8x8 dct coeffs into 4x4 lists */
        for( i8 = i8start; i8 <= i8end; i8++ )
            if( h->mb.i_cbp_luma & (1 << i8) )
                for( i4 = 0; i4 < 4; i4++ )
                    for( i = 0; i < 16; i++ )
                        h->dct.luma4x4[i4+i8*4][i] = h->dct.luma8x8[i8][i4+i*4];
    }

    for( i8 = i8start; i8 <= i8end; i8++ )
        if( h->mb.i_cbp_luma & (1 << i8) )
            for( i4 = 0; i4 < 4; i4++ )
            {
                h->mb.cache.non_zero_count[x264_scan8[i4+i8*4]] = array_non_zero_count( h->dct.luma4x4[i4+i8*4] );
                block_residual_write_cavlc( h, s, i4+i8*4, h->dct.luma4x4[i4+i8*4], 16 );
            }
}

/*****************************************************************************
 * x264_macroblock_write:
 *****************************************************************************/
void x264_macroblock_write_cavlc( x264_t *h, bs_t *s )
{
    const int i_mb_type = h->mb.i_type;
    int i_mb_i_offset;
    int i;

#ifndef RDO_SKIP_BS
    const int i_mb_pos_start = bs_pos( s );
    int       i_mb_pos_tex;
#endif

    switch( h->sh.i_type )
    {
        case SLICE_TYPE_I:
            i_mb_i_offset = 0;
            break;
        case SLICE_TYPE_P:
            i_mb_i_offset = 5;
            break;
        case SLICE_TYPE_B:
            i_mb_i_offset = 23;
            break;
        default:
            x264_log(h, X264_LOG_ERROR, "internal error or slice unsupported\n" );
            return;
    }

    if( h->sh.b_mbaff
        && (!(h->mb.i_mb_y & 1) || IS_SKIP(h->mb.type[h->mb.i_mb_xy - h->mb.i_mb_stride])) )
    {
        bs_write1( s, h->mb.b_interlaced );
    }

#ifndef RDO_SKIP_BS
    if( i_mb_type == I_PCM)
    {
        bs_write_ue( s, i_mb_i_offset + 25 );
        i_mb_pos_tex = bs_pos( s );
        h->stat.frame.i_mv_bits += i_mb_pos_tex - i_mb_pos_start;

        bs_align_0( s );

        memcpy( s->p, h->mb.pic.p_fenc[0], 256 );
        s->p += 256;
        for( i = 0; i < 8; i++ )
            memcpy( s->p + i*8, h->mb.pic.p_fenc[1] + i*FENC_STRIDE, 8 );
        s->p += 64;
        for( i = 0; i < 8; i++ )
            memcpy( s->p + i*8, h->mb.pic.p_fenc[2] + i*FENC_STRIDE, 8 );
        s->p += 64;

        /* if PCM is chosen, we need to store reconstructed frame data */
        h->mc.copy[PIXEL_16x16]( h->mb.pic.p_fdec[0], FDEC_STRIDE, h->mb.pic.p_fenc[0], FENC_STRIDE, 16 );
        h->mc.copy[PIXEL_8x8]  ( h->mb.pic.p_fdec[1], FDEC_STRIDE, h->mb.pic.p_fenc[1], FENC_STRIDE, 8 );
        h->mc.copy[PIXEL_8x8]  ( h->mb.pic.p_fdec[2], FDEC_STRIDE, h->mb.pic.p_fenc[2], FENC_STRIDE, 8 );

        h->stat.frame.i_tex_bits += bs_pos(s) - i_mb_pos_tex;
        return;
    }
#endif

    /* Write:
      - type
      - prediction
      - mv */
    if( i_mb_type == I_4x4 || i_mb_type == I_8x8 )
    {
        int di = i_mb_type == I_8x8 ? 4 : 1;
        bs_write_ue( s, i_mb_i_offset + 0 );
        if( h->pps->b_transform_8x8_mode )
            bs_write1( s, h->mb.b_transform_8x8 );

        /* Prediction: Luma */
        for( i = 0; i < 16; i += di )
        {
            int i_pred = x264_mb_predict_intra4x4_mode( h, i );
            int i_mode = x264_mb_pred_mode4x4_fix( h->mb.cache.intra4x4_pred_mode[x264_scan8[i]] );

            if( i_pred == i_mode )
                bs_write1( s, 1 );  /* b_prev_intra4x4_pred_mode */
            else
                bs_write( s, 4, i_mode - (i_mode > i_pred) );
        }
        bs_write_ue( s, x264_mb_pred_mode8x8c_fix[ h->mb.i_chroma_pred_mode ] );
    }
    else if( i_mb_type == I_16x16 )
    {
        bs_write_ue( s, i_mb_i_offset + 1 + x264_mb_pred_mode16x16_fix[h->mb.i_intra16x16_pred_mode] +
                        h->mb.i_cbp_chroma * 4 + ( h->mb.i_cbp_luma == 0 ? 0 : 12 ) );
        bs_write_ue( s, x264_mb_pred_mode8x8c_fix[ h->mb.i_chroma_pred_mode ] );
    }
    else if( i_mb_type == P_L0 )
    {
        DECLARE_ALIGNED_4( int16_t mvp[2] );

        if( h->mb.i_partition == D_16x16 )
        {
            bs_write_ue( s, 0 );

            if( h->mb.pic.i_fref[0] > 1 )
                bs_write_te( s, h->mb.pic.i_fref[0] - 1, h->mb.cache.ref[0][x264_scan8[0]] );
            x264_mb_predict_mv( h, 0, 0, 4, mvp );
            bs_write_se( s, h->mb.cache.mv[0][x264_scan8[0]][0] - mvp[0] );
            bs_write_se( s, h->mb.cache.mv[0][x264_scan8[0]][1] - mvp[1] );
        }
        else if( h->mb.i_partition == D_16x8 )
        {
            bs_write_ue( s, 1 );
            if( h->mb.pic.i_fref[0] > 1 )
            {
                bs_write_te( s, h->mb.pic.i_fref[0] - 1, h->mb.cache.ref[0][x264_scan8[0]] );
                bs_write_te( s, h->mb.pic.i_fref[0] - 1, h->mb.cache.ref[0][x264_scan8[8]] );
            }

            x264_mb_predict_mv( h, 0, 0, 4, mvp );
            bs_write_se( s, h->mb.cache.mv[0][x264_scan8[0]][0] - mvp[0] );
            bs_write_se( s, h->mb.cache.mv[0][x264_scan8[0]][1] - mvp[1] );

            x264_mb_predict_mv( h, 0, 8, 4, mvp );
            bs_write_se( s, h->mb.cache.mv[0][x264_scan8[8]][0] - mvp[0] );
            bs_write_se( s, h->mb.cache.mv[0][x264_scan8[8]][1] - mvp[1] );
        }
        else if( h->mb.i_partition == D_8x16 )
        {
            bs_write_ue( s, 2 );
            if( h->mb.pic.i_fref[0] > 1 )
            {
                bs_write_te( s, h->mb.pic.i_fref[0] - 1, h->mb.cache.ref[0][x264_scan8[0]] );
                bs_write_te( s, h->mb.pic.i_fref[0] - 1, h->mb.cache.ref[0][x264_scan8[4]] );
            }

            x264_mb_predict_mv( h, 0, 0, 2, mvp );
            bs_write_se( s, h->mb.cache.mv[0][x264_scan8[0]][0] - mvp[0] );
            bs_write_se( s, h->mb.cache.mv[0][x264_scan8[0]][1] - mvp[1] );

            x264_mb_predict_mv( h, 0, 4, 2, mvp );
            bs_write_se( s, h->mb.cache.mv[0][x264_scan8[4]][0] - mvp[0] );
            bs_write_se( s, h->mb.cache.mv[0][x264_scan8[4]][1] - mvp[1] );
        }
    }
    else if( i_mb_type == P_8x8 )
    {
        int b_sub_ref0;
        if( (h->mb.cache.ref[0][x264_scan8[0]] | h->mb.cache.ref[0][x264_scan8[ 4]] |
             h->mb.cache.ref[0][x264_scan8[8]] | h->mb.cache.ref[0][x264_scan8[12]]) == 0 )
        {
            bs_write_ue( s, 4 );
            b_sub_ref0 = 0;
        }
        else
        {
            bs_write_ue( s, 3 );
            b_sub_ref0 = 1;
        }

        /* sub mb type */
        if( h->param.analyse.inter & X264_ANALYSE_PSUB8x8 )
            for( i = 0; i < 4; i++ )
                bs_write_ue( s, sub_mb_type_p_to_golomb[ h->mb.i_sub_partition[i] ] );
        else
            bs_write( s, 4, 0xf );

        /* ref0 */
        if( h->mb.pic.i_fref[0] > 1 && b_sub_ref0 )
        {
            bs_write_te( s, h->mb.pic.i_fref[0] - 1, h->mb.cache.ref[0][x264_scan8[0]] );
            bs_write_te( s, h->mb.pic.i_fref[0] - 1, h->mb.cache.ref[0][x264_scan8[4]] );
            bs_write_te( s, h->mb.pic.i_fref[0] - 1, h->mb.cache.ref[0][x264_scan8[8]] );
            bs_write_te( s, h->mb.pic.i_fref[0] - 1, h->mb.cache.ref[0][x264_scan8[12]] );
        }

        for( i = 0; i < 4; i++ )
            cavlc_mb8x8_mvd( h, s, 0, i );
    }
    else if( i_mb_type == B_8x8 )
    {
        bs_write_ue( s, 22 );

        /* sub mb type */
        for( i = 0; i < 4; i++ )
            bs_write_ue( s, sub_mb_type_b_to_golomb[ h->mb.i_sub_partition[i] ] );

        /* ref */
        for( i = 0; i < 4; i++ )
            if( x264_mb_partition_listX_table[0][ h->mb.i_sub_partition[i] ] )
                bs_write_te( s, h->mb.pic.i_fref[0] - 1, h->mb.cache.ref[0][x264_scan8[i*4]] );
        for( i = 0; i < 4; i++ )
            if( x264_mb_partition_listX_table[1][ h->mb.i_sub_partition[i] ] )
                bs_write_te( s, h->mb.pic.i_fref[1] - 1, h->mb.cache.ref[1][x264_scan8[i*4]] );

        /* mvd */
        for( i = 0; i < 4; i++ )
            cavlc_mb8x8_mvd( h, s, 0, i );
        for( i = 0; i < 4; i++ )
            cavlc_mb8x8_mvd( h, s, 1, i );
    }
    else if( i_mb_type != B_DIRECT )
    {

        /* All B mode */
        /* Motion Vector */
        int i_list;
        DECLARE_ALIGNED_4( int16_t mvp[2] );

        int b_list[2][2];

        /* init ref list utilisations */
        for( i = 0; i < 2; i++ )
        {
            b_list[0][i] = x264_mb_type_list0_table[i_mb_type][i];
            b_list[1][i] = x264_mb_type_list1_table[i_mb_type][i];
        }

        bs_write_ue( s, mb_type_b_to_golomb[ h->mb.i_partition - D_16x8 ][ i_mb_type - B_L0_L0 ] );

        for( i_list = 0; i_list < 2; i_list++ )
        {
            const int i_ref_max = (i_list == 0 ? h->mb.pic.i_fref[0] : h->mb.pic.i_fref[1]) - 1;

            if( i_ref_max )
                switch( h->mb.i_partition )
                {
                    case D_16x16:
                        if( b_list[i_list][0] ) bs_write_te( s, i_ref_max, h->mb.cache.ref[i_list][x264_scan8[0]] );
                        break;
                    case D_16x8:
                        if( b_list[i_list][0] ) bs_write_te( s, i_ref_max, h->mb.cache.ref[i_list][x264_scan8[0]] );
                        if( b_list[i_list][1] ) bs_write_te( s, i_ref_max, h->mb.cache.ref[i_list][x264_scan8[8]] );
                        break;
                    case D_8x16:
                        if( b_list[i_list][0] ) bs_write_te( s, i_ref_max, h->mb.cache.ref[i_list][x264_scan8[0]] );
                        if( b_list[i_list][1] ) bs_write_te( s, i_ref_max, h->mb.cache.ref[i_list][x264_scan8[4]] );
                        break;
                }
        }
        for( i_list = 0; i_list < 2; i_list++ )
        {
            switch( h->mb.i_partition )
            {
                case D_16x16:
                    if( b_list[i_list][0] )
                    {
                        x264_mb_predict_mv( h, i_list, 0, 4, mvp );
                        bs_write_se( s, h->mb.cache.mv[i_list][x264_scan8[0]][0] - mvp[0] );
                        bs_write_se( s, h->mb.cache.mv[i_list][x264_scan8[0]][1] - mvp[1] );
                    }
                    break;
                case D_16x8:
                    if( b_list[i_list][0] )
                    {
                        x264_mb_predict_mv( h, i_list, 0, 4, mvp );
                        bs_write_se( s, h->mb.cache.mv[i_list][x264_scan8[0]][0] - mvp[0] );
                        bs_write_se( s, h->mb.cache.mv[i_list][x264_scan8[0]][1] - mvp[1] );
                    }
                    if( b_list[i_list][1] )
                    {
                        x264_mb_predict_mv( h, i_list, 8, 4, mvp);
                        bs_write_se( s, h->mb.cache.mv[i_list][x264_scan8[8]][0] - mvp[0] );
                        bs_write_se( s, h->mb.cache.mv[i_list][x264_scan8[8]][1] - mvp[1] );
                    }
                    break;
                case D_8x16:
                    if( b_list[i_list][0] )
                    {
                        x264_mb_predict_mv( h, i_list, 0, 2, mvp);
                        bs_write_se( s, h->mb.cache.mv[i_list][x264_scan8[0]][0] - mvp[0] );
                        bs_write_se( s, h->mb.cache.mv[i_list][x264_scan8[0]][1] - mvp[1] );
                    }
                    if( b_list[i_list][1] )
                    {
                        x264_mb_predict_mv( h, i_list, 4, 2, mvp );
                        bs_write_se( s, h->mb.cache.mv[i_list][x264_scan8[4]][0] - mvp[0] );
                        bs_write_se( s, h->mb.cache.mv[i_list][x264_scan8[4]][1] - mvp[1] );
                    }
                    break;
            }
        }

    }
    else if( i_mb_type == B_DIRECT )
        bs_write_ue( s, 0 );
    else
    {
        x264_log(h, X264_LOG_ERROR, "invalid/unhandled mb_type\n" );
        return;
    }

#ifndef RDO_SKIP_BS
    i_mb_pos_tex = bs_pos( s );
    h->stat.frame.i_mv_bits += i_mb_pos_tex - i_mb_pos_start;
#endif

    /* Coded block patern */
    if( i_mb_type == I_4x4 || i_mb_type == I_8x8 )
        bs_write_ue( s, intra4x4_cbp_to_golomb[( h->mb.i_cbp_chroma << 4 )|h->mb.i_cbp_luma] );
    else if( i_mb_type != I_16x16 )
        bs_write_ue( s, inter_cbp_to_golomb[( h->mb.i_cbp_chroma << 4 )|h->mb.i_cbp_luma] );

    /* transform size 8x8 flag */
    if( x264_mb_transform_8x8_allowed( h ) && h->mb.i_cbp_luma )
        bs_write1( s, h->mb.b_transform_8x8 );

    /* write residual */
    if( i_mb_type == I_16x16 )
    {
        cavlc_qp_delta( h, s );

        /* DC Luma */
        block_residual_write_cavlc( h, s, BLOCK_INDEX_LUMA_DC , h->dct.luma16x16_dc, 16 );

        /* AC Luma */
        if( h->mb.i_cbp_luma )
            for( i = 0; i < 16; i++ )
            {
                h->mb.cache.non_zero_count[x264_scan8[i]] = array_non_zero_count( h->dct.luma4x4[i] );
                block_residual_write_cavlc( h, s, i, h->dct.luma4x4[i]+1, 15 );
            }
    }
    else if( h->mb.i_cbp_luma | h->mb.i_cbp_chroma )
    {
        cavlc_qp_delta( h, s );
        x264_macroblock_luma_write_cavlc( h, s, 0, 3 );
    }

    if( h->mb.i_cbp_chroma )
    {
        /* Chroma DC residual present */
        block_residual_write_cavlc( h, s, BLOCK_INDEX_CHROMA_DC, h->dct.chroma_dc[0], 4 );
        block_residual_write_cavlc( h, s, BLOCK_INDEX_CHROMA_DC, h->dct.chroma_dc[1], 4 );
        if( h->mb.i_cbp_chroma&0x02 ) /* Chroma AC residual present */
            for( i = 16; i < 24; i++ )
            {
                h->mb.cache.non_zero_count[x264_scan8[i]] = array_non_zero_count( h->dct.luma4x4[i] );
                block_residual_write_cavlc( h, s, i, h->dct.luma4x4[i]+1, 15 );
            }
    }

#ifndef RDO_SKIP_BS
    h->stat.frame.i_tex_bits += bs_pos(s) - i_mb_pos_tex;
#endif
}


#ifdef TPC

/****************************************************************************
 * block_residual_write_cavlc_tpc:
 ****************************************************************************/
static void block_residual_write_cavlc_tpc( x264_t *h, bs_t *s, int i_idx, int16_t *l, int i_count, x264_mb_t *mb )
{
    int level[16], run[16];
    int i_total, i_trailing;
    int i_total_zero;
    int i_last;
    unsigned int i_sign;
    int i;
    int i_suffix_length;

    /* first find i_last */
    for( i_last = i_count-1; i_last >= 3; i_last -= 4 )
        if( *(uint64_t*)(l+i_last-3) )
            break;
    while( i_last >= 0 && l[i_last] == 0 )
        i_last--;

    i_sign = 0;
    i_total = 0;
    i_trailing = 0;
    i_total_zero = i_last + 1;

    if( i_last >= 0 )
    {
        int idx = 0;

        /* level and run and total */
        while( i_last >= 0 )
        {
            int r = 0;
            level[idx] = l[i_last];
            while( --i_last >= 0 && l[i_last] == 0 )
                r++;
            run[idx++] = r;
        }

        i_total = idx;
        i_total_zero -= idx;

        i_trailing = X264_MIN(3, idx);
        for( idx = 0; idx < i_trailing; idx++ )
        {
            if( (unsigned)(level[idx]+1) > 2 )
            {
                i_trailing = idx;
                break;
            }
            i_sign <<= 1;
            i_sign |= level[idx] < 0;
        }
    }

    /* total/trailing */
    if( i_idx == BLOCK_INDEX_CHROMA_DC )
        bs_write_vlc( s, x264_coeff_token[4][i_total*4+i_trailing] );
    else
    {
        /* x264_mb_predict_non_zero_code return 0 <-> (16+16+1)>>1 = 16 */
        static const int ct_index[17] = {0,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,3 };
        int nC = x264_mb_predict_non_zero_code_tpc(  i_idx == BLOCK_INDEX_LUMA_DC ? 0 : i_idx, mb );
        bs_write_vlc( s, x264_coeff_token[ct_index[nC]][i_total*4+i_trailing] );
    }

    if( i_total <= 0 )
        return;

    i_suffix_length = i_total > 10 && i_trailing < 3 ? 1 : 0;
    if( i_trailing > 0 )
        bs_write( s, i_trailing, i_sign );
    for( i = i_trailing; i < i_total; i++ )
    {
        int mask = level[i] >> 15;
        int abs_level = (level[i]^mask)-mask;
        int i_level_code = abs_level*2-mask-2;

        if( i == i_trailing && i_trailing < 3 )
            i_level_code -= 2; /* as level[i] can't be 1 for the first one if i_trailing < 3 */

        if( ( i_level_code >> i_suffix_length ) < 14 )
            bs_write( s, (i_level_code >> i_suffix_length) + 1 + i_suffix_length,
                     (1<<i_suffix_length) + (i_level_code & ((1<<i_suffix_length)-1)) );
        else if( i_suffix_length == 0 && i_level_code < 30 )
            bs_write( s, 19, (1<<4) + (i_level_code - 14) );
        else if( i_suffix_length > 0 && ( i_level_code >> i_suffix_length ) == 14 )
            bs_write( s, 15 + i_suffix_length,
                      (1<<i_suffix_length) + (i_level_code & ((1<<i_suffix_length)-1)) );
        else
        {
            int i_level_prefix = 15;
            i_level_code -= 15 << i_suffix_length;
            if( i_suffix_length == 0 )
                i_level_code -= 15;

            /* If the prefix size exceeds 15, High Profile is required. */
            if( i_level_code >= 1<<12 )
            {
                if( h->sps->i_profile_idc >= PROFILE_HIGH )
                {
                    while( i_level_code > 1<<(i_level_prefix-3) )
                    {
                        i_level_code -= 1<<(i_level_prefix-3);
                        i_level_prefix++;
                    }
                }
                else
                {
#ifdef RDO_SKIP_BS
                    /* Weight highly against overflows. */
                    s->i_bits_encoded += 1000000;
#else
                    x264_log(h, X264_LOG_WARNING, "OVERFLOW levelcode=%d is only allowed in High Profile", i_level_code );
                    /* clip level, preserving sign */
                    i_level_code = (1<<12) - 2 + (i_level_code & 1);
#endif
                }
            }
            bs_write( s, i_level_prefix + 1, 1 );
            bs_write( s, i_level_prefix - 3, i_level_code & ((1<<(i_level_prefix-3))-1) );
        }

        if( i_suffix_length == 0 )
            i_suffix_length++;
        if( abs_level > (3 << (i_suffix_length-1)) && i_suffix_length < 6 )
            i_suffix_length++;
    }

    if( i_total < i_count )
    {
        if( i_idx == BLOCK_INDEX_CHROMA_DC )
            bs_write_vlc( s, x264_total_zeros_dc[i_total-1][i_total_zero] );
        else
            bs_write_vlc( s, x264_total_zeros[i_total-1][i_total_zero] );
    }

    for( i = 0; i < i_total-1 && i_total_zero > 0; i++ )
    {
        int i_zl = X264_MIN( i_total_zero - 1, 6 );
        bs_write_vlc( s, x264_run_before[i_zl][run[i]] );
        i_total_zero -= run[i];
    }
}


static void cavlc_qp_delta_tpc( x264_t *h, bs_t *s, x264_mb_t *mb, x264_dct_t *dct )
{
    int i_dqp = mb->i_qp - mb->i_last_qp;

    /* Avoid writing a delta quant if we have an empty i16x16 block, e.g. in a completely flat background area */
    if( mb->i_type == I_16x16 && !(mb->i_cbp_luma | mb->i_cbp_chroma)
        && !array_non_zero(dct->luma16x16_dc) )
    {
#ifndef RDO_SKIP_BS
        mb->i_qp = mb->i_last_qp;
#endif
        i_dqp = 0;
    }

    if( i_dqp )
    {
        if( i_dqp < -26 )
            i_dqp += 52;
        else if( i_dqp > 25 )
            i_dqp -= 52;
    }
    bs_write_se( s, i_dqp );
}


static void cavlc_mb_mvd_tpc( x264_t *h, bs_t *s, int i_list, int idx, int width, x264_mb_t *mb )
{
    DECLARE_ALIGNED_4( int16_t mvp[2] );
    x264_mb_predict_mv_tpc( h, i_list, idx, width, mvp, mb );
    bs_write_se( s, mb->cache.mv[i_list][x264_scan8[idx]][0] - mvp[0] );
    bs_write_se( s, mb->cache.mv[i_list][x264_scan8[idx]][1] - mvp[1] );
}



static void cavlc_mb8x8_mvd_tpc( x264_t *h, bs_t *s, int i_list, int i, x264_mb_t *mb )
{
    if( !x264_mb_partition_listX_table[i_list][ mb->i_sub_partition[i] ] )
        return;

    switch( mb->i_sub_partition[i] )
    {
        case D_L0_8x8:
        case D_L1_8x8:
        case D_BI_8x8:
            cavlc_mb_mvd_tpc( h, s, i_list, 4*i, 2, mb);
            break;
        case D_L0_8x4:
        case D_L1_8x4:
        case D_BI_8x4:
            cavlc_mb_mvd_tpc( h, s, i_list, 4*i+0, 2, mb);
            cavlc_mb_mvd_tpc( h, s, i_list, 4*i+2, 2, mb);
            break;
        case D_L0_4x8:
        case D_L1_4x8:
        case D_BI_4x8:
            cavlc_mb_mvd_tpc( h, s, i_list, 4*i+0, 1, mb);
            cavlc_mb_mvd_tpc( h, s, i_list, 4*i+1, 1, mb);
            break;
        case D_L0_4x4:
        case D_L1_4x4:
        case D_BI_4x4:
            cavlc_mb_mvd_tpc( h, s, i_list, 4*i+0, 1, mb);
            cavlc_mb_mvd_tpc( h, s, i_list, 4*i+1, 1, mb);
            cavlc_mb_mvd_tpc( h, s, i_list, 4*i+2, 1, mb);
            cavlc_mb_mvd_tpc( h, s, i_list, 4*i+3, 1, mb);
            break;
    }
}

static inline void x264_macroblock_luma_write_cavlc_tpc( x264_t *h, bs_t *s, int i8start, int i8end, x264_mb_t *mb, x264_dct_t *dct )
{


    int i8, i4, i;
    if( mb->b_transform_8x8 )
    {
        /* shuffle 8x8 dct coeffs into 4x4 lists */
        for( i8 = i8start; i8 <= i8end; i8++ )
            if( mb->i_cbp_luma & (1 << i8) )
                for( i4 = 0; i4 < 4; i4++ )
                    for( i = 0; i < 16; i++ )
                        dct->luma4x4[i4+i8*4][i] = dct->luma8x8[i8][i4+i*4];
    }

    for( i8 = i8start; i8 <= i8end; i8++ )
        if( mb->i_cbp_luma & (1 << i8) )
            for( i4 = 0; i4 < 4; i4++ )
            {
                mb->cache.non_zero_count[x264_scan8[i4+i8*4]] = array_non_zero_count( dct->luma4x4[i4+i8*4] );
                block_residual_write_cavlc_tpc( h, s, i4+i8*4, dct->luma4x4[i4+i8*4], 16, mb );
            }


}

/*****************************************************************************
 * x264_macroblock_write_tpc:
 *****************************************************************************/
void x264_macroblock_write_cavlc_tpc( x264_t *h, bs_t *s, x264_mb_t *mb, x264_dct_t *dct )
{

    const int i_mb_type = mb->i_type;




    int i_mb_i_offset;
    int i;
#ifndef RDO_SKIP_BS
    const int i_mb_pos_start = bs_pos( s );
    int       i_mb_pos_tex;
#endif
#if 1
    switch( h->sh.i_type )
    {
        case SLICE_TYPE_I:
            i_mb_i_offset = 0;
            break;
        case SLICE_TYPE_P:
            i_mb_i_offset = 5;
            break;
        case SLICE_TYPE_B:
            i_mb_i_offset = 23;
            break;
        default:
            x264_log(h, X264_LOG_ERROR, "internal error or slice unsupported\n" );
            return;
    }

    if( h->sh.b_mbaff
        && (!(mb->i_mb_y & 1) || IS_SKIP(mb->type[mb->i_mb_xy - mb->i_mb_stride])) )
    {
        bs_write1( s, mb->b_interlaced );
    }

#ifndef RDO_SKIP_BS
    if( i_mb_type == I_PCM)
    {
        bs_write_ue( s, i_mb_i_offset + 25 );
        i_mb_pos_tex = bs_pos( s );
        h->stat.frame.i_mv_bits += i_mb_pos_tex - i_mb_pos_start;

        bs_align_0( s );

        memcpy( s->p, mb->pic.p_fenc[0], 256 );
        s->p += 256;
        for( i = 0; i < 8; i++ )
            memcpy( s->p + i*8, mb->pic.p_fenc[1] + i*FENC_STRIDE, 8 );
        s->p += 64;
        for( i = 0; i < 8; i++ )
            memcpy( s->p + i*8, mb->pic.p_fenc[2] + i*FENC_STRIDE, 8 );
        s->p += 64;

        /* if PCM is chosen, we need to store reconstructed frame data */
        h->mc.copy[PIXEL_16x16]( mb->pic.p_fdec[0], FDEC_STRIDE, mb->pic.p_fenc[0], FENC_STRIDE, 16 );
        h->mc.copy[PIXEL_8x8]  ( mb->pic.p_fdec[1], FDEC_STRIDE, mb->pic.p_fenc[1], FENC_STRIDE, 8 );
        h->mc.copy[PIXEL_8x8]  ( mb->pic.p_fdec[2], FDEC_STRIDE, mb->pic.p_fenc[2], FENC_STRIDE, 8 );

        h->stat.frame.i_tex_bits += bs_pos(s) - i_mb_pos_tex;
        return;
    }
#endif

    /* Write:
      - type
      - prediction
      - mv */
    if( i_mb_type == I_4x4 || i_mb_type == I_8x8 )
    {
        //assert( (i_mb_type != I_4x4) && (i_mb_type != I_8x8) );
        int di = i_mb_type == I_8x8 ? 4 : 1;
        bs_write_ue( s, i_mb_i_offset + 0 );
        if( h->pps->b_transform_8x8_mode )
            bs_write1( s, mb->b_transform_8x8 );

        /* Prediction: Luma */
        for( i = 0; i < 16; i += di )
        {
            int i_pred = x264_mb_predict_intra4x4_mode_tpc( h, i, mb );
            int i_mode = x264_mb_pred_mode4x4_fix( mb->cache.intra4x4_pred_mode[x264_scan8[i]] );

            if( i_pred == i_mode )
                bs_write1( s, 1 );  /* b_prev_intra4x4_pred_mode */
            else
                bs_write( s, 4, i_mode - (i_mode > i_pred) );
        }
        bs_write_ue( s, x264_mb_pred_mode8x8c_fix[ mb->i_chroma_pred_mode ] );
    }
    else if( i_mb_type == I_16x16 )
    {

        bs_write_ue( s, i_mb_i_offset + 1 + x264_mb_pred_mode16x16_fix[mb->i_intra16x16_pred_mode] +
                        mb->i_cbp_chroma * 4 + ( mb->i_cbp_luma == 0 ? 0 : 12 ) );
        bs_write_ue( s, x264_mb_pred_mode8x8c_fix[ mb->i_chroma_pred_mode ] );
    }
    else if( i_mb_type == P_L0 )
    {
        DECLARE_ALIGNED_4( int16_t mvp[2] );

	//puts("Write");

        if( mb->i_partition == D_16x16 )
        {
            bs_write_ue( s, 0 );

            if( mb->pic.i_fref[0] > 1 )
                bs_write_te( s, mb->pic.i_fref[0] - 1, mb->cache.ref[0][x264_scan8[0]] );


            x264_mb_predict_mv_tpc( h, 0, 0, 4, mvp, mb );

            bs_write_se( s, mb->cache.mv[0][x264_scan8[0]][0] - mvp[0] );
            bs_write_se( s, mb->cache.mv[0][x264_scan8[0]][1] - mvp[1] );


        }
        else if( mb->i_partition == D_16x8 )
        {
            assert(mb->i_partition != D_16x8 );
            bs_write_ue( s, 1 );
            if( mb->pic.i_fref[0] > 1 )
            {
                bs_write_te( s, mb->pic.i_fref[0] - 1, mb->cache.ref[0][x264_scan8[0]] );
                bs_write_te( s, mb->pic.i_fref[0] - 1, mb->cache.ref[0][x264_scan8[8]] );
            }

            x264_mb_predict_mv_tpc( h, 0, 0, 4, mvp, mb );
            bs_write_se( s, mb->cache.mv[0][x264_scan8[0]][0] - mvp[0] );
            bs_write_se( s, mb->cache.mv[0][x264_scan8[0]][1] - mvp[1] );

            x264_mb_predict_mv_tpc( h, 0, 8, 4, mvp, mb );
            bs_write_se( s, mb->cache.mv[0][x264_scan8[8]][0] - mvp[0] );
            bs_write_se( s, mb->cache.mv[0][x264_scan8[8]][1] - mvp[1] );
        }
        else if( mb->i_partition == D_8x16 )
        {
            assert( mb->i_partition != D_8x16);
            bs_write_ue( s, 2 );
            if( mb->pic.i_fref[0] > 1 )
            {
                bs_write_te( s, mb->pic.i_fref[0] - 1, mb->cache.ref[0][x264_scan8[0]] );
                bs_write_te( s, mb->pic.i_fref[0] - 1, mb->cache.ref[0][x264_scan8[4]] );
            }

            x264_mb_predict_mv_tpc( h, 0, 0, 2, mvp, mb );
            bs_write_se( s, mb->cache.mv[0][x264_scan8[0]][0] - mvp[0] );
            bs_write_se( s, mb->cache.mv[0][x264_scan8[0]][1] - mvp[1] );

            x264_mb_predict_mv_tpc( h, 0, 4, 2, mvp, mb );
            bs_write_se( s, mb->cache.mv[0][x264_scan8[4]][0] - mvp[0] );
            bs_write_se( s, mb->cache.mv[0][x264_scan8[4]][1] - mvp[1] );
        }
    }
    else if( i_mb_type == P_8x8 )
    {
        assert( i_mb_type != P_8x8 );
        int b_sub_ref0;
        if( (mb->cache.ref[0][x264_scan8[0]] | mb->cache.ref[0][x264_scan8[ 4]] |
             mb->cache.ref[0][x264_scan8[8]] | mb->cache.ref[0][x264_scan8[12]]) == 0 )
        {
            bs_write_ue( s, 4 );
            b_sub_ref0 = 0;
        }
        else
        {
            bs_write_ue( s, 3 );
            b_sub_ref0 = 1;
        }

        /* sub mb type */
        if( h->param.analyse.inter & X264_ANALYSE_PSUB8x8 )
            for( i = 0; i < 4; i++ )
                bs_write_ue( s, sub_mb_type_p_to_golomb[ mb->i_sub_partition[i] ] );
        else
            bs_write( s, 4, 0xf );

        /* ref0 */
        if( mb->pic.i_fref[0] > 1 && b_sub_ref0 )
        {
            bs_write_te( s, mb->pic.i_fref[0] - 1, mb->cache.ref[0][x264_scan8[0]] );
            bs_write_te( s, mb->pic.i_fref[0] - 1, mb->cache.ref[0][x264_scan8[4]] );
            bs_write_te( s, mb->pic.i_fref[0] - 1, mb->cache.ref[0][x264_scan8[8]] );
            bs_write_te( s, mb->pic.i_fref[0] - 1, mb->cache.ref[0][x264_scan8[12]] );
        }

        for( i = 0; i < 4; i++ )
            cavlc_mb8x8_mvd_tpc( h, s, 0, i, mb );
    }
    else if( i_mb_type == B_8x8 )
    {
        assert( i_mb_type != B_8x8);
        bs_write_ue( s, 22 );

        /* sub mb type */
        for( i = 0; i < 4; i++ )
            bs_write_ue( s, sub_mb_type_b_to_golomb[ mb->i_sub_partition[i] ] );

        /* ref */
        for( i = 0; i < 4; i++ )
            if( x264_mb_partition_listX_table[0][ mb->i_sub_partition[i] ] )
                bs_write_te( s, mb->pic.i_fref[0] - 1, mb->cache.ref[0][x264_scan8[i*4]] );
        for( i = 0; i < 4; i++ )
            if( x264_mb_partition_listX_table[1][ mb->i_sub_partition[i] ] )
                bs_write_te( s, mb->pic.i_fref[1] - 1, mb->cache.ref[1][x264_scan8[i*4]] );

        /* mvd */
        for( i = 0; i < 4; i++ )
            cavlc_mb8x8_mvd_tpc( h, s, 0, i, mb );
        for( i = 0; i < 4; i++ )
            cavlc_mb8x8_mvd_tpc( h, s, 1, i, mb );
    }
    else if( i_mb_type != B_DIRECT )
    {
        /* All B mode */
        /* Motion Vector */
        int i_list;
        DECLARE_ALIGNED_4( int16_t mvp[2] );

        int b_list[2][2];

        /* init ref list utilisations */
        for( i = 0; i < 2; i++ )
        {
            b_list[0][i] = x264_mb_type_list0_table[i_mb_type][i];
            b_list[1][i] = x264_mb_type_list1_table[i_mb_type][i];
        }

        bs_write_ue( s, mb_type_b_to_golomb[ mb->i_partition - D_16x8 ][ i_mb_type - B_L0_L0 ] );


        for( i_list = 0; i_list < 2; i_list++ )
        {
            const int i_ref_max = (i_list == 0 ? mb->pic.i_fref[0] : mb->pic.i_fref[1]) - 1;

            if( i_ref_max )
                switch( mb->i_partition )
                {
                    case D_16x16:
                        if( b_list[i_list][0] ) bs_write_te( s, i_ref_max, mb->cache.ref[i_list][x264_scan8[0]] );
                        break;
                    case D_16x8:
                        assert(mb->i_partition != D_16x8);
                        if( b_list[i_list][0] ) bs_write_te( s, i_ref_max, mb->cache.ref[i_list][x264_scan8[0]] );
                        if( b_list[i_list][1] ) bs_write_te( s, i_ref_max, mb->cache.ref[i_list][x264_scan8[8]] );
                        break;
                    case D_8x16:
                        assert(mb->i_partition != D_8x16);
                        if( b_list[i_list][0] ) bs_write_te( s, i_ref_max, mb->cache.ref[i_list][x264_scan8[0]] );
                        if( b_list[i_list][1] ) bs_write_te( s, i_ref_max, mb->cache.ref[i_list][x264_scan8[4]] );
                        break;
                }
        }

        for( i_list = 0; i_list < 2; i_list++ )
        {
            switch( mb->i_partition )
            {
                case D_16x16:
                    if( b_list[i_list][0] )
                    {
                         x264_mb_predict_mv_tpc( h, i_list, 0, 4, mvp, mb );

                         bs_write_se( s, mb->cache.mv[i_list][x264_scan8[0]][0] - mvp[0] );
                         bs_write_se( s, mb->cache.mv[i_list][x264_scan8[0]][1] - mvp[1] );
                    }
                    break;
                case D_16x8:
                    assert( mb->i_partition != D_16x8);
                    if( b_list[i_list][0] )
                    {
                        x264_mb_predict_mv_tpc( h, i_list, 0, 4, mvp, mb );
                        bs_write_se( s, mb->cache.mv[i_list][x264_scan8[0]][0] - mvp[0] );
                        bs_write_se( s, mb->cache.mv[i_list][x264_scan8[0]][1] - mvp[1] );
                    }
                    if( b_list[i_list][1] )
                    {
                        x264_mb_predict_mv_tpc( h, i_list, 8, 4, mvp, mb );
                        bs_write_se( s, mb->cache.mv[i_list][x264_scan8[8]][0] - mvp[0] );
                        bs_write_se( s, mb->cache.mv[i_list][x264_scan8[8]][1] - mvp[1] );
                    }
                    break;
                case D_8x16:
                    assert( mb->i_partition != D_8x16);
                    if( b_list[i_list][0] )
                    {
                        x264_mb_predict_mv_tpc( h, i_list, 0, 2, mvp, mb );
                        bs_write_se( s, mb->cache.mv[i_list][x264_scan8[0]][0] - mvp[0] );
                        bs_write_se( s, mb->cache.mv[i_list][x264_scan8[0]][1] - mvp[1] );
                    }
                    if( b_list[i_list][1] )
                    {
                        x264_mb_predict_mv_tpc( h, i_list, 4, 2, mvp, mb );
                        bs_write_se( s, mb->cache.mv[i_list][x264_scan8[4]][0] - mvp[0] );
                        bs_write_se( s, mb->cache.mv[i_list][x264_scan8[4]][1] - mvp[1] );
                    }
                    break;
            }
        }

    }
    else if( i_mb_type == B_DIRECT )
        bs_write_ue( s, 0 );
    else
    {
        x264_log(h, X264_LOG_ERROR, "invalid/unhandled mb_type\n" );
        return;
    }

#endif




#ifndef RDO_SKIP_BS
    i_mb_pos_tex = bs_pos( s );
    h->stat.frame.i_mv_bits += i_mb_pos_tex - i_mb_pos_start;
#endif

    /* Coded block patern */
    if( i_mb_type == I_4x4 || i_mb_type == I_8x8 )
        bs_write_ue( s, intra4x4_cbp_to_golomb[( mb->i_cbp_chroma << 4 )|mb->i_cbp_luma] );
    else if( i_mb_type != I_16x16 )
        bs_write_ue( s, inter_cbp_to_golomb[( mb->i_cbp_chroma << 4 )|mb->i_cbp_luma] );




    /* transform size 8x8 flag */
    if( x264_mb_transform_8x8_allowed_tpc( h, mb ) && mb->i_cbp_luma )
        bs_write1( s, mb->b_transform_8x8 );

    /* write residual */
    if( i_mb_type == I_16x16 )
    {
        cavlc_qp_delta_tpc( h, s, mb, dct );

        /* DC Luma */
        block_residual_write_cavlc_tpc( h, s, BLOCK_INDEX_LUMA_DC , dct->luma16x16_dc, 16, mb );

        /* AC Luma */
        if( mb->i_cbp_luma )
            for( i = 0; i < 16; i++ )
            {
                mb->cache.non_zero_count[x264_scan8[i]] = array_non_zero_count( dct->luma4x4[i] );
                block_residual_write_cavlc_tpc( h, s, i, dct->luma4x4[i]+1, 15, mb );
            }
    }
    else if( mb->i_cbp_luma | mb->i_cbp_chroma )
    {
        cavlc_qp_delta_tpc( h, s, mb, dct );
        x264_macroblock_luma_write_cavlc_tpc( h, s, 0, 3, mb, dct );

    }

    if( mb->i_cbp_chroma )
    {

        /* Chroma DC residual present */
        block_residual_write_cavlc_tpc( h, s, BLOCK_INDEX_CHROMA_DC, dct->chroma_dc[0], 4, mb );
        block_residual_write_cavlc_tpc( h, s, BLOCK_INDEX_CHROMA_DC, dct->chroma_dc[1], 4, mb);

        if( mb->i_cbp_chroma&0x02 ) /* Chroma AC residual present */
            for( i = 16; i < 24; i++ )
            {
                mb->cache.non_zero_count[x264_scan8[i]] = array_non_zero_count( dct->luma4x4[i] );
                block_residual_write_cavlc_tpc( h, s, i, dct->luma4x4[i]+1, 15, mb );
            }

    }



#ifndef RDO_SKIP_BS
    h->stat.frame.i_tex_bits += bs_pos(s) - i_mb_pos_tex;
#endif





}


static int x264_i8x8_chroma_size_cavlc_tpc( x264_t *h, x264_mb_t *mb, x264_dct_t *dct )
{
    h->out.bs.i_bits_encoded = bs_size_ue( x264_mb_pred_mode8x8c_fix[ mb->i_chroma_pred_mode ] );
    if( mb->i_cbp_chroma )
    {
        block_residual_write_cavlc_tpc( h, &h->out.bs, BLOCK_INDEX_CHROMA_DC, dct->chroma_dc[0], 4, mb );
        block_residual_write_cavlc_tpc( h, &h->out.bs, BLOCK_INDEX_CHROMA_DC, dct->chroma_dc[1], 4, mb );

        if( mb->i_cbp_chroma == 2 )
        {
            int i;
            for( i = 16; i < 24; i++ )
            {
                mb->cache.non_zero_count[x264_scan8[i]] = array_non_zero_count( dct->luma4x4[i] );
                block_residual_write_cavlc_tpc( h, &h->out.bs, i, dct->luma4x4[i]+1, 15, mb );
            }
        }
    }
    return h->out.bs.i_bits_encoded;
}

#endif



#ifdef RDO_SKIP_BS
/*****************************************************************************
 * RD only; doesn't generate a valid bitstream
 * doesn't write cbp or chroma dc (I don't know how much this matters)
 * works on all partition sizes except 16x16
 * for sub8x8, call once per 8x8 block
 *****************************************************************************/
static int x264_partition_size_cavlc( x264_t *h, int i8, int i_pixel )
{
    bs_t s;
    const int i_mb_type = h->mb.i_type;
    int j;

    s.i_bits_encoded = 0;

    if( i_mb_type == P_8x8 )
    {
        bs_write_ue( &s, sub_mb_type_p_to_golomb[ h->mb.i_sub_partition[i8] ] );
        if( h->mb.pic.i_fref[0] > 1 )
            bs_write_te( &s, h->mb.pic.i_fref[0] - 1, h->mb.cache.ref[0][x264_scan8[4*i8]] );
        cavlc_mb8x8_mvd( h, &s, 0, i8 );
    }
    else if( i_mb_type == P_L0 )
    {
        if( h->mb.pic.i_fref[0] > 1 )
            bs_write_te( &s, h->mb.pic.i_fref[0] - 1, h->mb.cache.ref[0][x264_scan8[4*i8]] );
        if( h->mb.i_partition == D_16x8 )
            cavlc_mb_mvd( h, &s, 0, 4*i8, 4 );
        else //8x16
            cavlc_mb_mvd( h, &s, 0, 4*i8, 2 );
    }
    else if( i_mb_type == B_8x8 )
    {
        bs_write_ue( &s, sub_mb_type_b_to_golomb[ h->mb.i_sub_partition[i8] ] );

        if( h->mb.pic.i_fref[0] > 1
            && x264_mb_partition_listX_table[0][ h->mb.i_sub_partition[i8] ] )
            bs_write_te( &s, h->mb.pic.i_fref[0] - 1, h->mb.cache.ref[0][x264_scan8[4*i8]] );
        if( h->mb.pic.i_fref[1] > 1
            && x264_mb_partition_listX_table[1][ h->mb.i_sub_partition[i8] ] )
            bs_write_te( &s, h->mb.pic.i_fref[1] - 1, h->mb.cache.ref[1][x264_scan8[4*i8]] );

        cavlc_mb8x8_mvd( h, &s, 0, i8 );
        cavlc_mb8x8_mvd( h, &s, 1, i8 );
    }
    else
    {
        x264_log(h, X264_LOG_ERROR, "invalid/unhandled mb_type\n" );
        return 0;
    }

    for( j = (i_pixel < PIXEL_8x8); j >= 0; j-- )
    {
        x264_macroblock_luma_write_cavlc( h, &s, i8, i8 );
        h->mb.cache.non_zero_count[x264_scan8[16+i8]] = array_non_zero_count( h->dct.luma4x4[16+i8] );
        block_residual_write_cavlc( h, &s, 16+i8, h->dct.luma4x4[16+i8]+1, 15 );
        h->mb.cache.non_zero_count[x264_scan8[20+i8]] = array_non_zero_count( h->dct.luma4x4[20+i8] );
        block_residual_write_cavlc( h, &s, 20+i8, h->dct.luma4x4[20+i8]+1, 15 );
        i8 += x264_pixel_size[i_pixel].h >> 3;
    }

    return s.i_bits_encoded;
}

static int cavlc_intra4x4_pred_size( x264_t *h, int i4, int i_mode )
{
    if( x264_mb_predict_intra4x4_mode( h, i4 ) == x264_mb_pred_mode4x4_fix( i_mode ) )
        return 1;
    else
        return 4;
}

static int x264_partition_i8x8_size_cavlc( x264_t *h, int i8, int i_mode )
{
    int i4, i;
    h->out.bs.i_bits_encoded = cavlc_intra4x4_pred_size( h, 4*i8, i_mode );
    for( i4 = 0; i4 < 4; i4++ )
    {
        for( i = 0; i < 16; i++ )
            h->dct.luma4x4[i4+i8*4][i] = h->dct.luma8x8[i8][i4+i*4];
        h->mb.cache.non_zero_count[x264_scan8[i4+i8*4]] =
            array_non_zero_count( h->dct.luma4x4[i4+i8*4] );
        block_residual_write_cavlc( h, &h->out.bs, i4+i8*4, h->dct.luma4x4[i4+i8*4], 16 );
    }
    return h->out.bs.i_bits_encoded;
}

static int x264_partition_i4x4_size_cavlc( x264_t *h, int i4, int i_mode )
{
    h->out.bs.i_bits_encoded = cavlc_intra4x4_pred_size( h, i4, i_mode );
    block_residual_write_cavlc( h, &h->out.bs, i4, h->dct.luma4x4[i4], 16 );
    return h->out.bs.i_bits_encoded;
}

static int x264_i8x8_chroma_size_cavlc( x264_t *h )
{
    h->out.bs.i_bits_encoded = bs_size_ue( x264_mb_pred_mode8x8c_fix[ h->mb.i_chroma_pred_mode ] );
    if( h->mb.i_cbp_chroma )
    {
        block_residual_write_cavlc( h, &h->out.bs, BLOCK_INDEX_CHROMA_DC, h->dct.chroma_dc[0], 4 );
        block_residual_write_cavlc( h, &h->out.bs, BLOCK_INDEX_CHROMA_DC, h->dct.chroma_dc[1], 4 );

        if( h->mb.i_cbp_chroma == 2 )
        {
            int i;
            for( i = 16; i < 24; i++ )
            {
                h->mb.cache.non_zero_count[x264_scan8[i]] = array_non_zero_count( h->dct.luma4x4[i] );
                block_residual_write_cavlc( h, &h->out.bs, i, h->dct.luma4x4[i]+1, 15 );
            }
        }
    }
    return h->out.bs.i_bits_encoded;
}
#endif


