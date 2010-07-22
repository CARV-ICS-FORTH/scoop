/*****************************************************************************
 * rdo.c: h264 encoder library (rate-distortion optimization)
 *****************************************************************************
 * Copyright (C) 2005-2008 Loren Merritt <lorenm@u.washington.edu>
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

/* duplicate all the writer functions, just calculating bit cost
 * instead of writing the bitstream.
 * TODO: use these for fast 1st pass too. */

#include <stdint.h>

#include "common/spu/common_tpc.h"
#include "encoder/spu/macroblock_tpc.h"
#include "common/spu/macroblock_tpc.h"



#define RDO_SKIP_BS

//static uint8_t cabac_prefix_transition[15][128];
//static uint16_t cabac_prefix_size[15][128];
/* CAVLC: produces exactly the same bit count as a normal encode */
/* this probably still leaves some unnecessary computations */
/*#define bs_write1(s,v)     ((s)->i_bits_encoded += 1)
#define bs_write(s,n,v)    ((s)->i_bits_encoded += (n))
#define bs_write_ue(s,v)   ((s)->i_bits_encoded += bs_size_ue(v))
#define bs_write_se(s,v)   ((s)->i_bits_encoded += bs_size_se(v))
#define bs_write_te(s,v,l) ((s)->i_bits_encoded += bs_size_te(v,l))*/
//#define x264_macroblock_write_cavlc_tpc   x264_macroblock_size_cavlc_tpc
#ifdef ENTROPY
#include "cavlc_tpc.c"
#endif

#if 0
/* CABAC: not exactly the same. x264_cabac_size_decision() keeps track of
 * fractional bits, but only finite precision. */
#undef  x264_cabac_encode_decision
#define x264_cabac_encode_decision(c,x,v) x264_cabac_size_decision(c,x,v)
#define x264_cabac_encode_terminal(c)     x264_cabac_size_decision(c,276,0)
#define x264_cabac_encode_bypass(c,v)     ((c)->f8_bits_encoded += 256)
#define x264_cabac_encode_ue_bypass(c,e,v) ((c)->f8_bits_encoded += (bs_size_ue_big(v+(1<<e)-1)-e)<<8)
#define x264_cabac_encode_flush(h,c)
#define x264_macroblock_write_cabac  static x264_macroblock_size_cabac
#define x264_macroblock_write_cabac_tpc  static x264_macroblock_size_cabac_tpc
#include "cabac.c"

#define COPY_CABAC h->mc.memcpy_aligned( &cabac_tmp.f8_bits_encoded, &h->cabac.f8_bits_encoded, \
        sizeof(x264_cabac_t) - offsetof(x264_cabac_t,f8_bits_encoded) )

#endif

#if 0
/* Sum the cached SATDs to avoid repeating them. */
int sum_satd_tpc( x264_data_t *data, int pixel, int x, int y, x264_mb_t *mb )
{
    int satd = 0;
    int min_x = x>>2;
    int min_y = y>>2;
    int max_x = (x>>2) + (x264_pixel_size[pixel].w>>2);
    int max_y = (y>>2) + (x264_pixel_size[pixel].h>>2);
    if( pixel == PIXEL_16x16 )
        return mb->pic.fenc_satd_sum;
    for( y = min_y; y < max_y; y++ )
        for( x = min_x; x < max_x; x++ )
            satd += mb->pic.fenc_satd[y][x];
    return satd;
}

/* Psy RD distortion metric: SSD plus "Absolute Difference of Complexities" */
/* SATD and SA8D are used to measure block complexity. */
/* The difference between SATD and SA8D scores are both used to avoid bias from the DCT size.  Using SATD */
/* only, for example, results in overusage of 8x8dct, while the opposite occurs when using SA8D. */

/* FIXME:  Is there a better metric than averaged SATD/SA8D difference for complexity difference? */
/* Hadamard transform is recursive, so a SATD+SA8D can be done faster by taking advantage of this fact. */
/* This optimization can also be used in non-RD transform decision. */

static inline int ssd_plane_tpc( x264_data_t *data, int size, int p, int x, int y, x264_mb_t *mb )
{
    DECLARE_ALIGNED_16(static uint8_t zero[16]);
    int satd = 0;
    uint8_t *fdec = mb->pic.p_fdec[p] + x + y*FDEC_STRIDE;
    uint8_t *fenc = mb->pic.p_fenc[p] + x + y*FENC_STRIDE;
    if( p == 0 && mb->i_psy_rd )
    {
#if 0
        /* If the plane is smaller than 8x8, we can't do an SA8D; this probably isn't a big problem. */
        if( size <= PIXEL_8x8 )
        {
            uint64_t acs = h->pixf.hadamard_ac[size]( fdec, FDEC_STRIDE );
            satd = abs((int32_t)acs - sum_satd( h, size, x, y ))
                 + abs((int32_t)(acs>>32) - sum_sa8d( h, size, x, y ));
            satd >>= 1;
        }
        else
        {
            int dc = h->pixf.sad[size]( fdec, FDEC_STRIDE, zero, 0 ) >> 1;
            satd = abs(satdf[size]( fdec, FDEC_STRIDE, zero, 0 ) - dc - sum_satd( h, size, x, y ));
        }

#else

	{
            int dc = sadf[size]( fdec, FDEC_STRIDE, zero, 0 ) >> 1;
            satd = abs(satdf[size]( fdec, FDEC_STRIDE, zero, 0 ) - dc - sum_satd_tpc( data, size, x, y, mb ));
	}

#endif

        satd = (satd * mb->i_psy_rd * x264_lambda_tab[mb->i_qp] + 128) >> 8;
    }
    return ssdf[size](fenc, FENC_STRIDE, fdec, FDEC_STRIDE) + satd;
}




inline int ssd_mb_tpc( x264_data_t *data, x264_mb_t *mb )
{
    return ssd_plane_tpc(data, PIXEL_16x16, 0, 0, 0, mb)
         + ssd_plane_tpc(data, PIXEL_8x8,   1, 0, 0, mb)
         + ssd_plane_tpc(data, PIXEL_8x8,   2, 0, 0, mb);
}

#endif
#if 0

static int x264_rd_cost_mb_tpc( x264_data_t *data, int i_lambda2, x264_mb_t *mb, x264_dct_t *dct )
{
    int b_transform_bak = mb->b_transform_8x8;
    int i_ssd;
    int i_bits;

    x264_macroblock_encode_tpc( data, mb, dct );

    i_ssd = ssd_mb_tpc( data, mb );

    if( IS_SKIP( mb->i_type ) )
    {
        i_bits = (1 * i_lambda2 + 128) >> 8;
    }

#if 0
    else if( h->param.b_cabac )  //// TODO: INSERT MB SUPPORT 
    {
        assert(0);
        x264_cabac_t cabac_tmp;
        COPY_CABAC;
        x264_macroblock_size_cabac_tpc( h, &cabac_tmp, mb );
        i_bits = ( (uint64_t)cabac_tmp.f8_bits_encoded * i_lambda2 + 32768 ) >> 16;
    }

    else
    {
        bs_t bs_tmp = h->out.bs;
        bs_tmp.i_bits_encoded = 0;
        x264_macroblock_size_cavlc_tpc( h, &bs_tmp, mb, dct );
        i_bits = ( bs_tmp.i_bits_encoded * i_lambda2 + 128 ) >> 8;
    }

#endif
    mb->b_transform_8x8 = b_transform_bak;

    return i_ssd + i_bits;
}

static uint64_t x264_rd_cost_i8x8_chroma_tpc( x264_data_t *data, int i_lambda2, int i_mode, int b_dct, x264_mb_t *mb, x264_dct_t *dct )
{
    uint64_t i_ssd, i_bits;

    if( b_dct )
        x264_mb_encode_8x8_chroma_tpc( data, 0, mb->i_chroma_qp, mb );
    i_ssd = ssd_plane_tpc( data, PIXEL_8x8, 1, 0, 0, mb ) +
            ssd_plane_tpc( data, PIXEL_8x8, 2, 0, 0, mb );

    mb->i_chroma_pred_mode = i_mode;

#if 0
    if( h->param.b_cabac )
    {

        x264_cabac_t cabac_tmp;
        COPY_CABAC;
        x264_i8x8_chroma_size_cabac( h, &cabac_tmp );
        i_bits = ( (uint64_t)cabac_tmp.f8_bits_encoded * i_lambda2 + 128 ) >> 8;

    }
    else
    {
        i_bits = x264_i8x8_chroma_size_cavlc_tpc( h, mb, dct ) * i_lambda2;
    }

#endif
    return (i_ssd<<8) + i_bits;
}
#endif
# if 0

// TODO:
// support chroma and i16x16 DC
// save cabac state between blocks?
// use trellis' RD score instead of x264_mb_decimate_score?
// code 8x8 sig/last flags forwards with deadzone and save the contexts at
//   each position?
// change weights when using CQMs?

// possible optimizations:
// make scores fit in 32bit
// save quantized coefs during rd, to avoid a duplicate trellis in the final encode
// if trellissing all MBRD modes, finish SSD calculation so we can skip all of
//   the normal dequant/idct/ssd/cabac

// the unquant_mf here is not the same as dequant_mf:
// in normal operation (dct->quant->dequant->idct) the dct and idct are not
// normalized. quant/dequant absorb those scaling factors.
// in this function, we just do (quant->unquant) and want the output to be
// comparable to the input. so unquant is the direct inverse of quant,
// and uses the dct scaling factors, not the idct ones.

static inline void quant_trellis_cabac_tpc( x264_t *h, int16_t *dct,
                                 const uint16_t *quant_mf, const int *unquant_mf,
                                 const int *coef_weight, const uint8_t *zigzag,
                                 int i_ctxBlockCat, int i_lambda2, int b_ac, int i_coefs, int idx, x264_mb_t *mb )
{
    int abs_coefs[64], signs[64];
    trellis_node_t nodes[2][8];
    trellis_node_t *nodes_cur = nodes[0];
    trellis_node_t *nodes_prev = nodes[1];
    trellis_node_t *bnode;
    uint8_t cabac_state_sig[64];
    uint8_t cabac_state_last[64];
    const int b_interlaced = mb->b_interlaced;
    const int f = 1 << 15; // no deadzone
    int i_last_nnz;
    int i, j;

    // (# of coefs) * (# of ctx) * (# of levels tried) = 1024
    // we don't need to keep all of those: (# of coefs) * (# of ctx) would be enough,
    // but it takes more time to remove dead states than you gain in reduced memory.
    struct {
        uint16_t abs_level;
        uint16_t next;
    } level_tree[64*8*2];
    int i_levels_used = 1;

    /* init coefs */
    for( i = i_coefs-1; i >= b_ac; i-- )
        if( (unsigned)(dct[zigzag[i]] * quant_mf[zigzag[i]] + f-1) >= 2*f )
            break;

    if( i < b_ac )
    {
        memset( dct, 0, i_coefs * sizeof(*dct) );
        return;
    }

    i_last_nnz = i;

    for( ; i >= b_ac; i-- )
    {
        int coef = dct[zigzag[i]];
        abs_coefs[i] = abs(coef);
        signs[i] = coef < 0 ? -1 : 1;
    }

    /* init trellis */
    for( i = 1; i < 8; i++ )
        nodes_cur[i].score = TRELLIS_SCORE_MAX;
    nodes_cur[0].score = 0;
    nodes_cur[0].level_idx = 0;
    level_tree[0].abs_level = 0;
    level_tree[0].next = 0;

    // coefs are processed in reverse order, because that's how the abs value is coded.
    // last_coef and significant_coef flags are normally coded in forward order, but
    // we have to reverse them to match the levels.
    // in 4x4 blocks, last_coef and significant_coef use a separate context for each
    // position, so the order doesn't matter, and we don't even have to update their contexts.
    // in 8x8 blocks, some positions share contexts, so we'll just have to hope that
    // cabac isn't too sensitive.

    if( i_coefs == 64 )
    {
        const uint8_t *ctx_sig  = &h->cabac.state[ significant_coeff_flag_offset[b_interlaced][i_ctxBlockCat] ];
        const uint8_t *ctx_last = &h->cabac.state[ last_coeff_flag_offset[b_interlaced][i_ctxBlockCat] ];
        for( i = 0; i < 63; i++ )
        {
            cabac_state_sig[i]  = ctx_sig[ significant_coeff_flag_offset_8x8[b_interlaced][i] ];
            cabac_state_last[i] = ctx_last[ last_coeff_flag_offset_8x8[i] ];
        }
    }
    else
    {
        memcpy( cabac_state_sig,  &h->cabac.state[ significant_coeff_flag_offset[b_interlaced][i_ctxBlockCat] ], 15 );
        memcpy( cabac_state_last, &h->cabac.state[ last_coeff_flag_offset[b_interlaced][i_ctxBlockCat] ], 15 );
    }
    memcpy( nodes_cur[0].cabac_state, &h->cabac.state[ coeff_abs_level_m1_offset[i_ctxBlockCat] ], 10 );

    for( i = i_last_nnz; i >= b_ac; i-- )
    {
        int i_coef = abs_coefs[i];
        int q = ( f + i_coef * quant_mf[zigzag[i]] ) >> 16;
        int abs_level;
        int cost_sig[2], cost_last[2];
        trellis_node_t n;

        // skip 0s: this doesn't affect the output, but saves some unnecessary computation.
        if( q == 0 )
        {
            // no need to calculate ssd of 0s: it's the same in all nodes.
            // no need to modify level_tree for ctx=0: it starts with an infinite loop of 0s.
            const uint32_t cost_sig0 = x264_cabac_size_decision_noup( &cabac_state_sig[i], 0 )
                                     * (uint64_t)i_lambda2 >> ( CABAC_SIZE_BITS - LAMBDA_BITS );
            for( j = 1; j < 8; j++ )
            {
                if( nodes_cur[j].score != TRELLIS_SCORE_MAX )
                {
#define SET_LEVEL(n,l) \
                    level_tree[i_levels_used].abs_level = l; \
                    level_tree[i_levels_used].next = n.level_idx; \
                    n.level_idx = i_levels_used; \
                    i_levels_used++;

                    SET_LEVEL( nodes_cur[j], 0 );
                    nodes_cur[j].score += cost_sig0;
                }
            }
            continue;
        }

        XCHG( trellis_node_t*, nodes_cur, nodes_prev );

        for( j = 0; j < 8; j++ )
            nodes_cur[j].score = TRELLIS_SCORE_MAX;

        if( i < i_coefs-1 )
        {
            cost_sig[0] = x264_cabac_size_decision_noup( &cabac_state_sig[i], 0 );
            cost_sig[1] = x264_cabac_size_decision_noup( &cabac_state_sig[i], 1 );
            cost_last[0] = x264_cabac_size_decision_noup( &cabac_state_last[i], 0 );
            cost_last[1] = x264_cabac_size_decision_noup( &cabac_state_last[i], 1 );
        }
        else
        {
            cost_sig[0] = cost_sig[1] = 0;
            cost_last[0] = cost_last[1] = 0;
        }

        // there are a few cases where increasing the coeff magnitude helps,
        // but it's only around .003 dB, and skipping them ~doubles the speed of trellis.
        // could also try q-2: that sometimes helps, but also sometimes decimates blocks
        // that are better left coded, especially at QP > 40.
        for( abs_level = q; abs_level >= q-1; abs_level-- )
        {
            int unquant_abs_level = ((unquant_mf[zigzag[i]] * abs_level + 128) >> 8);
            int d = i_coef - unquant_abs_level;
            int64_t ssd;
            /* Psy trellis: bias in favor of higher AC coefficients in the reconstructed frame. */
            if( mb->i_psy_trellis && i )
            {
                int orig_coef = (i_coefs == 64) ? mb->pic.fenc_dct8[idx][i] : mb->pic.fenc_dct4[idx][i];
                int predicted_coef = orig_coef - i_coef * signs[i];
                int psy_value = mb->i_psy_trellis * abs(predicted_coef + unquant_abs_level * signs[i]);
                int psy_weight = (i_coefs == 64) ? x264_dct8_weight_tab[zigzag[i]] : x264_dct4_weight_tab[zigzag[i]];
                ssd = (int64_t)d*d * coef_weight[i] - psy_weight * psy_value;
            }
            else
                ssd = (int64_t)d*d * coef_weight[i];

            for( j = 0; j < 8; j++ )
            {
                int node_ctx = j;
                if( nodes_prev[j].score == TRELLIS_SCORE_MAX )
                    continue;
                n = nodes_prev[j];

                /* code the proposed level, and count how much entropy it would take */
                if( abs_level || node_ctx )
                {
                    unsigned f8_bits = cost_sig[ abs_level != 0 ];
                    if( abs_level )
                    {
                        const int i_prefix = X264_MIN( abs_level - 1, 14 );
                        f8_bits += cost_last[ node_ctx == 0 ];
                        f8_bits += x264_cabac_size_decision2( &n.cabac_state[coeff_abs_level1_ctx[node_ctx]], i_prefix > 0 );
                        if( i_prefix > 0 )
                        {
                            uint8_t *ctx = &n.cabac_state[coeff_abs_levelgt1_ctx[node_ctx]];
                            f8_bits += cabac_prefix_size[i_prefix][*ctx];
                            *ctx = cabac_prefix_transition[i_prefix][*ctx];
                            if( abs_level >= 15 )
                                f8_bits += bs_size_ue( abs_level - 15 ) << CABAC_SIZE_BITS;
                            node_ctx = coeff_abs_level_transition[1][node_ctx];
                        }
                        else
                        {
                            f8_bits += 1 << CABAC_SIZE_BITS;
                            node_ctx = coeff_abs_level_transition[0][node_ctx];
                        }
                    }
                    n.score += (uint64_t)f8_bits * i_lambda2 >> ( CABAC_SIZE_BITS - LAMBDA_BITS );
                }

                n.score += ssd;

                /* save the node if it's better than any existing node with the same cabac ctx */
                if( n.score < nodes_cur[node_ctx].score )
                {
                    SET_LEVEL( n, abs_level );
                    nodes_cur[node_ctx] = n;
                }
            }
        }
    }

    /* output levels from the best path through the trellis */
    bnode = &nodes_cur[0];
    for( j = 1; j < 8; j++ )
        if( nodes_cur[j].score < bnode->score )
            bnode = &nodes_cur[j];

    j = bnode->level_idx;
    for( i = b_ac; i < i_coefs; i++ )
    {
        dct[zigzag[i]] = level_tree[j].abs_level * signs[i];
        j = level_tree[j].next;
    }
}


void x264_quant_4x4_trellis_tpc( x264_t *h, int16_t dct[4][4], int i_quant_cat,
                             int i_qp, int i_ctxBlockCat, int b_intra, int idx, x264_mb_t *mb )
{
    int b_ac = (i_ctxBlockCat == DCT_LUMA_AC);
    quant_trellis_cabac_tpc( h, (int16_t*)dct,
        h->quant4_mf[i_quant_cat][i_qp], h->unquant4_mf[i_quant_cat][i_qp],
        x264_dct4_weight2_zigzag[mb->b_interlaced],
        x264_zigzag_scan4[mb->b_interlaced],
        i_ctxBlockCat, lambda2_tab[b_intra][i_qp], b_ac, 16, idx, mb );
}


void x264_quant_8x8_trellis_tpc( x264_t *h, int16_t dct[8][8], int i_quant_cat,
                             int i_qp, int b_intra, int idx, x264_mb_t *mb )
{
    quant_trellis_cabac_tpc( h, (int16_t*)dct,
        h->quant8_mf[i_quant_cat][i_qp], h->unquant8_mf[i_quant_cat][i_qp],
        x264_dct8_weight2_zigzag[mb->b_interlaced],
        x264_zigzag_scan8[mb->b_interlaced],
        DCT_LUMA_8x8, lambda2_tab[b_intra][i_qp], 0, 64, idx, mb );
}


#endif


