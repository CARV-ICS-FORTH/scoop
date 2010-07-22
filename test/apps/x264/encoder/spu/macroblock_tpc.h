/*****************************************************************************
 * macroblock.h: h264 encoder library
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

#ifndef X264_ENCODER_MACROBLOCK_TPC_H
#define X264_ENCODER_MACROBLOCK_TPC_H


extern const int x264_lambda2_tab[52];
extern const int x264_lambda_tab[52];


int x264_macroblock_probe_skip_tpc( x264_data_t *data, int b_bidir, x264_mb_t *mb );
static inline int x264_macroblock_probe_pskip_tpc( x264_data_t *data, x264_mb_t *mb )
    { return x264_macroblock_probe_skip_tpc( data, 0, mb ); }
static inline int x264_macroblock_probe_bskip_tpc( x264_data_t *data, x264_mb_t *mb )
    { return x264_macroblock_probe_skip_tpc( data, 1, mb ); }



#if 0

//void x264_macroblock_encode_tpc      ( x264_t *h, x264_mb_t *mb, x264_dct_t *dct );
void x264_quant_4x4_trellis_tpc( x264_t *h, int16_t dct[4][4], int i_quant_cat,
                             int i_qp, int i_ctxBlockCat, int b_intra, int idx, x264_mb_t *mb );
void x264_quant_8x8_trellis_tpc( x264_t *h, int16_t dct[8][8], int i_quant_cat,
                             int i_qp, int b_intra, int idx, x264_mb_t *mb );



#endif



#endif

