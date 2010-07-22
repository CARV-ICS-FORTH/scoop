/*****************************************************************************
 * macroblock.h: h264 encoder library
 *****************************************************************************
 * Copyright (C) 2005-2008 x264 project
 *
 * Authors: Loren Merritt <lorenm@u.washington.edu>
 *          Laurent Aimar <fenrir@via.ecp.fr>
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

#ifndef X264_MACROBLOCK_TPC_H
#define X264_MACROBLOCK_TPC_H

#ifdef TPC

int x264_macroblock_cache_init_tpc( x264_t *h );
void x264_macroblock_cache_save_tpc( x264_t *h, x264_mb_t *mb);

void x264_macroblock_cache_reload_tpc_mv( x264_t *h, int i_mb_x, int i_mb_y, x264_mb_t *mb );


void x264_macroblock_cache_reload_tpc( x264_t *h, int i_mb_x, int i_mb_y, x264_mb_t *mb );
void x264_macroblock_cache_reload_tpc_x4( x264_t *h, int i_mb_x, int i_mb_y, x264_mb_t *mb );


void x264_macroblock_cache_resave_tpc( x264_t *h, x264_mb_t *mb );
void x264_macroblock_cache_resave_tpc_x4( x264_t *h, x264_mb_t *mb );
//void x264_macroblock_cache_load_tpc( x264_t *h, int i_mb_x, int i_mb_y, x264_mb_t *mb );
void x264_macroblock_cache_load_tpc( x264_t *h,  x264_mb_t *mb );
void x264_macroblock_cache_end_tpc( x264_t *h);

/* x264_mb_load_mv_direct8x8:
 *      set h->mb.cache.mv and h->mb.cache.ref for B_DIRECT
 *      must be called only after x264_mb_predict_mv_direct16x16 */
void x264_mb_load_mv_direct8x8_tpc(  int idx, x264_mb_t *mb );
/* x264_mb_predict_mv_direct16x16:
 *      set h->mb.cache.mv and h->mb.cache.ref for B_SKIP or B_DIRECT
 *      h->mb. need only valid values from other blocks.
 *      return 1 on success, 0 on failure.
 *      if b_changed != NULL, set it to whether refs or mvs differ from
 *      before this functioncall. */
int x264_mb_predict_mv_direct16x16_tpc( x264_t *h, int *b_changed, x264_mb_t *mb );

static ALWAYS_INLINE void x264_macroblock_cache_rect1_tpc( void *dst, int width, int height, uint8_t val )
{
    int dy;
    if( width == 4 )
    {
        uint32_t val2 = val * 0x01010101;
        for( dy = 0; dy < height; dy++ )
            ((uint32_t*)dst)[2*dy] = val2;
    }
    else // 2
    {
        uint32_t val2 = val * 0x0101;
        for( dy = 0; dy < height; dy++ )
            ((uint16_t*)dst)[4*dy] = val2;
    }
}
static ALWAYS_INLINE void x264_macroblock_cache_rect4_tpc( void *dst, int width, int height, uint32_t val )
{
    int dy, dx;
    if( width == 1 || WORD_SIZE < 8 )
    {
        for( dy = 0; dy < height; dy++ )
            for( dx = 0; dx < width; dx++ )
                ((uint32_t*)dst)[dx+8*dy] = val;
    }
    else
    {
        uint64_t val64 = val + ((uint64_t)val<<32);
        for( dy = 0; dy < height; dy++ )
            for( dx = 0; dx < width/2; dx++ )
                ((uint64_t*)dst)[dx+4*dy] = val64;
    }
}
#define x264_macroblock_cache_mv_ptr_tpc(x,y,w,h,l,mv,mb) x264_macroblock_cache_mv_tpc(x,y,w,h,l,*(uint32_t*)mv,mb)
static ALWAYS_INLINE void x264_macroblock_cache_mv_tpc(  int x, int y, int width, int height, int i_list, uint32_t mv, x264_mb_t *mb )
{
    x264_macroblock_cache_rect4_tpc( &(mb->cache.mv[i_list][X264_SCAN8_0+x+8*y]), width, height, mv );
}
static ALWAYS_INLINE void x264_macroblock_cache_mvd_tpc( int x, int y, int width, int height, int i_list, uint32_t mv, x264_mb_t *mb )
{
    x264_macroblock_cache_rect4_tpc( &(mb->cache.mvd[i_list][X264_SCAN8_0+x+8*y]), width, height, mv );
}
static ALWAYS_INLINE void x264_macroblock_cache_ref_tpc( int x, int y, int width, int height, int i_list, uint8_t ref, x264_mb_t *mb )
{
    x264_macroblock_cache_rect1( &mb->cache.ref[i_list][X264_SCAN8_0+x+8*y], width, height, ref );
}
static ALWAYS_INLINE void x264_macroblock_cache_skip_tpc(int x, int y, int width, int height, int b_skip, x264_mb_t *mb )
{
    x264_macroblock_cache_rect1( &mb->cache.skip[X264_SCAN8_0+x+8*y], width, height, b_skip );
}

void x264_mb_mc_tpc( x264_t *h, x264_mb_t *mb );

/* x264_mb_predict_mv_16x16_tpc:
 *      set mvp with predicted mv for D_16x16 block
 *      mb. need only valid values from other blocks */
void x264_mb_predict_mv_16x16_tpc(  int i_list, int i_ref, int16_t mvp[2], x264_mb_t *mb );


/* x264_mb_predict_mv_ref16x16_tpc:
 *      set mvc with D_16x16 prediction.
 *      uses all neighbors, even those that didn't end up using this ref.
 *      h->mb. need only valid values from other blocks */
void x264_mb_predict_mv_ref16x16_tpc( x264_t *h, int i_list, int i_ref, int16_t mvc[8][2], int *i_mvc, x264_mb_t *mb );

void x264_macroblock_slice_init_tpc( x264_t *h, x264_mb_t *mb );

/* x264_mb_predict_mv:
 *      set mvp with predicted mv for all blocks except SKIP and DIRECT
 *      h->mb. need valid ref/partition/sub of current block to be valid
 *      and valid mv/ref from other blocks. */
void x264_mb_predict_mv_tpc( x264_t *h, int i_list, int idx, int i_width, int16_t mvp[2], x264_mb_t *mb );


/* x264_mb_transform_8x8_allowed:
 *      check whether any partition is smaller than 8x8 (or at least
 *      might be, according to just partition type.)
 *      doesn't check for cbp */
static inline int x264_mb_transform_8x8_allowed_tpc( x264_t *h, x264_mb_t *mb )
{
    // intra and skip are disallowed
    // large partitions are allowed
    // direct and 8x8 are conditional
    static const uint8_t partition_tab[X264_MBTYPE_MAX] = {
        0,0,0,0,1,2,0,2,1,1,1,1,1,1,1,1,1,2,0,
    };
    int p, i;

    if( !h->pps->b_transform_8x8_mode )
        return 0;
    p = partition_tab[mb->i_type];
    if( p < 2 )
        return p;
    else if( mb->i_type == B_DIRECT )
        return h->sps->b_direct8x8_inference;
    else if( mb->i_type == P_8x8 )
    {
        if( !(h->param.analyse.inter & X264_ANALYSE_PSUB8x8) )
            return 1;
        for( i=0; i<4; i++ )
            if( mb->i_sub_partition[i] != D_L0_8x8 )
                return 0;
        return 1;
    }
    else // B_8x8
    {
        // x264 currently doesn't use sub-8x8 B partitions, so don't check for them
        if( h->sps->b_direct8x8_inference )
            return 1;
        for( i=0; i<4; i++ )
            if( mb->i_sub_partition[i] == D_DIRECT_8x8 )
                return 0;
        return 1;
    }
}

static inline int x264_mb_predict_non_zero_code_tpc(  int idx, x264_mb_t *mb )
{
    const int za = mb->cache.non_zero_count[x264_scan8[idx] - 1];
    const int zb = mb->cache.non_zero_count[x264_scan8[idx] - 8];

    int i_ret = za + zb;

    if( i_ret < 0x80 )
    {
        i_ret = ( i_ret + 1 ) >> 1;
    }
    return i_ret & 0x7f;
}





void x264_macroblock_store_pic_tpc( x264_t *h, int i, x264_mb_t *mb);

#endif


#if 0


static ALWAYS_INLINE uint32_t pack16to32( int a, int b )
{
#ifdef WORDS_BIGENDIAN
   return b + (a<<16);
#else
   return a + (b<<16);
#endif
}
static ALWAYS_INLINE uint32_t pack8to16( int a, int b )
{
#ifdef WORDS_BIGENDIAN
   return b + (a<<8);
#else
   return a + (b<<8);
#endif
}
static ALWAYS_INLINE uint32_t pack8to32( int a, int b, int c, int d )
{
#ifdef WORDS_BIGENDIAN
   return d + (c<<8) + (b<<16) + (a<<24);
#else
   return a + (b<<8) + (c<<16) + (d<<24);
#endif
}
static ALWAYS_INLINE uint32_t pack16to32_mask( int a, int b )
{
#ifdef WORDS_BIGENDIAN
   return (b&0xFFFF) + (a<<16);
#else
   return (a&0xFFFF) + (b<<16);
#endif
}
static ALWAYS_INLINE void x264_macroblock_cache_rect1( void *dst, int width, int height, uint8_t val )
{
    int dy;
    if( width == 4 )
    {
        uint32_t val2 = val * 0x01010101;
        for( dy = 0; dy < height; dy++ )
            ((uint32_t*)dst)[2*dy] = val2;
    }
    else // 2
    {
        uint32_t val2 = val * 0x0101;
        for( dy = 0; dy < height; dy++ )
            ((uint16_t*)dst)[4*dy] = val2;
    }
}
static ALWAYS_INLINE void x264_macroblock_cache_rect4( void *dst, int width, int height, uint32_t val )
{
    int dy, dx;
    if( width == 1 || WORD_SIZE < 8 )
    {
        for( dy = 0; dy < height; dy++ )
            for( dx = 0; dx < width; dx++ )
                ((uint32_t*)dst)[dx+8*dy] = val;
    }
    else
    {
        uint64_t val64 = val + ((uint64_t)val<<32);
        for( dy = 0; dy < height; dy++ )
            for( dx = 0; dx < width/2; dx++ )
                ((uint64_t*)dst)[dx+4*dy] = val64;
    }
}
#define x264_macroblock_cache_mv_ptr(a,x,y,w,h,l,mv) x264_macroblock_cache_mv(a,x,y,w,h,l,*(uint32_t*)mv)
static ALWAYS_INLINE void x264_macroblock_cache_mv( x264_t *h, int x, int y, int width, int height, int i_list, uint32_t mv )
{
    x264_macroblock_cache_rect4( &h->mb.cache.mv[i_list][X264_SCAN8_0+x+8*y], width, height, mv );
}
static ALWAYS_INLINE void x264_macroblock_cache_mvd( x264_t *h, int x, int y, int width, int height, int i_list, uint32_t mv )
{
    x264_macroblock_cache_rect4( &h->mb.cache.mvd[i_list][X264_SCAN8_0+x+8*y], width, height, mv );
}
static ALWAYS_INLINE void x264_macroblock_cache_ref( x264_t *h, int x, int y, int width, int height, int i_list, uint8_t ref )
{
    x264_macroblock_cache_rect1( &h->mb.cache.ref[i_list][X264_SCAN8_0+x+8*y], width, height, ref );
}
static ALWAYS_INLINE void x264_macroblock_cache_skip( x264_t *h, int x, int y, int width, int height, int b_skip )
{
    x264_macroblock_cache_rect1( &h->mb.cache.skip[X264_SCAN8_0+x+8*y], width, height, b_skip );
}
static ALWAYS_INLINE void x264_macroblock_cache_intra8x8_pred( x264_t *h, int x, int y, int i_mode )
{
    int8_t *cache = &h->mb.cache.intra4x4_pred_mode[X264_SCAN8_0+x+8*y];
    cache[0] = cache[1] = cache[8] = cache[9] = i_mode;
}
#define array_non_zero(a) array_non_zero_int(a, sizeof(a))
#define array_non_zero_int array_non_zero_int_c
static ALWAYS_INLINE int array_non_zero_int_c( void *v, int i_count )
{
    uint64_t *x = v;
    if(i_count == 8)
        return !!x[0];
    else if(i_count == 16)
        return !!(x[0]|x[1]);
    else if(i_count == 32)
        return !!(x[0]|x[1]|x[2]|x[3]);
    else
    {
        int i;
        i_count /= sizeof(uint64_t);
        for( i = 0; i < i_count; i++ )
            if( x[i] ) return 1;
        return 0;
    }
}
/* This function and its MMX version only work on arrays of size 16 */
static ALWAYS_INLINE int array_non_zero_count( int16_t *v )
{
    int i;
    int i_nz;

    for( i = 0, i_nz = 0; i < 16; i++ )
        if( v[i] )
            i_nz++;

    return i_nz;
}
#endif
static inline int x264_mb_predict_intra4x4_mode_tpc( x264_t *h, int idx, x264_mb_t *mb_t )
{
    const int ma = mb_t->cache.intra4x4_pred_mode[x264_scan8[idx] - 1];
    const int mb = mb_t->cache.intra4x4_pred_mode[x264_scan8[idx] - 8];
    const int m  = X264_MIN( x264_mb_pred_mode4x4_fix(ma),
                             x264_mb_pred_mode4x4_fix(mb) );

    if( m < 0 )
        return I_PRED_4x4_DC;

    return m;
}

#endif

