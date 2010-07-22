;

static void refine_subpel_tpc( x264_data_t *data, x264_me_t *m, int hpel_iters, int qpel_iters, int *p_halfpel_thresh, int b_refine_qpel, x264_mb_t *mb );

#define BITS_MVD( mx, my )\
    ((unsigned int) (p_cost_mvx[(mx)<<2] + (unsigned int ) p_cost_mvy[(my)<<2]))

#define COST_MV( mx, my )\
{\
    int cost = fpelcmpf[i_pixel]( m->p_fenc[0], FENC_STRIDE,\
                   &p_fref[(my)*m->i_stride[0]+(mx)], m->i_stride[0] )\
             + BITS_MVD(mx,my);\
    COPY3_IF_LT( bcost, cost, bmx, mx, bmy, my );\
}
 
#define COST_MV_HPEL( mx, my ) \
{ \
    int stride = 16; \
    uint8_t *src =  get_ref( pix, &stride, m->p_fref, m->i_stride[0], mx, my, bw, bh ); \
    int cost = fpelcmpf[i_pixel]( m->p_fenc[0], FENC_STRIDE, src, stride ) \
             + p_cost_mvx[ mx ] + p_cost_mvy[ my ]; \
    COPY3_IF_LT( bpred_cost, cost, bpred_mx, mx, bpred_my, my ); \
}

#define COST_MV_X3_DIR( m0x, m0y, m1x, m1y, m2x, m2y, costs )\
{\
    uint8_t *pix_base = p_fref + bmx + bmy*m->i_stride[0];\
    fpelcmp_x3f[i_pixel]( m->p_fenc[0],\
        pix_base + (m0x) + (m0y)*m->i_stride[0],\
        pix_base + (m1x) + (m1y)*m->i_stride[0],\
        pix_base + (m2x) + (m2y)*m->i_stride[0],\
        m->i_stride[0], costs );\
    (costs)[0] += BITS_MVD( bmx+(m0x), bmy+(m0y) );\
    (costs)[1] += BITS_MVD( bmx+(m1x), bmy+(m1y) );\
    (costs)[2] += BITS_MVD( bmx+(m2x), bmy+(m2y) );\
}

#define COST_MV_X4( m0x, m0y, m1x, m1y, m2x, m2y, m3x, m3y )\
{\
    uint8_t *pix_base = p_fref + omx + omy*m->i_stride[0];\
     fpelcmp_x4f[i_pixel]( m->p_fenc[0],\
        pix_base + (m0x) + (m0y)*m->i_stride[0],\
        pix_base + (m1x) + (m1y)*m->i_stride[0],\
        pix_base + (m2x) + (m2y)*m->i_stride[0],\
        pix_base + (m3x) + (m3y)*m->i_stride[0],\
        m->i_stride[0], costs );\
    costs[0] += BITS_MVD( omx+(m0x), omy+(m0y) );\
    costs[1] += BITS_MVD( omx+(m1x), omy+(m1y) );\
    costs[2] += BITS_MVD( omx+(m2x), omy+(m2y) );\
    costs[3] += BITS_MVD( omx+(m3x), omy+(m3y) );\
    COPY3_IF_LT( bcost, costs[0], bmx, omx+(m0x), bmy, omy+(m0y) );\
    COPY3_IF_LT( bcost, costs[1], bmx, omx+(m1x), bmy, omy+(m1y) );\
    COPY3_IF_LT( bcost, costs[2], bmx, omx+(m2x), bmy, omy+(m2y) );\
    COPY3_IF_LT( bcost, costs[3], bmx, omx+(m3x), bmy, omy+(m3y) );\
}

#define COST_MV_X3_ABS( m0x, m0y, m1x, m1y, m2x, m2y )\
{\
    fpelcmp_x3f[i_pixel]( m->p_fenc[0],\
        p_fref + (m0x) + (m0y)*m->i_stride[0],\
        p_fref + (m1x) + (m1y)*m->i_stride[0],\
        p_fref + (m2x) + (m2y)*m->i_stride[0],\
        m->i_stride[0], costs );\
    costs[0] += p_cost_mvx[(m0x)<<2]; /* no cost_mvy */\
    costs[1] += p_cost_mvx[(m1x)<<2];\
    costs[2] += p_cost_mvx[(m2x)<<2];\
    COPY3_IF_LT( bcost, costs[0], bmx, m0x, bmy, m0y );\
    COPY3_IF_LT( bcost, costs[1], bmx, m1x, bmy, m1y );\
    COPY3_IF_LT( bcost, costs[2], bmx, m2x, bmy, m2y );\
}

/*  1  */
/* 101 */
/*  1  */
#define DIA1_ITER( mx, my )\
{\
    omx = mx; omy = my;\
    COST_MV_X4( 0,-1, 0,1, -1,0, 1,0 );\
}

#define CROSS( start, x_max, y_max )\
{\
    i = start;\
    if( x_max <= X264_MIN(mv_x_max-omx, omx-mv_x_min) )\
        for( ; i < x_max-2; i+=4 )\
            COST_MV_X4( i,0, -i,0, i+2,0, -i-2,0 );\
    for( ; i < x_max; i+=2 )\
    {\
        if( omx+i <= mv_x_max )\
            COST_MV( omx+i, omy );\
        if( omx-i >= mv_x_min )\
            COST_MV( omx-i, omy );\
    }\
    i = start;\
    if( y_max <= X264_MIN(mv_y_max-omy, omy-mv_y_min) )\
        for( ; i < y_max-2; i+=4 )\
            COST_MV_X4( 0,i, 0,-i, 0,i+2, 0,-i-2 );\
    for( ; i < y_max; i+=2 )\
    {\
        if( omy+i <= mv_y_max )\
            COST_MV( omx, omy+i );\
        if( omy-i >= mv_y_min )\
            COST_MV( omx, omy-i );\
    }\
}

void x264_me_search_ref_tpc( x264_data_t *data, x264_me_t *m, int16_t (*mvc)[2], int i_mvc, int *p_halfpel_thresh, x264_mb_t *mb )
{
    const int bw = x264_pixel_size[m->i_pixel].w;
    const int bh = x264_pixel_size[m->i_pixel].h;
    const int i_pixel = m->i_pixel;
    int i_me_range = data->i_me_range;
    int bmx, bmy, bcost;
    int bpred_mx = 0, bpred_my = 0, bpred_cost = COST_MAX;
    int omx, omy, pmx, pmy;
    uint8_t *p_fref = m->p_fref[0];
    int8_t pix[16*16] ;

    int i = 0, j;
    int dir;
    int costs[6];

#if 0
    mb->mv_min_fpel[0] = -data->i_me_range ;
    mb->mv_min_fpel[1] = -data->i_me_range ;
    mb->mv_max_fpel[0] =  data->i_me_range +16;
    mb->mv_max_fpel[1] =  data->i_me_range +16;
#endif
    int mv_x_min = mb->mv_min_fpel[0];
    int mv_y_min = mb->mv_min_fpel[1];
    int mv_x_max = mb->mv_max_fpel[0];
    int mv_y_max = mb->mv_max_fpel[1];


#define CHECK_MVRANGE(mx,my) ( mx >= mv_x_min && mx <= mv_x_max && my >= mv_y_min && my <= mv_y_max )

    const int16_t *p_cost_mvx = m->p_cost_mv - m->mvp[0];
    const int16_t *p_cost_mvy = m->p_cost_mv - m->mvp[1];
    bmx = x264_clip3( m->mvp[0], mv_x_min*4, mv_x_max*4 );
    bmy = x264_clip3( m->mvp[1], mv_y_min*4, mv_y_max*4 );
    pmx = ( bmx + 2 ) >> 2;
    pmy = ( bmy + 2 ) >> 2;
    bcost = COST_MAX;

    /* try extra predictors if provided */
    if( mb->i_subpel_refine >= 3 )
    {
        uint32_t bmv = pack16to32_mask(bmx,bmy);
        COST_MV_HPEL( bmx, bmy );
        do
        {
            if( *(uint32_t*)mvc[i] && (bmv - *(uint32_t*)mvc[i]) )
            {
                int mx = x264_clip3( mvc[i][0], mv_x_min*4, mv_x_max*4 );
                int my = x264_clip3( mvc[i][1], mv_y_min*4, mv_y_max*4 );
                COST_MV_HPEL( mx, my );
            }
        } while( ++i < i_mvc );
        bmx = ( bpred_mx + 2 ) >> 2;
        bmy = ( bpred_my + 2 ) >> 2;
        COST_MV( bmx, bmy );
    }
    else
    {
        /* check the MVP */
        COST_MV( pmx, pmy );
        /* Because we are rounding the predicted motion vector to fullpel, there will be
         * an extra MV cost in 15 out of 16 cases.  However, when the predicted MV is
         * chosen as the best predictor, it is often the case that the subpel search will
         * result in a vector at or next to the predicted motion vector.  Therefore, it is
         * sensible to remove the cost of the MV from the rounded MVP to avoid unfairly
         * biasing against use of the predicted motion vector. */
        bcost -= BITS_MVD( pmx, pmy );
        do
        {
            int mx = (mvc[i][0] + 2) >> 2;
            int my = (mvc[i][1] + 2) >> 2;
            if( (mx | my) && ((mx-bmx) | (my-bmy)) )
            {
                mx = x264_clip3( mx, mv_x_min, mv_x_max );
                my = x264_clip3( my, mv_y_min, mv_y_max );
                COST_MV( mx, my );
            }
        } while( ++i < i_mvc );
    }


     COST_MV( 0, 0 );




    switch( mb->i_me_method )
    {
    case X264_ME_DIA:
        /* diamond search, radius 1 */
        i = 0;
        do
        {
            DIA1_ITER( bmx, bmy );
            if( (bmx == omx) & (bmy == omy) )
                break;
            if( !CHECK_MVRANGE(bmx, bmy) )
                break;

        } while( ++i < i_me_range );
        break;


    case X264_ME_HEX:
me_hex2:
        /* hexagon search, radius 2 */
#if 0

#else
        /* equivalent to the above, but eliminates duplicate candidates */
        dir = -2;

        /* hexagon */
        COST_MV_X3_DIR( -2,0, -1, 2,  1, 2, costs   );
        COST_MV_X3_DIR(  2,0,  1,-2, -1,-2, costs+3 );
        COPY2_IF_LT( bcost, costs[0], dir, 0 );
        COPY2_IF_LT( bcost, costs[1], dir, 1 );
        COPY2_IF_LT( bcost, costs[2], dir, 2 );
        COPY2_IF_LT( bcost, costs[3], dir, 3 );
        COPY2_IF_LT( bcost, costs[4], dir, 4 );
        COPY2_IF_LT( bcost, costs[5], dir, 5 );

        if( dir != -2 )
        {
            bmx += hex2[dir+1][0];
            bmy += hex2[dir+1][1];
            /* half hexagon, not overlapping the previous iteration */
            for( i = 1; i < i_me_range/2 && CHECK_MVRANGE(bmx, bmy); i++ )
            {
                const int odir = mod6m1[dir+1];
                COST_MV_X3_DIR( hex2[odir+0][0], hex2[odir+0][1],
                                hex2[odir+1][0], hex2[odir+1][1],
                                hex2[odir+2][0], hex2[odir+2][1],
                                costs );
                dir = -2;
                COPY2_IF_LT( bcost, costs[0], dir, odir-1 );
                COPY2_IF_LT( bcost, costs[1], dir, odir   );
                COPY2_IF_LT( bcost, costs[2], dir, odir+1 );
                if( dir == -2 )
                    break;
                bmx += hex2[dir+1][0];
                bmy += hex2[dir+1][1];
            }
        }
#endif
        /* square refine */
        omx = bmx; omy = bmy;
        COST_MV_X4(  0,-1,  0,1, -1,0, 1,0 );
        COST_MV_X4( -1,-1, -1,1, 1,-1, 1,1 );
        break;

    case X264_ME_UMH:
        {
            /* Uneven-cross Multi-Hexagon-grid Search
             * as in JM, except with different early termination */

            static const int x264_pixel_size_shift[7] = { 0, 1, 1, 2, 3, 3, 4 };

            int ucost1, ucost2;
            int cross_start = 1;

            /* refine predictors */
            ucost1 = bcost;
            DIA1_ITER( pmx, pmy );
            if( pmx | pmy )
                DIA1_ITER( 0, 0 );

            if(i_pixel == PIXEL_4x4)
                goto me_hex2;

            ucost2 = bcost;
            if( (bmx | bmy) && ((bmx-pmx) | (bmy-pmy)) )
                DIA1_ITER( bmx, bmy );
            if( bcost == ucost2 )
                cross_start = 3;
            omx = bmx; omy = bmy;

            /* early termination */
#define SAD_THRESH(v) ( bcost < ( v >> x264_pixel_size_shift[i_pixel] ) )
            if( bcost == ucost2 && SAD_THRESH(2000) )
            {
                COST_MV_X4( 0,-2, -1,-1, 1,-1, -2,0 );
                COST_MV_X4( 2, 0, -1, 1, 1, 1,  0,2 );
                if( bcost == ucost1 && SAD_THRESH(500) )
                    break;
                if( bcost == ucost2 )
                {
                    int range = (i_me_range>>1) | 1;
                    CROSS( 3, range, range );
                    COST_MV_X4( -1,-2, 1,-2, -2,-1, 2,-1 );
                    COST_MV_X4( -2, 1, 2, 1, -1, 2, 1, 2 );
                    if( bcost == ucost2 )
                        break;
                    cross_start = range + 2;
                }
            }
            /* adaptive search range */
            if( i_mvc )
            {
                /* range multipliers based on casual inspection of some statistics of
                 * average distance between current predictor and final mv found by ESA.
                 * these have not been tuned much by actual encoding. */
                static const int range_mul[4][4] =
                {
                    { 3, 3, 4, 4 },
                    { 3, 4, 4, 4 },
                    { 4, 4, 4, 5 },
                    { 4, 4, 5, 6 },
                };
                int mvd;
                int sad_ctx, mvd_ctx;
                int denom = 1;

                if( i_mvc == 1 )
                {
                    if( i_pixel == PIXEL_16x16 )
                        /* mvc is probably the same as mvp, so the difference isn't meaningful.
                         * but prediction usually isn't too bad, so just use medium range */
                        mvd = 25;
                    else
                        mvd = abs( m->mvp[0] - mvc[0][0] )
                            + abs( m->mvp[1] - mvc[0][1] );
                }
                else
                {
                    /* calculate the degree of agreement between predictors. */
                    /* in 16x16, mvc includes all the neighbors used to make mvp,
                     * so don't count mvp separately. */
                    denom = i_mvc - 1;
                    mvd = 0;
                    if( i_pixel != PIXEL_16x16 )
                    {
                        mvd = abs( m->mvp[0] - mvc[0][0] )
                            + abs( m->mvp[1] - mvc[0][1] );
                        denom++;
                    }
                    mvd += x264_predictor_difference( mvc, i_mvc );
                }

                sad_ctx = SAD_THRESH(1000) ? 0
                        : SAD_THRESH(2000) ? 1
                        : SAD_THRESH(4000) ? 2 : 3;
                mvd_ctx = mvd < 10*denom ? 0
                        : mvd < 20*denom ? 1
                        : mvd < 40*denom ? 2 : 3;

                i_me_range = i_me_range * range_mul[mvd_ctx][sad_ctx] / 4;
            }

            /* FIXME if the above DIA2/OCT2/CROSS found a new mv, it has not updated omx/omy.
             * we are still centered on the same place as the DIA2. is this desirable? */
            CROSS( cross_start, i_me_range, i_me_range/2 );

            COST_MV_X4( -2,-2, -2,2, 2,-2, 2,2 );

            /* hexagon grid */
            omx = bmx; omy = bmy;
            i = 1;
            do
            {
                static const int hex4[16][2] = {
                    {-4, 2}, {-4, 1}, {-4, 0}, {-4,-1}, {-4,-2},
                    { 4,-2}, { 4,-1}, { 4, 0}, { 4, 1}, { 4, 2},
                    { 2, 3}, { 0, 4}, {-2, 3},
                    {-2,-3}, { 0,-4}, { 2,-3},
                };

                if( 4*i > X264_MIN4( mv_x_max-omx, omx-mv_x_min,
                                     mv_y_max-omy, omy-mv_y_min ) )
                {
                    for( j = 0; j < 16; j++ )
                    {
                        int mx = omx + hex4[j][0]*i;
                        int my = omy + hex4[j][1]*i;
                        if( CHECK_MVRANGE(mx, my) )
                            COST_MV( mx, my );
                    }
                }
                else
                {
                    COST_MV_X4( -4*i, 2*i, -4*i, 1*i, -4*i, 0*i, -4*i,-1*i );
                    COST_MV_X4( -4*i,-2*i,  4*i,-2*i,  4*i,-1*i,  4*i, 0*i );
                    COST_MV_X4(  4*i, 1*i,  4*i, 2*i,  2*i, 3*i,  0*i, 4*i );
                    COST_MV_X4( -2*i, 3*i, -2*i,-3*i,  0*i,-4*i,  2*i,-3*i );
                }
            } while( ++i <= i_me_range/4 );
            if( bmy <= mv_y_max )
                goto me_hex2;
            break;
        }


    case X264_ME_OLD_ESA:
    case X264_ME_ESA:
        {
            const int min_x = X264_MAX( bmx - i_me_range, mv_x_min );
            const int min_y = X264_MAX( bmy - i_me_range, mv_y_min );
            const int max_x = X264_MIN( bmx + i_me_range, mv_x_max );
            const int max_y = X264_MIN( bmy + i_me_range, mv_y_max );
	/* Check for the range */
            int my;
            /* plain old exhaustive search */
            int mx;
            for( my = min_y; my < max_y; my++ )
                for( mx = min_x; mx < max_x; mx++ )
                    COST_MV( mx, my );



        }
        break;
/* No FULL ESA */
#if 0
#endif
        }
        break;

#endif
    }

    /* -> qpel mv */
    if( bpred_cost < bcost )
    {
        m->mv[0] = bpred_mx;
        m->mv[1] = bpred_my;
        m->cost = bpred_cost;
    }
    else
    {
        m->mv[0] = bmx << 2;
        m->mv[1] = bmy << 2;
        m->cost = bcost;
    }



    /* compute the real cost */
    m->cost_mv = p_cost_mvx[ m->mv[0] ] + p_cost_mvy[ m->mv[1] ];
    if( bmx == pmx && bmy == pmy && mb->i_subpel_refine < 3 )
        m->cost += m->cost_mv;
    /* subpel refine */
    if( mb->i_subpel_refine >= 2 )
    {
        int hpel = subpel_iterations[mb->i_subpel_refine][2];
        int qpel = subpel_iterations[mb->i_subpel_refine][3];
        refine_subpel_tpc( data, m, hpel, qpel, p_halfpel_thresh, 1, mb );
    }
    else if( m->mv[1] > mb->mv_max_spel[1] )
        m->mv[1] = mb->mv_max_spel[1];

    /* Check for the range */
    if( m->mv[1] > mb->mv_max_spel[1] )  m->mv[1] = mb->mv_max_spel[1];
    if( m->mv[1] < mb->mv_min_spel[1] )  m->mv[1] = mb->mv_min_spel[1];
    if( m->mv[0] > mb->mv_max_spel[1] )  m->mv[0] = mb->mv_max_spel[0];
    if( m->mv[0] < mb->mv_min_spel[1] )  m->mv[0] = mb->mv_min_spel[0];




}
#undef COST_MV




void x264_me_refine_qpel_tpc( x264_data_t *data, x264_me_t *m, x264_mb_t *mb )
{
    int hpel = subpel_iterations[mb->i_subpel_refine][0];
    int qpel = subpel_iterations[mb->i_subpel_refine][1];

    if( m->i_pixel <= PIXEL_8x8 && mb->i_type == SLICE_TYPE_P )
        m->cost -= m->i_ref_cost;
	
    refine_subpel_tpc( data, m, hpel, qpel, NULL, 1, mb );
}

#define COST_MV_SAD( mx, my ) \
{ \
    int stride = 16; \
    uint8_t *src = get_ref( pix[0], &stride, m->p_fref, m->i_stride[0], mx, my, bw, bh ); \
    int cost = fpelcmpf[i_pixel]( m->p_fenc[0], FENC_STRIDE, src, stride ) \
             + p_cost_mvx[ mx ] + p_cost_mvy[ my ]; \
    COPY3_IF_LT( bcost, cost, bmx, mx, bmy, my ); \
}

#define COST_MV_SATD( mx, my, dir ) \
if( b_refine_qpel || (dir^1) != odir ) \
{ \
    int stride = 16; \
    uint8_t *src = get_ref( pix[0], &stride, m->p_fref, m->i_stride[0], mx, my, bw, bh ); \
    int cost = mbcmpf[i_pixel]( m->p_fenc[0], FENC_STRIDE, src, stride ) \
             + p_cost_mvx[ mx ] + p_cost_mvy[ my ]; \
    if( b_chroma_me && cost < bcost ) \
    { \
        \
        mc_chroma( pix[0], 8, m->p_fref[4], m->i_stride[1], mx, my, bw/2, bh/2 ); \
        cost += mbcmpf[i_pixel+3]( m->p_fenc[1], FENC_STRIDE, pix[0], 8 ); \
        if( cost < bcost ) \
        { \
            mc_chroma( pix[0], 8, m->p_fref[5], m->i_stride[1], mx, my, bw/2, bh/2 ); \
            cost += mbcmpf[i_pixel+3]( m->p_fenc[2], FENC_STRIDE, pix[0], 8 ); \
        } \
    } \
    if( cost < bcost ) \
    {                  \
        bcost = cost;  \
        bmx = mx;      \
        bmy = my;      \
        bdir = dir;    \
    } \
}

static void refine_subpel_tpc( x264_data_t *data, x264_me_t *m, int hpel_iters, int qpel_iters, int *p_halfpel_thresh, int b_refine_qpel, x264_mb_t *mb )
{
    const int bw = x264_pixel_size[m->i_pixel].w;
    const int bh = x264_pixel_size[m->i_pixel].h;
    const int16_t *p_cost_mvx = m->p_cost_mv - m->mvp[0];
    const int16_t *p_cost_mvy = m->p_cost_mv - m->mvp[1];
    const int i_pixel = m->i_pixel;
    const int b_chroma_me = mb->b_chroma_me && i_pixel <= PIXEL_8x8;
    uint8_t pix[2][32*18]; // really 17x17, but round up for alignment
    int omx, omy;
    int i;

    int bmx = m->mv[0];
    int bmy = m->mv[1];
    int bcost = m->cost;
    int odir = -1, bdir;

    /* try the subpel component of the predicted mv */
    if( hpel_iters && mb->i_subpel_refine < 3 )
    {
        int mx = x264_clip3( m->mvp[0], mb->mv_min_spel[0], mb->mv_max_spel[0] );
        int my = x264_clip3( m->mvp[1], mb->mv_min_spel[1], mb->mv_max_spel[1] );
        if( (mx-bmx)|(my-bmy) )
            COST_MV_SAD( mx, my );
    }




    /* SATD calls  m->p_fref[4] and  m->p_fref[5] - We don't have them in SPE so just disable them */
    if( !b_refine_qpel )
    {
        /* check for mvrange */
        if( bmy > mb->mv_max_spel[1] )
            bmy = mb->mv_max_spel[1];
        bcost = COST_MAX;
        COST_MV_SATD( bmx, bmy, -1 );
    }

    m->cost = bcost;
    m->mv[0] = bmx;
    m->mv[1] = bmy;
    m->cost_mv = p_cost_mvx[ bmx ] + p_cost_mvy[ bmy ];
    return;
#if 0
#endif

#if 0

#endif
#if 1
#undef COST_MV_SATD
#define COST_MV_SATD( mx, my, dst ) \
{ \
    int stride = 16; \
    uint8_t *src = get_ref( pix, &stride, m->p_fref, m->i_stride[0], mx, my, bw*4, bh*4 ); \
    dst = mbcmpf[i_pixel]( m->p_fenc[0], FENC_STRIDE, src, stride ) \
        + p_cost_mvx[mx] + p_cost_mvy[my]; \
    COPY1_IF_LT( bsatd, dst ); \
}

#define COST_MV_RD( mx, my, satd, do_dir, mdir ) \
{ \
    if( satd <= bsatd * SATD_THRESH )\
    { \
        uint64_t cost; \
        *(uint32_t*)cache_mv = *(uint32_t*)cache_mv2 = pack16to32_mask(mx,my); \
        cost = x264_rd_cost_part_tpc( data, i_lambda2, i8, m->i_pixel, mb ); \
        COPY4_IF_LT( bcost, cost, bmx, mx, bmy, my, dir, do_dir?mdir:dir ); \
    } \
}

#define SATD_THRESH 17/16

void x264_me_refine_qpel_rd_tpc(  x264_data_t *data,  x264_me_t *m, int i_lambda2, int i8, x264_mb_t *mb )
{
    // don't have to fill the whole mv cache rectangle
    static const int pixel_mv_offs[] = { 0, 4, 4*8, 0 };
    int16_t *cache_mv = mb->cache.mv[0][x264_scan8[i8*4]];
    int16_t *cache_mv2 = cache_mv + pixel_mv_offs[m->i_pixel];
    const int16_t *p_cost_mvx, *p_cost_mvy;
    const int bw = x264_pixel_size[m->i_pixel].w>>2;
    const int bh = x264_pixel_size[m->i_pixel].h>>2;
    const int i_pixel = m->i_pixel;

    DECLARE_ALIGNED_16( uint8_t pix[16*16] );
    uint64_t bcost = m->i_pixel == PIXEL_16x16 ? m->cost : COST_MAX64;
    int bmx = m->mv[0];
    int bmy = m->mv[1];
    int omx = bmx;
    int omy = bmy;
    int pmx, pmy, i, j;
    unsigned bsatd;
    int satd = 0;
    int dir = -2;
    int satds[8];

    if( m->i_pixel != PIXEL_16x16 && i8 != 0 )
        x264_mb_predict_mv_tpc( data, 0, i8*4, bw, m->mvp, mb );
    pmx = m->mvp[0];
    pmy = m->mvp[1];
    p_cost_mvx = m->p_cost_mv - pmx;
    p_cost_mvy = m->p_cost_mv - pmy;
    COST_MV_SATD( bmx, bmy, bsatd );
  //  COST_MV_RD( bmx, bmy, 0, 0, 0 );

    /* check the predicted mv */
    if( (bmx != pmx || bmy != pmy)
        && pmx >= mb->mv_min_spel[0] && pmx <= mb->mv_max_spel[0]
        && pmy >= mb->mv_min_spel[1] && pmy <= mb->mv_max_spel[1] )
    {
        COST_MV_SATD( pmx, pmy, satd );
//        COST_MV_RD( pmx, pmy, satd, 0,0 );
    }

    /* subpel hex search, same pattern as ME HEX. */
    dir = -2;
    omx = bmx;
    omy = bmy;
    for( j=0; j<6; j++ ) COST_MV_SATD( omx + hex2[j+1][0], omy + hex2[j+1][1], satds[j] );
//    for( j=0; j<6; j++ ) COST_MV_RD  ( omx + hex2[j+1][0], omy + hex2[j+1][1], satds[j], 1,j );
    if( dir != -2 )
    {
        /* half hexagon, not overlapping the previous iteration */
        for( i = 1; i < 10; i++ )
        {
            const int odir = mod6m1[dir+1];
            if( bmy > mb->mv_max_spel[1] - 2 ||
                bmy < mb->mv_min_spel[1] - 2 )
                break;
            dir = -2;
            omx = bmx;
            omy = bmy;
            for( j=0; j<3; j++ ) COST_MV_SATD( omx + hex2[odir+j][0], omy + hex2[odir+j][1], satds[j] );
//            for( j=0; j<3; j++ ) COST_MV_RD  ( omx + hex2[odir+j][0], omy + hex2[odir+j][1], satds[j], 1, odir-1+j );
            if( dir == -2 )
                break;
        }
    }

    /* square refine, same as pattern as ME HEX. */
    omx = bmx;
    omy = bmy;
    for( i=0; i<8; i++ ) COST_MV_SATD( omx + square1[i][0], omy  + square1[i][1], satds[i] );
//    for( i=0; i<8; i++ ) COST_MV_RD  ( omx + square1[i][0], omy  + square1[i][1], satds[i], 0,0 );

    bmy = x264_clip3( bmy, mb->mv_min_spel[1],  mb->mv_max_spel[1] );
    m->cost = bcost;
    m->mv[0] = bmx;
    m->mv[1] = bmy;
    x264_macroblock_cache_mv_tpc(  2*(i8&1), i8&2, bw, bh, 0, pack16to32_mask(bmx, bmy), mb );
    x264_macroblock_cache_mvd_tpc( 2*(i8&1), i8&2, bw, bh, 0, pack16to32_mask(bmx - pmx, bmy - pmy), mb );
}
#endif

