#include <math.h>
#include <limits.h>
#ifndef _MSC_VER
#include <unistd.h>
#endif


#include "common/common.h"
#include "encoder/macroblock.h"
#include "encoder/me.h"
#include "encoder/me_tpc.h"
#include "encoder/ratecontrol.h"
#include "encoder/analyse.h"

#include "common/macroblock_tpc.h"

#include <ppu_intrinsics.h>
#include <altivec.h>

#ifdef TPC


extern uint64_t x264_call_ticks;

DECLARE_ALIGNED_16(uint8_t mb_non_zero_count[4][X264_SCAN8_SIZE]);

#include "../../libtpc/include/tpc_common.h"
#include "../../libtpc/include/tpc_ppe.h"

#define ALIGN(x,a) (((x)+((a)-1))&~((a)-1))

#if 0
#define ISSUE_TASK()  					\
{							\
	uint64_t curr = __mftb();			\
	uint64_t diff = curr - h->last_time_sample;	\
	uint16_t idle = diff/SAMPLE_TIME ;		\
	h->last_time_sample = curr;			\
	h->pos += idle; 				\
	if ( h->pos < SAMPLES)				\
		h->outstanding[h->pos]++;		\
}							\

#else
#define ISSUE_TASK()  					\
{							\
}	

#endif


#if 1

void x264_macroblock_call_analyse_tpc(  x264_t *h, x264_mb_t *mb, int mb_i_xy, x264_dct_t *dct, int i_mb_x, int i_mb_y  ){
   
    uint64_t tmp = __mftb();

 
    const int mb_xy = mb_i_xy;
    x264_data_t *data = h->x264_data_table[mb_xy];
    uint8_t *chroma_ref0;
    uint8_t *chroma_ref1;
    uint8_t *chroma_ref2;
    uint8_t *chroma_ref3;

    int range = h->param.analyse.i_me_range*2 + 32   ;
    int chroma_range = range/2 +16 ;

    const int i_me_range = h->param.analyse.i_me_range ;
    const int i_me_chroma_range =  h->param.analyse.i_me_range/2  ;



    PREFETCH(data);
    const int i_stride = h->mb.pic.i_stride[0];
    const int chroma_i_stride0 = h->mb.pic.i_stride[1];
    const int chroma_i_stride1 = h->mb.pic.i_stride[2];

    /* Strides for decoded frame - */
    const int i_stride_dec_luma = h->fdec->i_stride[0];
    const int i_stride_dec_chroma = h->fdec->i_stride[1];

    data-> i_first_mb        =  h->sh.i_first_mb;
    data-> mb_i_xy           =  mb_xy ;
    data-> i_type            =  h->sh.i_type;
    data-> i_qp              =  mb->i_last_qp ;
    data-> i_chroma_qp       =  mb->i_chroma_qp ;
    data-> i_first_mb        =  h->sh.i_first_mb;		
    data-> i_type            =  h->sh.i_type;		
    data-> b_mbaff           =  h->sh.b_mbaff;			



    /* for reference frames*/
    const uint8_t * ref0 =  mb->pic.p_fref[0][0][0] - ( i_me_range + i_me_range*i_stride );
    const uint8_t * ref1 =  mb->pic.p_fref[1][0][0] - ( i_me_range + i_me_range*i_stride );

    chroma_ref0 =  mb->pic.p_fref[0][0][4] - ( i_me_chroma_range + i_me_chroma_range*chroma_i_stride0  );
    chroma_ref1 =  mb->pic.p_fref[0][0][5] - ( i_me_chroma_range + i_me_chroma_range*chroma_i_stride1  );

    chroma_ref2 =  mb->pic.p_fref[1][0][4] - ( i_me_chroma_range + i_me_chroma_range*chroma_i_stride0  );
    chroma_ref3 =  mb->pic.p_fref[1][0][5] - ( i_me_chroma_range + i_me_chroma_range*chroma_i_stride1  );



    /* calculate the offset to shift in SPE */ 
    data->offset =  ( (( unsigned long) chroma_ref0 )%16)  ;
   
    chroma_ref0 = (uint8_t *) (( (unsigned long) chroma_ref0 ) & 0xfffffff0 ) ;
    chroma_ref1 = (uint8_t *) (( (unsigned long) chroma_ref1 ) & 0xfffffff0 ) ;

    chroma_ref2 = (uint8_t *) (( (unsigned long) chroma_ref2 ) & 0xfffffff0 ) ;
    chroma_ref3 = (uint8_t *) (( (unsigned long) chroma_ref3 ) & 0xfffffff0 ) ;


    /* Plane fenc prepare */
    const int i_stride2_luma = i_stride_dec_luma << mb->b_interlaced;
    const int i_stride2_chroma = i_stride_dec_chroma << mb->b_interlaced;


    /* Offsets - w=16 for luma and w=8 for chroma */
    const int i_pix_offset_luma = mb->b_interlaced
                           ? 16 * (i_mb_x + (i_mb_y&~1) * i_stride_dec_luma) + (i_mb_y&1) * i_stride_dec_luma
                           : 16 * (i_mb_x + i_mb_y * i_stride_dec_luma);

    const int i_pix_offset_chroma = mb->b_interlaced
                           ? 8 * (i_mb_x + (i_mb_y&~1) * i_stride_dec_chroma) + (i_mb_y&1) * i_stride_dec_chroma
                           : 8 * (i_mb_x + i_mb_y * i_stride_dec_chroma);


    /* for plane fenc - index is 0 for luma  and 1,2 for chroma */
    const uint8_t * fenc_luma    = (uint8_t *) (( (unsigned long) &h->fenc->plane[0][i_pix_offset_luma]   ) & 0xfffffff0 )   ;
    const uint8_t * fenc_chroma1 = (uint8_t *) (( (unsigned long) &h->fenc->plane[1][i_pix_offset_chroma] ) & 0xfffffff0 ) ;
    const uint8_t * fenc_chroma2 = (uint8_t *) (( (unsigned long) &h->fenc->plane[2][i_pix_offset_chroma] ) & 0xfffffff0 ) ;

    data->offset2 = (( (unsigned long) &h->fenc->plane[1][i_pix_offset_chroma] ) % 16 ) ;

    /* for plane fdec - index is 0 for luma  and 1,2 for chroma */
    const uint8_t * fdec_luma    = (uint8_t *) (( (unsigned long) &h->fdec->plane[0][i_pix_offset_luma]   ) & 0xfffffff0 )   ;
    const uint8_t * fdec_chroma1 = (uint8_t *) (( (unsigned long) &h->fdec->plane[1][i_pix_offset_chroma] ) & 0xfffffff0 ) ;
    const uint8_t * fdec_chroma2 = (uint8_t *) (( (unsigned long) &h->fdec->plane[2][i_pix_offset_chroma] ) & 0xfffffff0 ) ;

    data->offset3 = (( (unsigned long) &h->fdec->plane[1][i_pix_offset_chroma] ) % 16 ) ;


    /* This code is for range  */
    int luma_range_y = range;
    int chroma_range_y = chroma_range;

    /* number of MBs */
    int i_height_mb = h->param.i_height/16;
    int i_width_mb = h->param.i_width/16;
    int i_mb_threshold = 32/16 + h->param.analyse.i_me_range/16;








    ISSUE_TASK(); 
    x264_call_ticks += (__mftb() -tmp );

#ifdef STAT
   uint64_t   tmp_time1 = __mftb();
#endif


    TASK_ENTRY entry;

     

//#define SET_ARGUMENT( task, arg_i, arg_ea, arg_size, arg_type, arg_stride )  

    if ( h->sh.i_type == SLICE_TYPE_B){


	    if ( mb->i_mb_y > ( i_height_mb - i_mb_threshold)  ){
		    luma_range_y = range - (16)*( -1 + i_mb_threshold - (i_height_mb - mb->i_mb_y)); 
		    chroma_range_y = chroma_range - (8)*( -1 + i_mb_threshold - (i_height_mb - mb->i_mb_y)); 
	    }


	    SET_ARGUMENT( entry, 0, mb->non_zero_count[mb->i_mb_xy],  32, TPC_OUT_ARG, 0 );
	    SET_ARGUMENT( entry, 1,    fdec_luma, TPC_BUILD_STRIDEARG(16,16),  TPC_STRIDE_ARG|TPC_OUT_ARG,  i_stride2_luma   );
	    SET_ARGUMENT( entry, 2, fdec_chroma1, TPC_BUILD_STRIDEARG( 8,16),  TPC_STRIDE_ARG|TPC_OUT_ARG,  i_stride2_chroma );
	    SET_ARGUMENT( entry, 3, fdec_chroma2, TPC_BUILD_STRIDEARG( 8,16),  TPC_STRIDE_ARG|TPC_OUT_ARG,  i_stride2_chroma );


	    SET_ARGUMENT( entry, 4,           mb,                sizeof(*mb),               TPC_INOUT_ARG,                 0 );
	    SET_ARGUMENT( entry, 5,          dct,               sizeof(*dct),               TPC_INOUT_ARG,                 0 );
	    SET_ARGUMENT( entry, 6,         data,              sizeof(*data),                  TPC_IN_ARG,                 0 );

            SET_ARGUMENT( entry, 7,         ref0, TPC_BUILD_STRIDEARG(luma_range_y,range),          TPC_STRIDE_ARG|TPC_IN_ARG, i_stride);
            SET_ARGUMENT( entry, 8,  chroma_ref0, TPC_BUILD_STRIDEARG(chroma_range_y,chroma_range), TPC_STRIDE_ARG|TPC_IN_ARG, chroma_i_stride0);
    	    SET_ARGUMENT( entry, 9,  chroma_ref1, TPC_BUILD_STRIDEARG(chroma_range_y,chroma_range), TPC_STRIDE_ARG|TPC_IN_ARG, chroma_i_stride1);

    	    SET_ARGUMENT( entry, 10,          ref1, TPC_BUILD_STRIDEARG(luma_range_y,range),          TPC_STRIDE_ARG|TPC_IN_ARG, i_stride);
       	    SET_ARGUMENT( entry, 11,  chroma_ref2, TPC_BUILD_STRIDEARG(chroma_range_y,chroma_range) ,TPC_STRIDE_ARG|TPC_IN_ARG, chroma_i_stride0);
       	    SET_ARGUMENT( entry, 12,  chroma_ref3, TPC_BUILD_STRIDEARG(chroma_range_y,chroma_range) ,     TPC_STRIDE_ARG|TPC_IN_ARG, chroma_i_stride1);

            SET_ARGUMENT( entry, 13,     fenc_luma,    TPC_BUILD_STRIDEARG(16,16), TPC_STRIDE_ARG|TPC_IN_ARG, i_stride2_luma);
	    SET_ARGUMENT( entry, 14,  fenc_chroma1,    TPC_BUILD_STRIDEARG(8,16), TPC_STRIDE_ARG|TPC_IN_ARG, i_stride2_chroma );
	    SET_ARGUMENT( entry, 15,  fenc_chroma2,    TPC_BUILD_STRIDEARG(8,16), TPC_STRIDE_ARG|TPC_IN_ARG, i_stride2_chroma );


	    h->tpc_id_table[mb_i_xy] = tpc_call_descriptor (1, 16, &entry);






   } else if ( h->sh.i_type == SLICE_TYPE_P ) {

    if ( mb->i_mb_y > ( i_height_mb - i_mb_threshold)  ){
	   luma_range_y = range - (16)*( -2 + i_mb_threshold - (i_height_mb - mb->i_mb_y)); 
	   chroma_range_y = chroma_range - (8)*( -1 + i_mb_threshold - (i_height_mb - mb->i_mb_y)); 
    }

	    SET_ARGUMENT( entry, 0, mb->non_zero_count[mb->i_mb_xy],  32, TPC_OUT_ARG, 0 );
	    SET_ARGUMENT( entry, 1,    fdec_luma, TPC_BUILD_STRIDEARG(16,16),  TPC_STRIDE_ARG|TPC_OUT_ARG,  i_stride2_luma   );
	    SET_ARGUMENT( entry, 2, fdec_chroma1, TPC_BUILD_STRIDEARG( 8,16),  TPC_STRIDE_ARG|TPC_OUT_ARG,  i_stride2_chroma );
	    SET_ARGUMENT( entry, 3, fdec_chroma2, TPC_BUILD_STRIDEARG( 8,16),  TPC_STRIDE_ARG|TPC_OUT_ARG,  i_stride2_chroma );


	    SET_ARGUMENT( entry, 4,           mb,                sizeof(*mb),               TPC_INOUT_ARG,                 0 );
	    SET_ARGUMENT( entry, 5,          dct,               sizeof(*dct),               TPC_INOUT_ARG,                 0 );
	    SET_ARGUMENT( entry, 6,         data,              sizeof(*data),                  TPC_IN_ARG,                 0 );

            SET_ARGUMENT( entry, 7,         ref0, TPC_BUILD_STRIDEARG(luma_range_y,range),          TPC_STRIDE_ARG|TPC_IN_ARG, i_stride);
            SET_ARGUMENT( entry, 8,  chroma_ref0, TPC_BUILD_STRIDEARG(chroma_range_y,chroma_range), TPC_STRIDE_ARG|TPC_IN_ARG, chroma_i_stride0);
    	    SET_ARGUMENT( entry, 9,  chroma_ref1, TPC_BUILD_STRIDEARG(chroma_range_y,chroma_range), TPC_STRIDE_ARG|TPC_IN_ARG, chroma_i_stride1);

 

            SET_ARGUMENT( entry, 10,     fenc_luma,    TPC_BUILD_STRIDEARG(16,16), TPC_STRIDE_ARG|TPC_IN_ARG, i_stride2_luma);
	    SET_ARGUMENT( entry, 11,  fenc_chroma1,    TPC_BUILD_STRIDEARG(8,16), TPC_STRIDE_ARG|TPC_IN_ARG, i_stride2_chroma );
	    SET_ARGUMENT( entry, 12,  fenc_chroma2,    TPC_BUILD_STRIDEARG(8,16), TPC_STRIDE_ARG|TPC_IN_ARG, i_stride2_chroma );



	    h->tpc_id_table[mb_i_xy] = tpc_call_descriptor (1, 13, &entry);



   }

#ifdef STAT
     stats[h->sh.i_type][CALL] += ( __mftb() - tmp_time1 );
#endif
}



#else

void x264_macroblock_call_analyse_tpc(  x264_t *h, x264_mb_t *mb, int mb_i_xy, x264_dct_t *dct, int i_mb_x, int i_mb_y  ){
   
    uint64_t tmp = __mftb();

 
    const int mb_xy = mb_i_xy;
    x264_data_t *data = h->x264_data_table[mb_xy];
    uint8_t *chroma_ref0;
    uint8_t *chroma_ref1;
    uint8_t *chroma_ref2;
    uint8_t *chroma_ref3;

    int range = h->param.analyse.i_me_range*2 + 32   ;
    int chroma_range = range/2 +16 ;

    const int i_me_range = h->param.analyse.i_me_range ;
    const int i_me_chroma_range =  h->param.analyse.i_me_range/2  ;



    PREFETCH(data);
    const int i_stride = h->mb.pic.i_stride[0];
    const int chroma_i_stride0 = h->mb.pic.i_stride[1];
    const int chroma_i_stride1 = h->mb.pic.i_stride[2];

    /* Strides for decoded frame - */
    const int i_stride_dec_luma = h->fdec->i_stride[0];
    const int i_stride_dec_chroma = h->fdec->i_stride[1];

    data-> i_first_mb        =  h->sh.i_first_mb;
    data-> mb_i_xy           =  mb_xy ;
    data-> i_type            =  h->sh.i_type;
    data-> i_qp              =  mb->i_last_qp ;
    data-> i_chroma_qp       =  mb->i_chroma_qp ;
    data-> i_first_mb        =  h->sh.i_first_mb;		
    data-> i_type            =  h->sh.i_type;		
    data-> b_mbaff           =  h->sh.b_mbaff;			



    /* for reference frames*/
    const uint8_t * ref0 =  mb->pic.p_fref[0][0][0] - ( i_me_range + i_me_range*i_stride );
    const uint8_t * ref1 =  mb->pic.p_fref[1][0][0] - ( i_me_range + i_me_range*i_stride );

    chroma_ref0 =  mb->pic.p_fref[0][0][4] - ( i_me_chroma_range + i_me_chroma_range*chroma_i_stride0  );
    chroma_ref1 =  mb->pic.p_fref[0][0][5] - ( i_me_chroma_range + i_me_chroma_range*chroma_i_stride1  );

    chroma_ref2 =  mb->pic.p_fref[1][0][4] - ( i_me_chroma_range + i_me_chroma_range*chroma_i_stride0  );
    chroma_ref3 =  mb->pic.p_fref[1][0][5] - ( i_me_chroma_range + i_me_chroma_range*chroma_i_stride1  );



    /* calculate the offset to shift in SPE */ 
    data->offset =  ( (( unsigned long) chroma_ref0 )%16)  ;
   
    chroma_ref0 = (uint8_t *) (( (unsigned long) chroma_ref0 ) & 0xfffffff0 ) ;
    chroma_ref1 = (uint8_t *) (( (unsigned long) chroma_ref1 ) & 0xfffffff0 ) ;

    chroma_ref2 = (uint8_t *) (( (unsigned long) chroma_ref2 ) & 0xfffffff0 ) ;
    chroma_ref3 = (uint8_t *) (( (unsigned long) chroma_ref3 ) & 0xfffffff0 ) ;


    /* Plane fenc prepare */
    const int i_stride2_luma = i_stride_dec_luma << mb->b_interlaced;
    const int i_stride2_chroma = i_stride_dec_chroma << mb->b_interlaced;


    /* Offsets - w=16 for luma and w=8 for chroma */
    const int i_pix_offset_luma = mb->b_interlaced
                           ? 16 * (i_mb_x + (i_mb_y&~1) * i_stride_dec_luma) + (i_mb_y&1) * i_stride_dec_luma
                           : 16 * (i_mb_x + i_mb_y * i_stride_dec_luma);

    const int i_pix_offset_chroma = mb->b_interlaced
                           ? 8 * (i_mb_x + (i_mb_y&~1) * i_stride_dec_chroma) + (i_mb_y&1) * i_stride_dec_chroma
                           : 8 * (i_mb_x + i_mb_y * i_stride_dec_chroma);


    /* for plane fenc - index is 0 for luma  and 1,2 for chroma */
    const uint8_t * fenc_luma    = (uint8_t *) (( (unsigned long) &h->fenc->plane[0][i_pix_offset_luma]   ) & 0xfffffff0 )   ;
    const uint8_t * fenc_chroma1 = (uint8_t *) (( (unsigned long) &h->fenc->plane[1][i_pix_offset_chroma] ) & 0xfffffff0 ) ;
    const uint8_t * fenc_chroma2 = (uint8_t *) (( (unsigned long) &h->fenc->plane[2][i_pix_offset_chroma] ) & 0xfffffff0 ) ;

    data->offset2 = (( (unsigned long) &h->fenc->plane[1][i_pix_offset_chroma] ) % 16 ) ;

    /* for plane fdec - index is 0 for luma  and 1,2 for chroma */
    const uint8_t * fdec_luma    = (uint8_t *) (( (unsigned long) &h->fdec->plane[0][i_pix_offset_luma]   ) & 0xfffffff0 )   ;
    const uint8_t * fdec_chroma1 = (uint8_t *) (( (unsigned long) &h->fdec->plane[1][i_pix_offset_chroma] ) & 0xfffffff0 ) ;
    const uint8_t * fdec_chroma2 = (uint8_t *) (( (unsigned long) &h->fdec->plane[2][i_pix_offset_chroma] ) & 0xfffffff0 ) ;

    data->offset3 = (( (unsigned long) &h->fdec->plane[1][i_pix_offset_chroma] ) % 16 ) ;


    /* This code is for range  */
    int luma_range_y = range;
    int chroma_range_y = chroma_range;

    /* number of MBs */
    int i_height_mb = h->param.i_height/16;
    int i_width_mb = h->param.i_width/16;
    int i_mb_threshold = 32/16 + h->param.analyse.i_me_range/16;




    ISSUE_TASK(); 
    x264_call_ticks += (__mftb() -tmp );

#ifdef STAT
   uint64_t   tmp_time1 = __mftb();
#endif

     if ( h->sh.i_type == SLICE_TYPE_B){


    if ( mb->i_mb_y > ( i_height_mb - i_mb_threshold)  ){
	   luma_range_y = range - (16)*( -1 + i_mb_threshold - (i_height_mb - mb->i_mb_y)); 
	   chroma_range_y = chroma_range - (8)*( -1 + i_mb_threshold - (i_height_mb - mb->i_mb_y)); 
    }

     h->tpc_id_table[mb_i_xy] = tpc_call(1, 15,
       	             fdec_luma,
       	                   TPC_BUILD_STRIDEARG(16,16) ,
       	                   TPC_STRIDE_ARG|TPC_OUT_ARG, i_stride2_luma,

       	             fdec_chroma1,
       	                   TPC_BUILD_STRIDEARG(8,16) ,
       	                   TPC_STRIDE_ARG|TPC_OUT_ARG, i_stride2_chroma,

       	             fdec_chroma2,
       	                   TPC_BUILD_STRIDEARG(8,16) ,
       	                   TPC_STRIDE_ARG|TPC_OUT_ARG, i_stride2_chroma,

       	             mb,   sizeof(*mb),   TPC_INOUT_ARG,
                     dct,  sizeof(*dct),  TPC_INOUT_ARG,
       	             data, sizeof(*data), TPC_IN_ARG, 
          
       	             ref0,
       	                   TPC_BUILD_STRIDEARG(luma_range_y,range) ,
       	                   TPC_STRIDE_ARG|TPC_IN_ARG, i_stride,

       	             chroma_ref0,
       	                   TPC_BUILD_STRIDEARG(chroma_range_y,chroma_range) ,
       	                   TPC_STRIDE_ARG|TPC_IN_ARG, chroma_i_stride0,

       	             chroma_ref1,
       	                   TPC_BUILD_STRIDEARG(chroma_range_y,chroma_range) ,
       	                   TPC_STRIDE_ARG|TPC_IN_ARG, chroma_i_stride1,

       	             ref1,
       	                   TPC_BUILD_STRIDEARG(luma_range_y,range) ,
       	                   TPC_STRIDE_ARG|TPC_IN_ARG, i_stride,


       	             chroma_ref2,
       	                   TPC_BUILD_STRIDEARG(chroma_range_y,chroma_range) ,
       	                   TPC_STRIDE_ARG|TPC_IN_ARG, chroma_i_stride0,

       	             chroma_ref3,
       	                   TPC_BUILD_STRIDEARG(chroma_range_y,chroma_range) ,
       	                   TPC_STRIDE_ARG|TPC_IN_ARG, chroma_i_stride1,

       	             fenc_luma,
       	                   TPC_BUILD_STRIDEARG(16,16) ,
       	                   TPC_STRIDE_ARG|TPC_IN_ARG, i_stride2_luma,

       	             fenc_chroma1,
       	                   TPC_BUILD_STRIDEARG(8,16) ,
       	                   TPC_STRIDE_ARG|TPC_IN_ARG, i_stride2_chroma,

       	             fenc_chroma2,
       	                   TPC_BUILD_STRIDEARG(8,16) ,
       	                   TPC_STRIDE_ARG|TPC_IN_ARG, i_stride2_chroma



              );



   } else if ( h->sh.i_type == SLICE_TYPE_P ) {

    if ( mb->i_mb_y > ( i_height_mb - i_mb_threshold)  ){
	   luma_range_y = range - (16)*( -2 + i_mb_threshold - (i_height_mb - mb->i_mb_y)); 
	   chroma_range_y = chroma_range - (8)*( -1 + i_mb_threshold - (i_height_mb - mb->i_mb_y)); 
    }

	h->tpc_id_table[mb_i_xy]=  tpc_call(1, 12,
       	             fdec_luma,
       	                   TPC_BUILD_STRIDEARG(16,16) ,
       	                   TPC_STRIDE_ARG|TPC_OUT_ARG, i_stride2_luma,

       	             fdec_chroma1,
       	                   TPC_BUILD_STRIDEARG(8,16) ,
       	                   TPC_STRIDE_ARG|TPC_OUT_ARG, i_stride2_chroma,
       	             fdec_chroma2,
       	                   TPC_BUILD_STRIDEARG(8,16) ,
       	                   TPC_STRIDE_ARG|TPC_OUT_ARG, i_stride2_chroma,
                    mb,   sizeof(*mb),   TPC_INOUT_ARG,
       	            dct,  sizeof(*dct),  TPC_INOUT_ARG,
                    data, sizeof(*data), TPC_IN_ARG,
           
                    ref0,
                          TPC_BUILD_STRIDEARG(luma_range_y,range) ,
                          TPC_STRIDE_ARG|TPC_IN_ARG, i_stride,

       	             chroma_ref0,
       	                   TPC_BUILD_STRIDEARG(chroma_range_y,chroma_range) ,
       	                   TPC_STRIDE_ARG|TPC_IN_ARG, chroma_i_stride0,

       	             chroma_ref1,
       	                   TPC_BUILD_STRIDEARG(chroma_range_y,chroma_range) ,
       	                   TPC_STRIDE_ARG|TPC_IN_ARG, chroma_i_stride1,

       	             fenc_luma,
       	                   TPC_BUILD_STRIDEARG(16,16) ,
       	                   TPC_STRIDE_ARG|TPC_IN_ARG, i_stride2_luma,

       	             fenc_chroma1,
       	                   TPC_BUILD_STRIDEARG(8,16) ,
       	                   TPC_STRIDE_ARG|TPC_IN_ARG, i_stride2_chroma,

       	             fenc_chroma2,
       	                   TPC_BUILD_STRIDEARG(8,16) ,
       	                   TPC_STRIDE_ARG|TPC_IN_ARG, i_stride2_chroma
           );
   }

#ifdef STAT
     stats[h->sh.i_type][CALL] += ( __mftb() - tmp_time1 );
#endif
}
#endif


/* From this func we init the (de)quantization tables */
void  quant_spu_init( x264_t *h)
{
	int i;

	int i_qp;
	if (  h->sh.i_type == SLICE_TYPE_P )
		i_qp =  h->sh.i_qp;
	else
		i_qp =  h->sh.i_qp+2 ;
        const int max_spes = h->spes;


#if 1
        /* Tramsfer dequant4_mf  */ 
	for ( i=0; i<max_spes; i++){

		tpc_call( 2, 6,
          		&(h->dequant4_mf[0][0][0][0]),   64,   TPC_IN_ARG,
                     	&(h->dequant4_mf[1][1][0][0]),   64,   TPC_IN_ARG,
                     	&(h->dequant4_mf[2][2][0][0]),   64,   TPC_IN_ARG,
                     	&(h->dequant4_mf[3][3][0][0]),   64,   TPC_IN_ARG,
                     	&(h->dequant4_mf[4][4][0][0]),   64,   TPC_IN_ARG,
                     	&(h->dequant4_mf[5][5][0][0]),   64,   TPC_IN_ARG
        	);


	}


#if 0

        /* Tramsfer dequant8_mf  */ 
	for ( i=0; i<max_spes; i++){

		tpc_call( 3, 6,
          		&(h->dequant8_mf[0][0][0][0]),  256,   TPC_IN_ARG,
                    	&(h->dequant8_mf[0][1][0][0]),  256,   TPC_IN_ARG,
                    	&(h->dequant8_mf[0][2][0][0]),  256,   TPC_IN_ARG,
                    	&(h->dequant8_mf[0][3][0][0]),  256,   TPC_IN_ARG,
                    	&(h->dequant8_mf[0][4][0][0]),  256,   TPC_IN_ARG,
                    	&(h->dequant8_mf[0][5][0][0]),  256,   TPC_IN_ARG
        	);


	}
#endif
        /* Tramsfer  unquant4_mf  */ 
	for ( i=0; i<max_spes; i++){

		tpc_call( 4, 4,
          		&(h->unquant4_mf[0][i_qp][0]),  64,   TPC_IN_ARG,
                     	&(h->unquant4_mf[1][i_qp][0]),  64,   TPC_IN_ARG,
                     	&(h->unquant4_mf[2][i_qp][0]),  64,   TPC_IN_ARG,
                     	&(h->unquant4_mf[3][i_qp][0]),  64,   TPC_IN_ARG
        	);
	}

#if 0
        /* Tramsfer  unquant8_mf  */ 
	for ( i=0; i<max_spes; i++){

		tpc_call( 5, 2,
          		&(h->unquant8_mf[0][i_qp][0]),  256,   TPC_IN_ARG,
                     	&(h->unquant8_mf[1][i_qp][0]),  256,   TPC_IN_ARG
        	);
	}
#endif



        /* Tramsfer  quant4_mf  */ 
	for ( i=0; i<max_spes; i++){

		tpc_call( 6, 4,
          		&(h->quant4_mf[0][i_qp][0]), 32,   TPC_IN_ARG,
                     	&(h->quant4_mf[1][i_qp][0]), 32,   TPC_IN_ARG,
                     	&(h->quant4_mf[2][i_qp][0]), 32,   TPC_IN_ARG,
                     	&(h->quant4_mf[3][i_qp][0]), 32,   TPC_IN_ARG
        	);
	}

        /* Tramsfer  quant8_mf  */ 
	for ( i=0; i<max_spes; i++){

		tpc_call( 7, 2,
          		&(h->quant8_mf[0][i_qp][0]),  128,   TPC_IN_ARG,
                     	&(h->quant8_mf[1][i_qp][0]),  128,   TPC_IN_ARG
        	);
	}


        /* Tramsfer  quant4_bias  */ 
	for ( i=0; i<max_spes; i++){

		tpc_call( 8, 4,
          		&(h->quant4_bias[0][i_qp][0]), 32,   TPC_IN_ARG,
                     	&(h->quant4_bias[1][i_qp][0]), 32,   TPC_IN_ARG,
                     	&(h->quant4_bias[2][i_qp][0]), 32,   TPC_IN_ARG,
                     	&(h->quant4_bias[3][i_qp][0]), 32,   TPC_IN_ARG
        	);
	}

        /* Tramsfer  quant8_bias:  */ 
	for ( i=0; i<max_spes; i++){

		tpc_call( 9, 2,
          		&(h->quant8_bias[0][i_qp][0]),  128,   TPC_IN_ARG,
                     	&(h->quant8_bias[1][i_qp][0]),  128,   TPC_IN_ARG
        	);
	}





#endif





}

void x264_macroblock_call_write_cavlc_tpc( x264_t *h, bs_t *s, x264_mb_t *mb, x264_dct_t *dct ){


#ifdef STAT
    uint64_t   tmp_time1 = __mftb();
#endif

    const int mb_xy = mb->i_mb_xy;
    x264_data_t *data = h->x264_data_table[mb_xy];
    uint8_t *bit_s;


    /* Entropy encoding */

    // top_xy
    const int i_top_y = mb->i_mb_y - (1 << mb->b_interlaced);
    const int i_top_xy = i_top_y * mb->i_mb_stride + mb->i_mb_x;
    const int i_left_xy = mb->i_mb_y * mb->i_mb_stride + mb->i_mb_x -1 ;
    uint8_t* non_zero_count_in_top = mb->non_zero_count[mb_xy];
    uint8_t* non_zero_count_in_left = mb->non_zero_count[mb_xy];

    /* Top non-zero count */	
    if ( i_top_y >= h->sh.i_first_mb){
	    mb->i_mb_type_top =  mb->type[i_top_xy];
	    non_zero_count_in_top = mb->non_zero_count[i_top_xy];
    }
    /* Left non-zero count */

    if( mb->i_mb_x > 0 && mb->i_mb_xy > h->sh.i_first_mb ){
	    mb->i_mb_type_left = mb->type[i_left_xy];
	    non_zero_count_in_left = mb->non_zero_count[i_left_xy];
    }



    /* calculate the offset to shift in SPE */ 
    data->offset =  ( (( unsigned long) s->p )%16)  ;
    ISSUE_TASK() ;

    /* Now mask the pointer  */
    bit_s =  (uint8_t *) (( (unsigned long) s->p ) & 0xfffffff0 ) ;

    h->tpc_id_table[mb->i_mb_xy]    = tpc_call(10, 8,
	    mb->non_zero_count[mb->i_mb_xy],  32, TPC_OUT_ARG,
            bit_s,  256,       TPC_INOUT_ARG,
            s,      sizeof(*s),    TPC_INOUT_ARG,
            mb,     sizeof(*mb),   TPC_IN_ARG,
            data,   sizeof(*data), TPC_IN_ARG,
            dct,    sizeof(*dct),  TPC_IN_ARG,
	    non_zero_count_in_top, 32, TPC_IN_ARG,
	    non_zero_count_in_left, 32, TPC_IN_ARG


            );

#ifdef STAT
    stats[h->sh.i_type][CALL] += ( __mftb() - tmp_time1 );
#endif

}


/**
 * Calls expand border: FIXME: more fine grain partitioning is needed - no local store space
 */


void x264_macroblock_call_plane_expand_border( x264_t *h, uint8_t *pix, int stride, int width, int height, int padh, int padv, int b_start, int b_end, int i_mb_y ){
  
    /* FIXME*/ 
    assert(0); 
    x264_data_t *data = h->x264_data_table[0];

    data-> b_dct_decimate    =  h->param.analyse.b_dct_decimate;
    data-> i_first_mb        =  h->sh.i_first_mb;

    data-> b_bidir_me        =  h->param.analyse.b_bidir_me;

    data->i_type             =  h->sh.i_type;
    data-> i_me_range        =  h->param.analyse.i_me_range;
    data-> i_me_method       =  h->param.analyse.i_me_method;
    data-> i_subpel_refine   =  h->param.analyse.i_subpel_refine;
    data-> i_mv_range        =  h->param.analyse.i_mv_range;
    data-> b_chroma_me       =  h->param.analyse.b_chroma_me;
    data-> b_mbaff           =  h->sh.b_mbaff;

    data-> i_mb_width        =  h->sps->i_mb_width;
    data-> i_mb_height       =  h->sps->i_mb_height;
    

    data->i_profile_idc      =  h->sps->i_profile_idc;



    /* these offsets are for deblocking filter */
    data->i_alpha_c0_offset =  h->sh.i_alpha_c0_offset;
    data->i_beta_offset =  h->sh.i_beta_offset;
    data->i_chroma_qp_offset =  h->param.analyse.i_chroma_qp_offset;

    data->i_stride[0] = h->fdec->i_stride[0];
    data->i_stride[1] = h->fdec->i_stride[1];

    data->b_cabac = h->pps->b_cabac;
    data->b_transform_8x8_mode = h->pps->b_transform_8x8_mode;

    /*expand border*/
    data->i_width[0] = width;
    data->i_height = height;
    data->i_padh = padh;
    data->i_padv = padv;
    data->b_pad_top = b_start;
    data->b_pad_bottom = b_end;
    data->i_stride[0] = stride;
  
    //offset
    pix = pix - (padh) - padv * stride;
   
    data->offset = (padh) + padv * stride ;
    int pix_size =  (height)*stride + padh  ;

    //fprintf(stderr,"PPU height:%d padv:%d stride:%d size:%d\n",  height, padv, stride, pix_size );

    /* ID 11 is for deblocking */
    tpc_call(11, 2,

	             pix, pix_size, TPC_INOUT_ARG, 
       	             data, sizeof(*data), TPC_IN_ARG
         
              );


}

/**
 * Calls hpel filter to SPU
 */
void x264_macroblock_call_hpel_filter( x264_t *h,  uint8_t *dsth, uint8_t *dstv, uint8_t *dstc, uint8_t *src,
                         int stride, int width, int height, int i_mb_y  ){
  
 
    x264_filter_data_t *data = h->x264_filter_data_table_luma[i_mb_y];


 
    /* these offsets are for deblocking filter */
    data->i_alpha_c0_offset =  h->sh.i_alpha_c0_offset;
    data->i_beta_offset =  h->sh.i_beta_offset;
    data->i_chroma_qp_offset =  h->param.analyse.i_chroma_qp_offset;

    data->i_stride[0] = h->fdec->i_stride[0];
    data->i_stride[1] = h->fdec->i_stride[1];



  
    //offset - TODO: FIXME: mask is donw in 32bit values - if 64bit not working
    data->pref_offset[0] = ( (uint32_t) dsth  & 0xf )  + 2;

    dsth = (uint8_t *) (((uint32_t) dsth-2) & 0xfffffff0) ;
    dstv = (uint8_t *) (((uint32_t) dstv-2) & 0xfffffff0) ;
    dstc = (uint8_t *) (((uint32_t) dstc-2) & 0xfffffff0) ;
    src  = (uint8_t *) (((uint32_t) src-2*stride) & 0xfffffff0) ;



    const int pix_size = height*stride/2   ;


    data->i_width[0]  = width;
    data->i_height    = height;
    data->i_stride[0] = stride;

    /* ID 11 is for deblocking */
    tpc_call(12, 5,
		dsth, pix_size, TPC_INOUT_ARG, 
		dstv, pix_size, TPC_INOUT_ARG, 
		dstc, pix_size, TPC_INOUT_ARG, 
		src , pix_size, TPC_IN_ARG, 
		data, sizeof(*data), TPC_IN_ARG
         
              );

}





/**
 * Calls hpel filter to SPU
 */
void x264_macroblock_call_deblock_luma( x264_t *h, uint8_t *pix, int mb_y ){
 

 
    x264_filter_data_t *data = h->x264_filter_data_table_luma[mb_y];



    data->i_stride[0] = h->fdec->i_stride[0];


    pix -= 3*data->i_stride[0]; 


    data->i_mb_width = h->sps->i_mb_width;
    data->i_chroma_qp_offset =  h->param.analyse.i_chroma_qp_offset;
    data->i_beta_offset = h->sh.i_beta_offset;
    data->i_alpha_c0_offset = h->sh.i_alpha_c0_offset ;

    data->i_mb_width = h->sps->i_mb_width;
    data->mb_y = mb_y;
    data->i_qp = h->mb_table[0]->i_qp;

    /* ID 14 is for Luma deblocking */
    tpc_call(14, 2,
                    pix, ((3+16+4)*data->i_stride[0]), TPC_INOUT_ARG,
 		    data, sizeof(*data), TPC_IN_ARG
         
              );

}




/**
 * Calls hpel filter to SPU
 */
void x264_macroblock_call_deblock_chroma( x264_t *h, uint8_t *pixu, uint8_t *pixv, int mb_y ){


    x264_filter_data_t *data = h->x264_filter_data_table_chroma[mb_y];



    data->i_stride[1] = h->fdec->i_stride[1];



    pixu  -= 3*h->fdec->i_stride[1] ;
    pixv  -= 3*h->fdec->i_stride[1] ;



    data->i_beta_offset = h->sh.i_beta_offset;
    data->i_alpha_c0_offset = h->sh.i_alpha_c0_offset ;

    data->i_mb_width = h->sps->i_mb_width;
    data->mb_y = mb_y;
    data->i_qp = h->mb_table[0]->i_qp;

    /* ID 15 is for Chroma deblocking */
#if 1
    tpc_call(15, 3,
                    pixu, ((3+8)*(data->i_stride[1]) ), TPC_IN_ARG,
                    pixv, ((3+8)*(data->i_stride[2]) ), TPC_IN_ARG,
 		    data, sizeof(*data), TPC_IN_ARG
         
              );
#else

        int elems = 3+8;
        int elemsz = data->i_stride[1]/2;
        int stride = data->i_stride[1];
        data->i_stride[1] =  data->i_stride[1]/2;
        data->i_mb_width =  data->i_mb_width/2;
        data->i_mb_stride = data->i_mb_stride/2;
    
        tpc_call(15, 3,

       	             pixu,
       	                   TPC_BUILD_STRIDEARG(elems,elemsz) ,
       	                   TPC_STRIDE_ARG|TPC_INOUT_ARG, stride,

                    pixv,
       	                   TPC_BUILD_STRIDEARG(elems,elemsz) ,
       	                   TPC_STRIDE_ARG|TPC_INOUT_ARG, stride,


  		    data, sizeof(*data), TPC_IN_ARG
         
              );

#endif


}
















/**
 * Calls copy intra in SPU
 */
void x264_macroblock_call_copy_intra( x264_t *h, int j, int mb_y, x264_mb_t *mb  ){
#ifdef STAT
   uint64_t   tmp_time1 = __mftb();
#endif


    /* for plane fenc - index is 0 for luma  and 1,2 for chroma */
    const uint8_t * fdec_luma =  h->fdec->plane[0] + ((mb_y*16   ) + j - 1 - h->sh.b_mbaff) * h->fdec->i_stride[0]   ;
    const uint8_t * fdec_chroma0 =  h->fdec->plane[1] + ((mb_y*8 ) + j - 1 - h->sh.b_mbaff) * h->fdec->i_stride[1]  ;
    const uint8_t * fdec_chroma1 =  h->fdec->plane[2] + ((mb_y*8 ) + j - 1 - h->sh.b_mbaff) * h->fdec->i_stride[2]  ;
    ISSUE_TASK() ;

    // We know thath deblocking filter uses 6 pixels in total 
    const int pix_size_luma   = ALIGN( h->sps->i_mb_width*16 , 16 );
    const int pix_size_chroma = ALIGN( h->sps->i_mb_width*8 , 16 );

    /* ID 21 is for deblocking */
    tpc_call(21,6 ,
		    mb->intra_border_backup[j][0], pix_size_luma, TPC_OUT_ARG,
		    mb->intra_border_backup[j][1], pix_size_chroma, TPC_OUT_ARG,
		    mb->intra_border_backup[j][2], pix_size_chroma, TPC_OUT_ARG,
                    fdec_luma,    pix_size_luma  , TPC_IN_ARG,
                    fdec_chroma0, pix_size_chroma, TPC_IN_ARG,
                    fdec_chroma1, pix_size_chroma, TPC_IN_ARG
         
              );
#ifdef STAT
     stats[h->sh.i_type][CALL] += ( __mftb() - tmp_time1 );
#endif
}


/**
 * Calls copy plane in SPU
 */
void x264_macroblock_call_copy_plane( x264_t *h, uint8_t *dst,  uint8_t *src,  x264_copy_data_t *cp_data){
#ifdef STAT
   uint64_t   tmp_time1 = __mftb();
#endif


 ISSUE_TASK() ;
       /* ID 12  is for copy */
       tpc_call(12, 3,
		    dst, cp_data->i_dst_stride*(cp_data->height), TPC_OUT_ARG,
                    src, cp_data->i_src_stride*(cp_data->height), TPC_IN_ARG,
                    cp_data, sizeof(*cp_data), TPC_IN_ARG
         
              );
#ifdef STAT
     stats[h->sh.i_type][CALL] += ( __mftb() - tmp_time1 );
#endif

}

/**
 * Calls copy plane in SPU
 */
void x264_macroblock_call_copy_mb( x264_t *h, x264_mb_t  *dst, x264_mb_t *src){
 ISSUE_TASK() ;
#ifdef STAT
   uint64_t   tmp_time1 = __mftb();
#endif


	assert(0);
       /* ID 12  is for copy */
       tpc_call(22, 2,
		    dst, sizeof(*dst), TPC_OUT_ARG,
                    src, sizeof(*src), TPC_IN_ARG
         
              );

#ifdef STAT
     stats[h->sh.i_type][CALL] += ( __mftb() - tmp_time1 );
#endif
}



/**
 * Calls memset in SPU
 */
void x264_macroblock_call_memset( x264_t *h,  void  *dst, int size){
 ISSUE_TASK() ;
#ifdef STAT
   uint64_t   tmp_time1 = __mftb();
#endif

     /* ID 12  is for copy */
      tpc_call(23, 1,
		    dst, size, TPC_OUT_ARG
         
              );
#ifdef STAT
     stats[h->sh.i_type][CALL] += ( __mftb() - tmp_time1 );
#endif
}


/* Calls copy in SPU
 */
void x264_macroblock_call_copy( x264_t *h,  uint8_t *dst,  uint8_t *src,  unsigned int size  ){
#ifdef STAT
   uint64_t   tmp_time1 = __mftb();
#endif


       ISSUE_TASK() ;
       /* ID 12  is for copy */
       tpc_call(24, 2,
		    dst, size, TPC_INOUT_ARG,
                    src, size, TPC_IN_ARG
         
              );
#ifdef STAT
     stats[h->sh.i_type][CALL] += ( __mftb() - tmp_time1 );
#endif
}





#endif


#if 0

		int ii,jj;
		char *tmp11=  (char *) ref0  ;
		printf("\nPPU:\n");
		for (jj=0; jj<8; jj++ ){
			for ( ii=0 ;ii<16; ii++ )
				printf("%x",tmp11[ii]);
			 printf("\n");

		 tmp11=tmp11 + i_stride2_chroma ;

		}
		printf("\n");

#endif


