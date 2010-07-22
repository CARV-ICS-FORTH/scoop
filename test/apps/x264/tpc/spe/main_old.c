#include <stdio.h>
#include <spu_intrinsics.h>
#include <spu_mfcio.h>

#include "../../../libtpc/include/tpc_common.h"
#include "../../../libtpc/include/tpc_spe.h"

#include "common/spu/common_tpc.h"
#include "encoder/spu/me_tpc.h"
#include "encoder/spu/analyse_tpc.h"

#define ALIGN(x,a) (((x)+((a)-1))&~((a)-1))

/* This struct used for storing pointers before execution in SPEs */
typedef struct x264_old_pointers {

    DECLARE_ALIGNED_16( uint8_t *p_fenc[4]);

    /* pointer over mb of the frame to be reconstructed  */
    DECLARE_ALIGNED_16( uint8_t *p_fdec[4]);

    /* pointer over mb of the references */
    DECLARE_ALIGNED_16(  uint8_t *p_fref[16]); /* last: lN, lH, lV, lHV, cU, cV */
    DECLARE_ALIGNED_16(  uint16_t *p_integral[16]);

    /* stides */
    DECLARE_ALIGNED_16( int  i_stride[4] );

    /* Bitstream - used in trntropy encoding */
    uint8_t *bit_s;

} __attribute__((aligned(16))) x264_old_pointers_t;


/* quantization matrix for decoding, [cqm][qp%6][coef_y][coef_x] */
int             dequant4_mf[4][6][4][4]; /* [4][6][4][4] */
/* quantization matrix for trellis, [cqm][qp][coef] */
int             unquant4_mf[4][1][16];   /* [4][1][16] */

/* quantization matrix for deadzone */
uint16_t        quant4_mf[4][1][16];     /* [4][1][16] */
uint16_t        quant8_mf[2][1][64];     /* [2][1][64] */
uint16_t        quant4_bias[4][1][16];   /* [4][1][16] */
uint16_t        quant8_bias[2][1][64];   /* [2][1][64] */


/* ?? */
uint32_t        nr_count[2];


static inline void copy_column8( uint8_t *dst, uint8_t *src )
{
    dst[0*FDEC_STRIDE] = src[0*FDEC_STRIDE];
    dst[1*FDEC_STRIDE] = src[1*FDEC_STRIDE];
    dst[2*FDEC_STRIDE] = src[2*FDEC_STRIDE];
    dst[3*FDEC_STRIDE] = src[3*FDEC_STRIDE];
    dst[4*FDEC_STRIDE] = src[4*FDEC_STRIDE];
    dst[5*FDEC_STRIDE] = src[5*FDEC_STRIDE];
    dst[6*FDEC_STRIDE] = src[6*FDEC_STRIDE];
    dst[7*FDEC_STRIDE] = src[7*FDEC_STRIDE];

}

static inline void x264_mb_predict_mv_pskip_tpc(  int16_t mv[2], x264_mb_t *mb )
{
    int     i_refa = mb->cache.ref[0][X264_SCAN8_0 - 1];
    int     i_refb = mb->cache.ref[0][X264_SCAN8_0 - 8];
    int16_t *mv_a  = mb->cache.mv[0][X264_SCAN8_0 - 1];
    int16_t *mv_b  = mb->cache.mv[0][X264_SCAN8_0 - 8];

    if( i_refa == -2 || i_refb == -2 ||
        ( i_refa == 0 && *(uint32_t*)mv_a == 0 ) ||
        ( i_refb == 0 && *(uint32_t*)mv_b == 0 ) )
    {
        *(uint32_t*)mv = 0;
    }
    else
    {
        x264_mb_predict_mv_16x16_tpc(  0, 0, mv, mb );
    }
}

static inline cache_load_tpc(x264_data_t *data, x264_mb_t *mb){

 		/* Non zero count */
    int i_top_xy = mb->i_mb_x + (mb->i_mb_y-1)*mb->i_mb_stride;
    int i_left_xy = mb->i_mb_xy - 1;



    mb->i_neighbour4[0] =
        mb->i_neighbour8[0] = (mb->i_neighbour & (MB_TOP|MB_LEFT|MB_TOPLEFT))
        | ((mb->i_neighbour & MB_TOP) ? MB_TOPRIGHT : 0);
    mb->i_neighbour4[4] =
        mb->i_neighbour4[1] = MB_LEFT | ((mb->i_neighbour & MB_TOP) ? (MB_TOP|MB_TOPLEFT|MB_TOPRIGHT) : 0);
    mb->i_neighbour4[2] =
        mb->i_neighbour4[8] =
        mb->i_neighbour4[10] =
        mb->i_neighbour8[2] = MB_TOP|MB_TOPRIGHT | ((mb->i_neighbour & MB_LEFT) ? (MB_LEFT|MB_TOPLEFT) : 0);
    mb->i_neighbour4[5] =
        mb->i_neighbour8[1] = MB_LEFT | (mb->i_neighbour & MB_TOPRIGHT)
        | ((mb->i_neighbour & MB_TOP) ? MB_TOP|MB_TOPLEFT : 0);



	if (data->i_type== SLICE_TYPE_P )
            x264_mb_predict_mv_pskip_tpc( mb->cache.pskip_mv, mb );


   {
        const int s8x8 = mb->i_b8_stride;
        const int s4x4 = mb->i_b4_stride;

        int i_list;
        int i;
        for( i_list = 0; i_list < (data->i_type == SLICE_TYPE_B ? 2  : 1 ); i_list++ )
        {

            if( mb->i_neighbour & MB_TOPLEFT )
            {
/*                left_mb = h->mb_table[ i_mb_x -1 + max_row*(i_mb_y-1) ];
                const int i8 = x264_scan8[0] - 1 - 1*8;
                const int ir = i_top_8x8 - 1;
                const int iv = i_top_4x4 - 1;
                mb->cache.ref[i_list][i8]  = left_mb->ref[i_list][ir];
                *(uint32_t*)mb->cache.mv[i_list][i8] = *(uint32_t*)left_mb->mv[i_list][iv];
            */}
            else
            {
                const int i8 = x264_scan8[0] - 1 - 1*8;
                mb->cache.ref[i_list][i8] = -2;
                *(uint32_t*)mb->cache.mv[i_list][i8] = 0;
            }

            if( mb->i_neighbour & MB_TOP )
            {
	/*	left_mb = h->mb_table[ i_mb_x + max_row*(i_mb_y-1) ];
                const int i8 = x264_scan8[0] - 8;
                const int ir = i_top_8x8;
                const int iv = i_top_4x4;
                mb->cache.ref[i_list][i8+0] =
                mb->cache.ref[i_list][i8+1] = left_mb->ref[i_list][ir + 0];
                mb->cache.ref[i_list][i8+2] =
                mb->cache.ref[i_list][i8+3] = left_mb->ref[i_list][ir + 1];
                *(uint64_t*)mb->cache.mv[i_list][i8+0] = *(uint64_t*)left_mb->mv[i_list][iv+0];
                *(uint64_t*)mb->cache.mv[i_list][i8+2] = *(uint64_t*)left_mb->mv[i_list][iv+2];
          */  }
            else
            {
                const int i8 = x264_scan8[0] - 8;
                *(uint64_t*)mb->cache.mv[i_list][i8+0] = 0;
                *(uint64_t*)mb->cache.mv[i_list][i8+2] = 0;
                *(uint32_t*)&mb->cache.ref[i_list][i8] = (uint8_t)(-2) * 0x01010101U;
            }

            if( mb->i_neighbour & MB_TOPRIGHT )
            {
	/*	left_mb = h->mb_table[ i_mb_x + 1 + max_row*(i_mb_y-1) ];
                const int i8 = x264_scan8[0] + 4 - 1*8;
                const int ir = i_top_8x8 + 2;
                const int iv = i_top_4x4 + 4;
                mb->cache.ref[i_list][i8]  = left_mb->ref[i_list][ir];
                *(uint32_t*)mb->cache.mv[i_list][i8] = *(uint32_t*)left_mb->mv[i_list][iv];
          */  }
            else
            {
                const int i8 = x264_scan8[0] + 4 - 1*8;
                mb->cache.ref[i_list][i8] = -2;
                *(uint32_t*)mb->cache.mv[i_list][i8] = 0;
            }

            if( mb->i_neighbour & MB_LEFT )
            {
	/*	left_mb = h->mb_table[ i_mb_x -1 + max_row*(i_mb_y) ];
                const int i8 = x264_scan8[0] - 1;
                const int ir = i_mb_8x8 - 1;
                const int iv = i_mb_4x4 - 1 ;
                mb->cache.ref[i_list][i8+0*8] =
                mb->cache.ref[i_list][i8+1*8] = left_mb->ref[i_list][ir + 0*s8x8];
                mb->cache.ref[i_list][i8+2*8] =
                mb->cache.ref[i_list][i8+3*8] = left_mb->ref[i_list][ir + 1*s8x8];

                *(uint32_t*)mb->cache.mv[i_list][i8+0*8] = *(uint32_t*)left_mb->mv[i_list][iv + 0*s4x4];
                *(uint32_t*)mb->cache.mv[i_list][i8+1*8] = *(uint32_t*)left_mb->mv[i_list][iv + 1*s4x4];
                *(uint32_t*)mb->cache.mv[i_list][i8+2*8] = *(uint32_t*)left_mb->mv[i_list][iv + 2*s4x4];
                *(uint32_t*)mb->cache.mv[i_list][i8+3*8] = *(uint32_t*)left_mb->mv[i_list][iv + 3*s4x4];
          */  }
            else
            {
                const int i8 = x264_scan8[0] - 1;
                for( i = 0; i < 4; i++ )
                {
                    mb->cache.ref[i_list][i8+i*8] = -2;
                    *(uint32_t*)mb->cache.mv[i_list][i8+i*8] = 0;
                }
            }

          }


    }


}



//int execute_task(queue_entry_t *ex_task, tpc_spe_task_state_t *task_info)
int execute_task( queue_entry_t *ex_task, tpc_spe_task_state_t *task_info)
{
    int exit = 0;
    int elems, elem_sz;
    void *arg0, *arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8, *arg9;
    void *arg10, *arg11, *arg12, *arg13, *arg14, *arg15, *arg16, *arg17, *arg18, *arg19;

    x264_data_t *data;
    x264_copy_data_t *copy_data;
    x264_mb_t *mb, *mb0, *mb1, *mb2, *mb3;
    x264_dct_t *dct, *dct0, *dct1, *dct2, *dct3;
    x264_filter_data_t *fdata;

    uint8_t *ref0, *ref1, *ref2, *ref3,  *ref4, *ref5, *ref6, *ref7, *ref8;
    uint8_t *fdec0, *fdec1, *fdec2 ;
    uint8_t *pix0;
    int i_me_range;
    int range,i;
    x264_old_pointers_t old_data;
    uint64_t _val;
    int chroma_range; 
    int i_me_chroma_range; 
    x264_ret_data_t *ret_entropy_data;
    uint8_t *dsth, *dstv, *dstc, *src;
    uint8_t *bit_s; /* Bit stream */
    bs_t *bs;
    uint8_t *mb_non_zero_count;
    uint32_t tmp1 ;
    vector unsigned int size_cp  ;
    int i_top_xy;
    int i_left_xy;
    x264_fread_t *file_data;

    switch(ex_task->funcid)
    {
         case 0:       
                        break;


         case 1:

#if 0
     		spu_write_decrementer(0);
		tmp1 = spu_read_decrementer() ; 
#endif




		/* OUT ARGS */

	        arg19  =  (void *)((void *)  task_info->ls_addr );
        //        mb_non_zero_count = arg19;
	

	        /* these are  fdec */
	        /* Fdec frame luma */
	        fdec0 =  (uint8_t *)((void *)   arg19 + ex_task->arguments[0].size);
	        arg13  =  (void *)((void *)  arg19 + ex_task->arguments[0].size );
	        elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[1].size);
	        elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[1].size);
	
	        /* Fdec frame chroma */
	        fdec1 =  (uint8_t *)((void *)arg13 + elems*elem_sz );
                arg14  =  (void *)((void *)arg13 + elems*elem_sz );
	        elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[2].size);
	        elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[2].size);


	        /* Fdec frame chroma */
                fdec2 =  (uint8_t *)((void *)arg14 + elems*elem_sz )  ;
                arg15 =  (void *)((void *)arg14 + elems*elem_sz );
                elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[3].size);
                elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[3].size);




		/* IN/OUT ARGS */

		/* Macroblock */
	       	mb = (x264_mb_t *) ( (void *) arg15 + elems*elem_sz );
             	arg1 = ( void *) arg15 + elems*elem_sz ;

		/* DCT struct  */
	      	dct = (x264_dct_t *)((void *)arg1 + ex_task->arguments[4].size);
	      	arg2 = ((void *)arg1 + ex_task->arguments[4].size);

///////////////////////////// 


		/* Metadata */
		data = (x264_data_t *)(  (void *)arg2 + ex_task->arguments[5].size );
                arg3 = (void *) ((void *)arg2 + ex_task->arguments[5].size);

 

		/* Reference frame (1) */
  
                ref0 = (uint8_t *)((void *)arg3 + ex_task->arguments[6].size );
                arg4 = (void *) ((void *)arg3 + ex_task->arguments[6].size);
                elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[7].size);
                elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[7].size);


		/* Reference frame (2) */
                ref1 =  (uint8_t *)((void *)arg4 + elems*elem_sz );
                arg5 =  (void *)((void *)arg4 + elems*elem_sz );
                elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[8].size);
                elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[8].size);


		/* Reference frame (3) */
                ref2 =  (uint8_t *)((void *)arg5 + elems*elem_sz );
                arg6 =  (void *)((void *)arg5 + elems*elem_sz );
                elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[9].size);
                elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[9].size);

		/* Reference frame (4) */
                ref3 =  (uint8_t *)((void *)arg6 + elems*elem_sz );
                arg7 =  (void *)((void *)arg6 + elems*elem_sz );
                elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[10].size);
                elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[10].size);



		/* Reference frame (5) */
       	        ref4 =  (uint8_t *)((void *)arg7 + elems*elem_sz );
               	arg8 =  (void *)((void *)arg7 + elems*elem_sz );
                elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[11].size);
                elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[11].size);


		/* Reference frame (6) */
                ref5 =  (uint8_t *)((void *)arg8 + elems*elem_sz );
                arg9 =  (void *)((void *)arg8 + elems*elem_sz );
                elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[12].size);
                elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[12].size);



		/* If B-frame we heve  elements  */
	 	if (data->i_type == SLICE_TYPE_B) {

			/* In B-frames these are  fenc */
			/* Fdec frame luma */
	                ref6 =  (uint8_t *)((void *)arg9 + elems*elem_sz );
	                arg10 =  (void *)((void *)arg9 + elems*elem_sz );
	                elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[13].size);
	                elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[13].size);
	
			/* Fdec frame chroma */
	                ref7 =  (uint8_t *)((void *)arg10 + elems*elem_sz );
                	arg11 =  (void *)((void *)arg10 + elems*elem_sz );
	                elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[14].size);
	                elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[14].size);


			/* Fdec frame chroma */
        	        ref8 =  (uint8_t *)((void *)arg11 + elems*elem_sz )  ;
        	        arg12 =  (void *)((void *)arg11 + elems*elem_sz );
        	        elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[15].size);
        	        elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[15].size);

		}


		i_me_range = data->i_me_range;
		range = i_me_range*2 + 32 ;
        	chroma_range = range/2 +16 ;
        	i_me_chroma_range = i_me_range/2;

		/* Save old pointers **/
                old_data.p_fref[0] = mb->pic.p_fref[0][0][0];

		/* Save Chroma */
                old_data.p_fref[2] = mb->pic.p_fref[0][0][4];
                old_data.p_fref[3] = mb->pic.p_fref[0][0][5];


       		old_data.p_fenc[0] = mb->pic.p_fenc[0];
       		old_data.p_fenc[1] = mb->pic.p_fenc[1];
       		old_data.p_fenc[2] = mb->pic.p_fenc[2];
		old_data.p_fdec[0] = mb->pic.p_fdec[0];
		old_data.p_fdec[1] = mb->pic.p_fdec[1];
		old_data.p_fdec[2] = mb->pic.p_fdec[2];

                /* Save old strides */
                old_data.i_stride[0] = mb->pic.i_stride[0];
                old_data.i_stride[1] = mb->pic.i_stride[1];
                old_data.i_stride[2] = mb->pic.i_stride[2];

		/* Calc new pointers **/

		/* encoded */
       		mb->pic.p_fenc[0] = mb->pic.fenc_buf ;
		mb->pic.p_fenc[1] = mb->pic.fenc_buf + 16*FENC_STRIDE;
		mb->pic.p_fenc[2] = mb->pic.fenc_buf + 16*FENC_STRIDE + 8;
		mb->pic.p_fdec[0] = mb->pic.fdec_buf + 2*FDEC_STRIDE;
		mb->pic.p_fdec[1] = mb->pic.fdec_buf + 19*FDEC_STRIDE;
		mb->pic.p_fdec[2] = mb->pic.fdec_buf + 19*FDEC_STRIDE + 16;

                
		/* Calc new strides */
                mb->pic.i_stride[0] = range  ;
                mb->pic.i_stride[1] = chroma_range  ;
                mb->pic.i_stride[2] = chroma_range  ;



		/* Reference pic list 0 */
                mb->pic.p_fref[0][0][0] =  (uint8_t*) ref0 + ( i_me_range + range*i_me_range );

		/* we must recalc the non aligned address for chroma list 0 */
                mb->pic.p_fref[0][0][4] =  (uint8_t*) ref1 + ( i_me_chroma_range + chroma_range*i_me_chroma_range ) + data->offset  ;
                mb->pic.p_fref[0][0][5] =  (uint8_t*) ref2 + ( i_me_chroma_range + chroma_range*i_me_chroma_range ) + data->offset  ;


		/* If B-frame do some extra things */
		if (data->i_type == SLICE_TYPE_B) {
	
			/* Save old pointers **/	
                	old_data.p_fref[1] = mb->pic.p_fref[1][0][0];

			/* Save chroma */
	                old_data.p_fref[4] = mb->pic.p_fref[1][0][4];
	                old_data.p_fref[5] = mb->pic.p_fref[1][0][5];

			/* Reference pic list 1 */
	                mb->pic.p_fref[1][0][0] =  (uint8_t*) ref3 + ( i_me_range + range*i_me_range ) ;

			/* we must recalc the non aligned address for chroma list 1 */
	                mb->pic.p_fref[1][0][4] =  (uint8_t*) ref4 + ( i_me_chroma_range + chroma_range*i_me_chroma_range ) + data->offset  ;
	                mb->pic.p_fref[1][0][5] =  (uint8_t*) ref5 + ( i_me_chroma_range + chroma_range*i_me_chroma_range ) + data->offset  ;

			/* Chroma is not always aligned so we have some padding */
			ref7 += data->offset2 ;
			ref8 += data->offset2 ;
			mc_copy_w16(  mb->pic.p_fenc[0], FENC_STRIDE,  ref6, 16, 16); 
			mc_copy_w8 (  mb->pic.p_fenc[1], FENC_STRIDE,  ref7, 16, 8); 
			mc_copy_w8 (  mb->pic.p_fenc[2], FENC_STRIDE,  ref8, 16, 8); 
		}else{  /* If P-frame */

			/* Chroma is not always aligned so we have some padding */
			ref4 += data->offset2 ;
			ref5 += data->offset2 ;

			mc_copy_w16(  mb->pic.p_fenc[0], FENC_STRIDE,  ref3, 16, 16); 
			mc_copy_w8 (  mb->pic.p_fenc[1], FENC_STRIDE,  ref4, 16, 8); 
			mc_copy_w8 (  mb->pic.p_fenc[2], FENC_STRIDE,  ref5, 16, 8); 
		} 
 
 #if 0

		int ii,jj;
		char *tmp11=  (char *) arg15 ;
		printf("\nSPU:\n");
		for (jj=0; jj<8; jj++ ){
			for ( ii=0 ;ii<16; ii++ )
				printf("%x",tmp11[ii]);
			 printf("\n");

		 tmp11=tmp11 + 16 ;

		}
		printf("\n");
		SYNC();

#endif




		mb->i_chroma_qp=data->i_chroma_qp;
		mb->i_qp= data->i_qp;

	   	if( !mb->b_interlaced )
		{
			copy_column8( mb->pic.p_fdec[0]-1, mb->pic.p_fdec[0]+15 );
			copy_column8( mb->pic.p_fdec[0]-1+8*FDEC_STRIDE, mb->pic.p_fdec[0]+15+8*FDEC_STRIDE );
			copy_column8( mb->pic.p_fdec[1]-1, mb->pic.p_fdec[1]+7 );
			copy_column8( mb->pic.p_fdec[2]-1, mb->pic.p_fdec[2]+7 );
		}

		cache_load_tpc( data, mb);


		mb->i_chroma_qp=data->i_chroma_qp;
		mb->i_qp= data->i_qp;

	   	if( !mb->b_interlaced )
		{
			copy_column8( mb->pic.p_fdec[0]-1, mb->pic.p_fdec[0]+15 );
			copy_column8( mb->pic.p_fdec[0]-1+8*FDEC_STRIDE, mb->pic.p_fdec[0]+15+8*FDEC_STRIDE );
			copy_column8( mb->pic.p_fdec[1]-1, mb->pic.p_fdec[1]+7 );
			copy_column8( mb->pic.p_fdec[2]-1, mb->pic.p_fdec[2]+7 );
		}



#ifdef STAT
                /* Init stats */
		mb->i_analyse = spu_read_decrementer() ; 
#endif

		/* TASK: Analyse current frame */
                x264_macroblock_analyse_tpc( data, mb, mb->i_mb_xy, dct);

#ifdef STAT
		/* Write stats for analyse */
      		mb->i_analyse -= spu_read_decrementer();
                /* Start counting for encoding */
		mb->i_encode = spu_read_decrementer() ; 
#endif
		x264_macroblock_encode_tpc( data, mb, dct );

#ifdef STAT
		/* Write stats for encoding */
      		mb->i_encode -= spu_read_decrementer() ;
#endif

		/* Copy fdec*/
		fdec1 += data->offset3 ;
		fdec2 += data->offset3 ;
		mc_copy_w16(  fdec0, 16, mb->pic.p_fdec[0], FDEC_STRIDE, 16); 
		mc_copy_w8 (  fdec1, 16, mb->pic.p_fdec[1], FDEC_STRIDE,  8); 
		mc_copy_w8 (  fdec2, 16, mb->pic.p_fdec[2], FDEC_STRIDE,  8); 



  	        /** Restore old pointers **/
                mb->pic.p_fref[0][0][0] =  old_data.p_fref[0]; 

		/** Chroma */
                mb->pic.p_fref[0][0][4] =  old_data.p_fref[2]; 
                mb->pic.p_fref[0][0][5] =  old_data.p_fref[3]; 


                /* Restore old strides */
                mb->pic.i_stride[0] =  old_data.i_stride[0];
                mb->pic.i_stride[1] =  old_data.i_stride[1];
                mb->pic.i_stride[2] =  old_data.i_stride[2];

	       	mb->pic.p_fenc[0] = old_data.p_fenc[0];
	       	mb->pic.p_fenc[1] = old_data.p_fenc[1];
	       	mb->pic.p_fenc[2] = old_data.p_fenc[2];
		mb->pic.p_fdec[0] = old_data.p_fdec[0];
		mb->pic.p_fdec[1] = old_data.p_fdec[1];
		mb->pic.p_fdec[2] = old_data.p_fdec[2];

		/* If B-frame restore list 1  */
		if (data->i_type == SLICE_TYPE_B) {

			/* Luma list 1 */
                	mb->pic.p_fref[1][0][0] =  old_data.p_fref[1]; 
			/* Chroma list 1 */
        	        mb->pic.p_fref[1][0][4] =  old_data.p_fref[4]; 
	                mb->pic.p_fref[1][0][5] =  old_data.p_fref[5]; 

		}




                if( mb->i_type != I_16x16 && mb->i_cbp_luma == 0 && mb->i_cbp_chroma == 0 )
                    mb->i_qp = mb->i_last_qp;
                mb->i_last_dqp = mb->i_qp - mb->i_last_qp;
                mb->i_last_qp = mb->i_qp;




#if 0
		if (data->i_type == SLICE_TYPE_B) 
		printf("%lu \n", tmp1 - spu_read_decrementer() );
#endif
                break;

        /*
	 *  We transfer (de)quantization matrices
	 * */

         case 2:
		/* Copy dequant4_mf - 4*4*sizeof(int) = 64 bytes, fixed value */
             	arg1 = ( void *) task_info->ls_addr;
                memcpy( &dequant4_mf[0][0][0][0], arg1, 64 );
                memcpy( &dequant4_mf[1][0][0][0], arg1, 64 );
                memcpy( &dequant4_mf[2][0][0][0], arg1, 64 );
                memcpy( &dequant4_mf[3][0][0][0], arg1, 64 );


		/* The same with +1 index */
	      	arg2 = ((void *)arg1 + ex_task->arguments[0].size);
		memcpy( &dequant4_mf[0][1][0][0], arg2, 64 );
		memcpy( &dequant4_mf[1][1][0][0], arg2, 64 );
		memcpy( &dequant4_mf[2][1][0][0], arg2, 64 );
		memcpy( &dequant4_mf[3][1][0][0], arg2, 64 );


		/* The same with +1 index */
	      	arg3 = ((void *)arg2 + ex_task->arguments[1].size);
		memcpy( &dequant4_mf[0][2][0][0], arg3, 64 );
		memcpy( &dequant4_mf[1][2][0][0], arg3, 64 );
		memcpy( &dequant4_mf[2][2][0][0], arg3, 64 );
		memcpy( &dequant4_mf[3][2][0][0], arg3, 64 );


		/* The same with +1 index */
	      	arg4 = ((void *)arg3 + ex_task->arguments[2].size);
		memcpy( &dequant4_mf[0][3][0][0], arg4,  64 );
		memcpy( &dequant4_mf[1][3][0][0], arg4,  64 );
		memcpy( &dequant4_mf[2][3][0][0], arg4,  64 );
		memcpy( &dequant4_mf[3][3][0][0], arg4,  64 );



		/* The same with +1 index */
	      	arg5 = ((void *)arg4 + ex_task->arguments[3].size);
		memcpy( &dequant4_mf[0][4][0][0], arg5, 64 );
		memcpy( &dequant4_mf[1][4][0][0], arg5, 64 );
		memcpy( &dequant4_mf[2][4][0][0], arg5, 64 );
		memcpy( &dequant4_mf[3][4][0][0], arg5, 64 );


		/* The same with +1 index */
	      	arg6 = ((void *)arg5 + ex_task->arguments[4].size);
		memcpy( &dequant4_mf[0][5][0][0], arg6, 64 );
		memcpy( &dequant4_mf[1][5][0][0], arg6, 64 );
		memcpy( &dequant4_mf[2][5][0][0], arg6, 64 );
		memcpy( &dequant4_mf[3][5][0][0], arg6, 64 );

		break;

         case 4:
		/* Copy unquant4_mf - 64 bytes, fixed value */
             	arg1 = ( void *) task_info->ls_addr;
                memcpy( &unquant4_mf[0][0][0], arg1, 64 );

		
	      	arg2 = ((void *)arg1 + ex_task->arguments[0].size);
                memcpy( &unquant4_mf[1][0][0], arg2, 64 );

		
	      	arg3 = ((void *)arg2 + ex_task->arguments[1].size);
                memcpy( &unquant4_mf[2][0][0], arg3, 64 );

		
	      	arg4 = ((void *)arg3 + ex_task->arguments[2].size);
                memcpy( &unquant4_mf[3][0][0], arg4, 64 );

		break;

#if 0
         case 3:
		/* Copy dequant4_mf - 8*8*sizeof(int) = 1536 bytes, fixed value */
             	arg1 = ( void *) task_info->ls_addr;
                memcpy( &dequant8_mf[0][0][0][0], arg1, 256 );
                memcpy( &dequant8_mf[1][0][0][0], arg1, 256 );

		/* The same with +1 index */
	      	arg2 = ((void *)arg1 + ex_task->arguments[0].size);
		memcpy( &dequant8_mf[0][1][0][0], arg2, 256 );
		memcpy( &dequant8_mf[1][1][0][0], arg2, 256 );

		/* The same with +1 index */
	      	arg3 = ((void *)arg2 + ex_task->arguments[1].size);
		memcpy( &dequant8_mf[0][2][0][0], arg3, 256 );
		memcpy( &dequant8_mf[1][2][0][0], arg3, 256 );

		/* The same with +1 index */
	      	arg4 = ((void *)arg3 + ex_task->arguments[2].size);
		memcpy( &dequant8_mf[0][3][0][0], arg4, 256 );
		memcpy( &dequant8_mf[1][3][0][0], arg4, 256 );

		/* The same with +1 index */
	      	arg5 = ((void *)arg4 + ex_task->arguments[3].size);
		memcpy( &dequant8_mf[0][4][0][0], arg5, 256 );
		memcpy( &dequant8_mf[1][4][0][0], arg5, 256 );

		/* The same with +1 index */
	      	arg6 = ((void *)arg5 + ex_task->arguments[4].size);
		memcpy( &dequant8_mf[0][5][0][0], arg6, 256 );
		memcpy( &dequant8_mf[1][5][0][0], arg6, 256 );
		break;




         case 5:
		/* Copy unquant8_mf - 256 bytes, fixed value */
             	arg1 = ( void *) task_info->ls_addr;
                memcpy( &unquant8_mf[0][0][0], arg1, 256 );

		
	      	arg2 = ((void *)arg1 + ex_task->arguments[0].size);
                memcpy( &unquant8_mf[1][0][0], arg2, 256 );
		break;
#endif

         case 6:
		/* Copy quant4_mf - 64 bytes, fixed value */
             	arg1 = ( void *) task_info->ls_addr;
                memcpy( &quant4_mf[0][0][0], arg1, 32 );

		
	      	arg2 = ((void *)arg1 + ex_task->arguments[0].size);
                memcpy( &quant4_mf[1][0][0], arg2, 32 );

	      	arg3 = ((void *)arg2 + ex_task->arguments[1].size);
                memcpy( &quant4_mf[2][0][0], arg3, 32 );

	      	arg4 = ((void *)arg3 + ex_task->arguments[2].size);
                memcpy( &quant4_mf[3][0][0], arg4, 32 );
		break;

         case 7:
		/* Copy quant8_mf - 256 bytes, fixed value */
             	arg1 = ( void *) task_info->ls_addr;
                memcpy( &quant8_mf[0][0][0], arg1, 128 );

		
	      	arg2 = ((void *)arg1 + ex_task->arguments[0].size);
                memcpy( &quant8_mf[1][0][0], arg2, 128 );
		break;

         case 8:
		/* Copy quant4_bias - 32 bytes, fixed value */
             	arg1 = ( void *) task_info->ls_addr;
                memcpy( &quant4_bias[0][0][0], arg1, 32 );

		
	      	arg2 = ((void *)arg1 + ex_task->arguments[0].size);
                memcpy( &quant4_bias[1][0][0], arg2, 32 );

	      	arg3 = ((void *)arg2 + ex_task->arguments[1].size);
                memcpy( &quant4_bias[2][0][0], arg3, 32 );

	      	arg4 = ((void *)arg3 + ex_task->arguments[2].size);
                memcpy( &quant4_bias[3][0][0], arg4, 32 );
		break;

         case 9:
		/* Copy quant4_bias - 256 bytes, fixed value */
             	arg1 = ( void *) task_info->ls_addr;
                memcpy( &quant8_bias[0][0][0], arg1, 128 );

		
	      	arg2 = ((void *)arg1 + ex_task->arguments[0].size);
                memcpy( &quant8_bias[1][0][0], arg2, 128 );

		break;

#ifdef ENTROPY
		/* This case is the entropy encoding */
         case 10:
#if 0
     		spu_write_decrementer(0);
		tmp1 = spu_read_decrementer() ; 
#endif
		/* cache non zero count */
		arg0 = ( void *) task_info->ls_addr;

		/* Bitstream for retun data */
             	arg1 = (((void *)arg0) + ex_task->arguments[0].size);
		bit_s =  (uint8_t *) arg1;


		/* filter data  */
		bs = (bs_t *) ((void *)arg1 + ex_task->arguments[1].size);
	      	arg2 = ((void *)arg1 + ex_task->arguments[1].size);


		/* MB struct  */
	      	mb = (x264_mb_t *)(((void *)arg2) + ex_task->arguments[2].size);
	      	arg3 = ((void *)arg2 + ex_task->arguments[2].size);

		/*  data  */
		data = (x264_data_t *) (((void *)arg3) + ex_task->arguments[3].size);
	      	arg4 = ((void *)arg3 + ex_task->arguments[3].size);



		/* dct  */
	      	dct = (x264_dct_t *)((void *)arg4 + ex_task->arguments[4].size);
	      	arg5 = ((void *)arg4 + ex_task->arguments[4].size);
                /* non zero top */
	      	arg6 = ((void *)arg5 + ex_task->arguments[5].size);
                /* non_Zero left */
	      	arg7 = ((void *)arg6 + ex_task->arguments[6].size);

		

		/* Save pointers */
		old_data.bit_s = bs->p;

		/* set the new pointer to the buffer */
                bit_s = bit_s +  data->offset  ;
                bs->p = bit_s  ;

		i_top_xy = mb->i_mb_x + (mb->i_mb_y-1)*mb->i_mb_stride;


		/* Non zero count in */

		if ( i_top_xy < 0){
			mb->i_mb_type_top = -1;

			/* load non_zero_count */

			mb->cache.non_zero_count[x264_scan8[0] - 8] =
				mb->cache.non_zero_count[x264_scan8[1] - 8] =
				mb->cache.non_zero_count[x264_scan8[4] - 8] =
				mb->cache.non_zero_count[x264_scan8[5] - 8] =
				mb->cache.non_zero_count[x264_scan8[16+0] - 8] =
				mb->cache.non_zero_count[x264_scan8[16+1] - 8] =
				mb->cache.non_zero_count[x264_scan8[16+4+0] - 8] =
				mb->cache.non_zero_count[x264_scan8[16+4+1] - 8] = 0x80; 
		}else{
			/* load non_zero_count */
			*(uint32_t*)&mb->cache.non_zero_count[x264_scan8[0] - 8] = *(uint32_t*) (arg6 + 12);
			/* shift because x264_scan8[16] is misaligned */
			*(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16] - 9] = *(uint16_t*)  (arg6 + 18) << 8;
			*(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16+4] - 9] = *(uint16_t*) (arg6 +22) << 8;


		}
		i_left_xy = mb->i_mb_xy - 1;
		if( mb->i_mb_x > 0 && mb->i_mb_xy >0  )
		{
			/* load non_zero_count */
			mb->cache.non_zero_count[x264_scan8[0 ] - 1] =  *(uint8_t*)(arg7 + 3);
			mb->cache.non_zero_count[x264_scan8[2 ] - 1] =  *(uint8_t*)(arg7 + 7);
			mb->cache.non_zero_count[x264_scan8[8 ] - 1] =  *(uint8_t*)(arg7 + 11);
			mb->cache.non_zero_count[x264_scan8[10] - 1] =  *(uint8_t*)(arg7 + 15);
			mb->cache.non_zero_count[x264_scan8[16+0] - 1] =  *(uint8_t*)(arg7 + 16+1);
			mb->cache.non_zero_count[x264_scan8[16+2] - 1] =  *(uint8_t*)(arg7 + 16+3);

			mb->cache.non_zero_count[x264_scan8[16+4+0] - 1] = *(uint8_t*)(arg7 + 16+4+1);
			mb->cache.non_zero_count[x264_scan8[16+4+2] - 1] = *(uint8_t*) (arg7 + 16+4+3);
		}
		else
		{
			mb->i_mb_type_left = -1;

			/* load non_zero_count */
			mb->cache.non_zero_count[x264_scan8[0 ] - 1] =
				mb->cache.non_zero_count[x264_scan8[2 ] - 1] =
				mb->cache.non_zero_count[x264_scan8[8 ] - 1] =
				mb->cache.non_zero_count[x264_scan8[10] - 1] =
				mb->cache.non_zero_count[x264_scan8[16+0] - 1] =
				mb->cache.non_zero_count[x264_scan8[16+2] - 1] =
				mb->cache.non_zero_count[x264_scan8[16+4+0] - 1] =
				mb->cache.non_zero_count[x264_scan8[16+4+2] - 1] = 0x80;
		}

                /* call write cavlc */
                x264_macroblock_write_cavlc_tpc( data, bs, mb, dct );

                /* Calc the difference */
                range = bs->p - bit_s;

                /* Calc new pointer */
                bs->p = old_data.bit_s + range     ;

                /* Copy back the non zero count in cache */

		/* save non zero count */
		*(uint32_t*) (arg0 + 0*4) = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[0]+0*8];
		*(uint32_t*) (arg0 + 1*4) = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[0]+1*8];
		*(uint32_t*) (arg0 + 2*4) = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[0]+2*8];
		*(uint32_t*) (arg0 + 3*4) = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[0]+3*8];


		*(uint16_t*) (arg0 + 16+0*2) = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16+0*2]-1] >> 8;
		*(uint16_t*) (arg0 + 16+1*2) = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16+1*2]-1] >> 8;
		*(uint16_t*) (arg0 + 16+2*2) = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16+2*2]-1] >> 8;
		*(uint16_t*) (arg0 + 16+3*2) = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16+3*2]-1] >> 8;


#if 0
                if (data->i_type == SLICE_TYPE_B) 
                    printf("%lu \n", tmp1 - spu_read_decrementer() );
#endif

                break;

#endif


	case 11: 
                break;




	case 12:/*  Copy intra */

		/* DST */
             	arg1 = ( void *) task_info->ls_addr;

		/* SRC */
                arg2 = ((void *) arg1 + ex_task->arguments[0].size);

                copy_data = (x264_copy_data_t *) ( (void *)arg2 + ex_task->arguments[1].size);
		{
			int i;
			for (i=copy_data->height;i>0;i--){
				memcpy(arg1,arg2,copy_data->width);
				arg1 += copy_data->i_dst_stride;
				arg2 += copy_data->i_src_stride;
			}
				
				
		}

		break;

#ifdef DEBLOCK
        case 14:
		/* Deblock luma */
             	arg1 = ( void *) task_info->ls_addr;
                fdata =  (x264_filter_data_t *)((void *)arg1 + ex_task->arguments[0].size )  ;
                x264_frame_deblock_row_tpc_luma(fdata,arg1);

                break;

        case 15:
		/* Deblock chroma */
             	arg1 = ( void *) task_info->ls_addr;
#if 1
                arg2 =  ((void *)arg1 + ex_task->arguments[0].size )  ;
                fdata =  (x264_filter_data_t *)((void *)arg2 + ex_task->arguments[1].size )  ;

	        /* Fdec frame chroma */
	        fdec1 =  (uint8_t *)((void *)arg13 + elems*elem_sz );

#else
              
	        elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[0].size);
	        elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[0].size);
	  
                arg2 =  ((void *)arg1 + elems*elem_sz )  ;
              
	        elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[1].size);
	        elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[1].size);

                fdata =  (x264_filter_data_t *)( (void *)arg2 + elems*elem_sz )  ;

#endif
		x264_macroblock_call_deblock_chroma( fdata, arg1, arg2 );
                break;
#endif

	case 21: 
		/* DST */
             	arg1 = ( void *) task_info->ls_addr;
	      	arg2 = ((void *) arg1 + ex_task->arguments[0].size);
                arg3 = ((void *) arg2 + ex_task->arguments[1].size);

		/* SRC */
                arg4 = ((void *) arg3 + ex_task->arguments[2].size);
                arg5 = ((void *) arg4 + ex_task->arguments[3].size);
                arg6 = ((void *) arg5 + ex_task->arguments[4].size);


		memcpy( arg1, arg4, ex_task->arguments[0].size);
		memcpy( arg2, arg5, ex_task->arguments[1].size);
		memcpy( arg3, arg6, ex_task->arguments[2].size);
		break;


	case 22: 
		/* DST */
             	arg1 = ( void *) task_info->ls_addr;

		/* SRC */
                arg2 = ((void *) arg1 + ex_task->arguments[0].size);


		memcpy( arg1, arg2, sizeof(x264_mb_t));
		break;


	case 23: 
		/* DST */
             	arg1 = ( void *) task_info->ls_addr;


		memset( arg1, 0x0, ex_task->arguments[0].size );
		break;



	case 24: 

		/* DST */
             	arg1 = ( void *) task_info->ls_addr;

		/* SRC */
                arg2 = ((void *) arg1 + ex_task->arguments[0].size);


		memcpy( arg1, arg2,  ex_task->arguments[0].size  );
		break;





         default:
                 exit = 1;
		fprintf(stderr, "ERROR: EXIT task id:%d \n", ex_task->funcid);
		SYNC();
                 break;
    }


    task_info->state = EXECUTED;
    return exit;
}

