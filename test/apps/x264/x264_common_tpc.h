#ifndef _X264_COMMON_TPC_H_
#define _X264_COMMON_TPC_H_

/** Data for ME */

/*
    data->offset2 = (( (unsigned long) &h->fenc->plane[1][i_pix_offset_chroma] ) % 16 ) ;
    data->offset3 = (( (unsigned long) &h->fdec->plane[1][i_pix_offset_chroma] ) % 16 ) ;

    data-> i_first_mb        =  h->sh.i_first_mb;
    data-> mb_i_xy           =  mb_xy ;
    data-> i_type            =  h->sh.i_type;
    data-> i_first_mb        =  h->sh.i_first_mb;		
    data-> i_type            =  h->sh.i_type;		
    data-> b_mbaff           =  h->sh.b_mbaff;			
*/



struct x264_data {


	/* Offset bits for non aligned address */
	int offset;  /* offset is for ref frames */
	int offset2; /* offset2 is for fdec chroma */
	int offset3; /* offset2 is for fenc chroma */


	int 	i_first_mb;
        int     i_type;
        int     mb_i_xy;


        /* params  */
        int b_bidir_me;
        int i_range;


        /* Search parameters */
        int     i_me_range;

        int     i_me_method;
        int     i_subpel_refine;

        int     i_chroma_qp;
        int     b_chroma_me;
        int     i_mv_range;
        int     b_mbaff;
	int	b_dct_decimate;

     

        int     i_mb_width;
        int     i_mb_height;
        int     i_direct_mv_pred;
        int     i_trellis;
        int     i_noise_reduction;

        /* K omws prepei */
        int i_mvc;
        int p_cost_offset;
        int pref_offset[3];

        /* extracted from  x264_slice_header_t */
        int i_num_ref_idx_l0_active;
        int i_num_ref_idx_l1_active;

        int     b_trellis;

        int     b_fast_pskip;  
	int 	i_qp;
        int     i_profile_idc;



	/* some extra info for the deblocking */
	int i_stride[2];

    	int b_cabac;
	int b_transform_8x8_mode;





} __attribute__ ((aligned (128)));

typedef struct x264_data x264_data_t;

struct x264_filter_data {

	/* some extra info for the deblocking */
	int i_alpha_c0_offset;
        int i_beta_offset;
	int i_chroma_qp_offset;
	int i_stride[2];
        int pref_offset[3];

        int b_mbaff;
        int i_mb_width;
        int i_qp;

	/* expand border*/
	int i_width[3];
	int i_height;
	int i_padh;
	int i_padv;
	int b_pad_top;
	int b_pad_bottom;

	/* deblock filter */
	int8_t tc[4];

	int  alpha;
	int  beta;

        int mb_y;

        /* stride */

        int xstride;
        int ystride;

        int i_mb_stride;



} __attribute__ ((aligned (128)));

typedef struct x264_filter_data x264_filter_data_t;

struct x264_copy_data {
	/* Offset bits for non aligned address */
	int offset[2];

	/* We need stride */
	int  i_src_stride; 
	int  i_dst_stride; 
	int  height;
	int  width;


} __attribute__ ((aligned (128)));

typedef struct x264_copy_data x264_copy_data_t;



/* output */
struct x264_ret_data {
    int16_t mvp_l0[2];
    int16_t mvp_l1[2];
    int i_list;
    int idx;
    int i_width;

} __attribute__ ((aligned (128)));




/* output */
struct x264_fread {
  uint32_t size;
  FILE *fh;
  int width, height;

} __attribute__ ((aligned (128)));



typedef struct x264_fread x264_fread_t;




typedef struct x264_ret_data x264_ret_data_t;

/** Func arrays for pix */

extern x264_pixel_cmp_t  sadf[7];
extern x264_pixel_cmp_t  ssdf[7];
extern x264_pixel_cmp_t satdf[7];

/* multiple parallel calls to cmp. */
extern x264_pixel_cmp_x3_t sad_x3f[7];
extern x264_pixel_cmp_x4_t sad_x4f[7];
extern x264_pixel_cmp_x3_t satd_x3f[7];
extern x264_pixel_cmp_x4_t satd_x4f[7];

/* abs-diff-sum for successive elimination.
 * may round width up to a multiple of 16. */
 typedef int (*x264_ads_t)( int enc_dc[4], uint16_t *sums, int delta,
               uint16_t *cost_mvx, int16_t *mvs, int width, int thresh );

extern x264_ads_t adsf[7];

extern x264_pixel_cmp_t mbcmpf[7]; /* either satd or sad for subpel refine and mode decision */
extern x264_pixel_cmp_t fpelcmpf[7]; /* either satd or sad for fullpel motion search */
extern x264_pixel_cmp_x3_t fpelcmp_x3f[7];
extern x264_pixel_cmp_x4_t fpelcmp_x4f[7];



/** Functions decl */

uint8_t *get_ref_spe( uint8_t *dst,   int *i_dst_stride,
                         uint8_t *src[4], int i_src_stride,
                         int mvx, int mvy,
                         int i_width, int i_height );


 #define PACK(src2,src1,src_stride,height,width) \
 { \
      int i;  \
      uint8_t *tmp = src1; \
      uint8_t *tmp2 = src2; \
      for (i=0; i<height  ; i++, tmp=tmp+src_stride, tmp2+=(width))  \
          tmp2 = memcpy(tmp2, tmp, width ); \
 }

#endif




