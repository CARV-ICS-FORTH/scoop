#include <stdio.h>
#include <math.h>
#include "ppu.app.h"

#define DEBUG_PPU_L0_LP0_
#define DEBUG_PPU_L0_LP1_
#define DEBUG_PPU_L0_LP2_

int ppu_loop0(double *pn,double *pl,double *pr,double *tl,double *tr,struct tpc_ds *ds){
	unsigned int iPattern;
	double t1, t2;

	#ifdef DEBUG_PPU_L0_LP0
	printf("LOOP:0. PPU reports : START -> %d - END -> %d\n", ds->start, ds->end);
	#endif

	for(iPattern = ds->start; iPattern < ds->end; iPattern++){
/*
		if(ds->vcted){
			vector double tmp_pl_0_3a = {pl[0],pl[1]};
			vector double tmp_pl_0_3b = {pl[2],pl[3]};
			vector double tmp_pr_0_3a = {pr[0],pr[1]};
			vector double tmp_pr_0_3b = {pr[2],pr[3]};

			vector double tmp_tl_0_3a = {tl[0],tl[1]};
			vector double tmp_tl_0_3b = {tl[2],tl[3]};
			vector double tmp_tl_4_7a = {tl[4],tl[5]};
			vector double tmp_tl_4_7b = {tl[6],tl[7]};
			vector double tmp_tl_8_11a = {tl[8],tl[9]};
			vector double tmp_tl_8_11b = {tl[10],tl[11]};
			vector double tmp_tl_12_15a = {tl[12],tl[13]};
			vector double tmp_tl_12_15b = {tl[14],tl[15]};

			vector double tmp_tr_0_3a = {tr[0],tr[1]};
			vector double tmp_tr_0_3b = {tr[2],tr[3]};
			vector double tmp_tr_4_7a = {tr[4],tr[5]};
			vector double tmp_tr_4_7b = {tr[6],tr[7]};
			vector double tmp_tr_8_11a = {tr[8],tr[9]};
			vector double tmp_tr_8_11b = {tr[10],tr[11]};
			vector double tmp_tr_12_15a = {tr[12],tr[13]};
			vector double tmp_tr_12_15b = {tr[14],tr[15]};

			vector double tmp_t1 = spu_madd(tmp_pl_0_3a, tmp_tl_0_3a, spu_mul(tmp_pl_0_3b, tmp_tl_0_3b));
			vector double tmp_t2 = spu_madd(tmp_pr_0_3a, tmp_tr_0_3a, spu_mul(tmp_pr_0_3b, tmp_tr_0_3b));

			t1 = spu_extract(tmp_t1, 0) + spu_extract(tmp_t1, 1);
			t2 = spu_extract(tmp_t2, 0) + spu_extract(tmp_t2, 1);
			pn[0] = t1 * t2;

			tmp_t1 = spu_madd(tmp_pl_0_3a, tmp_tl_4_7a, spu_mul(tmp_pl_0_3b, tmp_tl_4_7b));
			tmp_t2 = spu_madd(tmp_pr_0_3a, tmp_tr_4_7a, spu_mul(tmp_pr_0_3b, tmp_tr_4_7b));
			t1 = spu_extract(tmp_t1, 0) + spu_extract(tmp_t1, 1);
			t2 = spu_extract(tmp_t2, 0) + spu_extract(tmp_t2, 1);
			pn[1] = t1 * t2;
 
			tmp_t1 = spu_madd(tmp_pl_0_3a, tmp_tl_8_11a, spu_mul(tmp_pl_0_3b, tmp_tl_8_11b));
			tmp_t2 = spu_madd(tmp_pr_0_3a, tmp_tr_8_11a, spu_mul(tmp_pr_0_3b, tmp_tr_8_11b));
			t1 = spu_extract(tmp_t1, 0) + spu_extract(tmp_t1, 1);
			t2 = spu_extract(tmp_t2, 0) + spu_extract(tmp_t2, 1);
			pn[2] = t1 * t2;

			tmp_t1 = spu_madd(tmp_pl_0_3a, tmp_tl_12_15a, spu_mul(tmp_pl_0_3b, tmp_tl_12_15b));
			tmp_t2 = spu_madd(tmp_pr_0_3a, tmp_tr_12_15a, spu_mul(tmp_pr_0_3b, tmp_tr_12_15b));
			t1 = spu_extract(tmp_t1, 0) + spu_extract(tmp_t1, 1);
			t2 = spu_extract(tmp_t2, 0) + spu_extract(tmp_t2, 1);
			pn[3] = t1 * t2;
		}
*/
//		else{
			t1 = pl[0] * tl[0] + pl[1] * tl[1] + pl[2] * tl[2] + pl[3] * tl[3];
			t2 = pr[0] * tr[0] + pr[1] * tr[1] + pr[2] * tr[2] + pr[3] * tr[3];
			pn[0] = t1 * t2;
	
		        t1 = pl[0] * tl[4] + pl[1] * tl[5] + pl[2] * tl[6] + pl[3] * tl[7];
			t2 = pr[0] * tr[4] + pr[1] * tr[5] + pr[2] * tr[6] + pr[3] * tr[7];
			pn[1] = t1 * t2;
	
			t1 = pl[0] * tl[8] + pl[1] * tl[9] + pl[2] * tl[10] + pl[3] * tl[11];
			t2 = pr[0] * tr[8] + pr[1] * tr[9] + pr[2] * tr[10] + pr[3] * tr[11];
			pn[2] = t1 * t2;
	
			t1 = pl[0] * tl[12] + pl[1] * tl[13] + pl[2] * tl[14] + pl[3] * tl[15];
			t2 = pr[0] * tr[12] + pr[1] * tr[13] + pr[2] * tr[14] + pr[3] * tr[15];
			pn[3] = t1 * t2;
//		}

		pn += 4;
		pl += 4;
		pr += 4;
	}
	return 0;
}

int ppu_loop1(double *pn,double *pl,double *pr,double *pp,double *tl,double *tr,double *tp,struct tpc_ds *ds){
	unsigned int iPattern;
	double t1, t2, t3;

	#ifdef DEBUG_PPU_L0_LP1
	printf("LOOP:1. PPU reports : START -> %d - END -> %d\n", ds->start, ds->end);
	#endif

	for( iPattern = ds->start; iPattern < ds->end; iPattern++){
/*
		if(ds->vcted){
			vector double tmp_pl_0_3a = {pl[0],pl[1]};
			vector double tmp_pl_0_3b = {pl[2],pl[3]};
			vector double tmp_pr_0_3a = {pr[0],pr[1]};
			vector double tmp_pr_0_3b = {pr[2],pr[3]};
			vector double tmp_pp_0_3a = {pp[0],pp[1]};
			vector double tmp_pp_0_3b = {pp[2],pp[3]};

			vector double tmp_tl_0_3a = {tl[0],tl[1]};
			vector double tmp_tl_0_3b = {tl[2],tl[3]};
			vector double tmp_tl_4_7a = {tl[4],tl[5]};
			vector double tmp_tl_4_7b = {tl[6],tl[7]};
			vector double tmp_tl_8_11a = {tl[8],tl[9]};
			vector double tmp_tl_8_11b = {tl[10],tl[11]};
			vector double tmp_tl_12_15a = {tl[12],tl[13]};
			vector double tmp_tl_12_15b = {tl[14],tl[15]};

			vector double tmp_tr_0_3a = {tr[0],tr[1]};
			vector double tmp_tr_0_3b = {tr[2],tr[3]};
			vector double tmp_tr_4_7a = {tr[4],tr[5]};
			vector double tmp_tr_4_7b = {tr[6],tr[7]};
			vector double tmp_tr_8_11a = {tr[8],tr[9]};
			vector double tmp_tr_8_11b = {tr[10],tr[11]};
			vector double tmp_tr_12_15a = {tr[12],tr[13]};
			vector double tmp_tr_12_15b = {tr[14],tr[15]};

			vector double tmp_tp_0_3a = {tp[0],tp[1]};
			vector double tmp_tp_0_3b = {tp[2],tp[3]};
			vector double tmp_tp_4_7a = {tp[4],tp[5]};
			vector double tmp_tp_4_7b = {tp[6],tp[7]};
			vector double tmp_tp_8_11a = {tp[8],tp[9]};
			vector double tmp_tp_8_11b = {tp[10],tp[11]};
			vector double tmp_tp_12_15a = {tp[12],tp[13]};
			vector double tmp_tp_12_15b = {tp[14],tp[15]};

			vector double tmp_t1 = spu_madd(tmp_pl_0_3a, tmp_tl_0_3a, spu_mul(tmp_pl_0_3b, tmp_tl_0_3b));
			vector double tmp_t2 = spu_madd(tmp_pr_0_3a, tmp_tr_0_3a, spu_mul(tmp_pr_0_3b, tmp_tr_0_3b));
			vector double tmp_t3 = spu_madd(tmp_pp_0_3a, tmp_tp_0_3a, spu_mul(tmp_pp_0_3b, tmp_tp_0_3b));

			t1 = spu_extract(tmp_t1, 0) + spu_extract(tmp_t1, 1);
			t2 = spu_extract(tmp_t2, 0) + spu_extract(tmp_t2, 1);
			t3 = spu_extract(tmp_t3, 0) + spu_extract(tmp_t3, 1);
			pn[0] = t1 * t2 * t3;

			tmp_t1 = spu_madd(tmp_pl_0_3a, tmp_tl_4_7a, spu_mul(tmp_pl_0_3b, tmp_tl_4_7b));
			tmp_t2 = spu_madd(tmp_pr_0_3a, tmp_tr_4_7a, spu_mul(tmp_pr_0_3b, tmp_tr_4_7b));
			tmp_t3 = spu_madd(tmp_pp_0_3a, tmp_tp_4_7a, spu_mul(tmp_pp_0_3b, tmp_tp_4_7b));
			t1 = spu_extract(tmp_t1, 0) + spu_extract(tmp_t1, 1);
			t2 = spu_extract(tmp_t2, 0) + spu_extract(tmp_t2, 1);
			t3 = spu_extract(tmp_t3, 0) + spu_extract(tmp_t3, 1);
			pn[1] = t1 * t2 * t3;
 
			tmp_t1 = spu_madd(tmp_pl_0_3a, tmp_tl_8_11a, spu_mul(tmp_pl_0_3b, tmp_tl_8_11b));
			tmp_t2 = spu_madd(tmp_pr_0_3a, tmp_tr_8_11a, spu_mul(tmp_pr_0_3b, tmp_tr_8_11b));
			tmp_t3 = spu_madd(tmp_pp_0_3a, tmp_tp_8_11a, spu_mul(tmp_pp_0_3b, tmp_tp_8_11b));
			t1 = spu_extract(tmp_t1, 0) + spu_extract(tmp_t1, 1);
			t2 = spu_extract(tmp_t2, 0) + spu_extract(tmp_t2, 1);
			t3 = spu_extract(tmp_t3, 0) + spu_extract(tmp_t3, 1);
			pn[2] = t1 * t2 * t3;

			tmp_t1 = spu_madd(tmp_pl_0_3a, tmp_tl_12_15a, spu_mul(tmp_pl_0_3b, tmp_tl_12_15b));
			tmp_t2 = spu_madd(tmp_pr_0_3a, tmp_tr_12_15a, spu_mul(tmp_pr_0_3b, tmp_tr_12_15b));
			tmp_t3 = spu_madd(tmp_pp_0_3a, tmp_tp_12_15a, spu_mul(tmp_pp_0_3b, tmp_tp_12_15b));
			t1 = spu_extract(tmp_t1, 0) + spu_extract(tmp_t1, 1);
			t2 = spu_extract(tmp_t2, 0) + spu_extract(tmp_t2, 1);
			t3 = spu_extract(tmp_t3, 0) + spu_extract(tmp_t3, 1);
			pn[3] = t1 * t2 * t3;
		}
*/
//		else{
			t1 = pl[0] * tl[0] + pl[1] * tl[1] + pl[2] * tl[2] + pl[3] * tl[3];
			t2 = pr[0] * tr[0] + pr[1] * tr[1] + pr[2] * tr[2] + pr[3] * tr[3];
			t3 = pp[0] * tp[0] + pp[1] * tp[1] + pp[2] * tp[2] + pp[3] * tp[3];
			pn[0] = t1 * t2 * t3;
		
			t1 = pl[0] * tl[4] + pl[1] * tl[5] + pl[2] * tl[6] + pl[3] * tl[7];
			t2 = pr[0] * tr[4] + pr[1] * tr[5] + pr[2] * tr[6] + pr[3] * tr[7];
			t3 = pp[0] * tp[4] + pp[1] * tp[5] + pp[2] * tp[6] + pp[3] * tp[7];
			pn[1] = t1 * t2 * t3;
		
			t1 = pl[0] * tl[8] + pl[1] * tl[9] + pl[2] * tl[10] + pl[3] * tl[11];
			t2 = pr[0] * tr[8] + pr[1] * tr[9] + pr[2] * tr[10] + pr[3] * tr[11];
			t3 = pp[0] * tp[8] + pp[1] * tp[9] + pp[2] * tp[10] + pp[3] * tp[11];
			pn[2] = t1 * t2 * t3;
	
			t1 = pl[0] * tl[12] + pl[1] * tl[13] + pl[2] * tl[14] + pl[3] * tl[15];
			t2 = pr[0] * tr[12] + pr[1] * tr[13] + pr[2] * tr[14] + pr[3] * tr[15];
			t3 = pp[0] * tp[12] + pp[1] * tp[13] + pp[2] * tp[14] + pp[3] * tp[15];
			pn[3] = t1 * t2 * t3;
//		}
		pn += 4;
		pl += 4;
		pr += 4;
		pp += 4;
	}
	return 0;
}

int ppu_loop2(double *lnL,double *sroot,double *freq,struct tpc_ds *ds,int *weight){
	unsigned int iPattern;
	double temp = 0.0, local_lnL = 0.0;

	#ifdef DEBUG_PPU_L0_LP2
	printf("LOOP:2. PPU reports : START -> %d - END -> %d\n", ds->start, ds->end);
	#endif

	for( iPattern = ds->start; iPattern < ds->end; iPattern++){
/*
		if(ds->vcted){
			vector double tmp_sroot_0_3a = {sroot[0],sroot[1]};
			vector double tmp_sroot_0_3b = {sroot[2],sroot[3]};
			vector double tmp_freq_0_3a = {freq[0],freq[1]};
			vector double tmp_freq_0_3b = {freq[2],freq[3]};

			vector double tmp = spu_madd(tmp_sroot_0_3a, tmp_freq_0_3a, spu_mul(tmp_sroot_0_3b, tmp_freq_0_3b));

			temp = spu_extract(tmp, 0) + spu_extract(tmp, 1);
		}
*/
//		else
			temp = sroot[0] * freq[0] + sroot[1] * freq[1] + sroot[2] * freq[2] + sroot[3] * freq[3];

		if( temp == 0.0 )
			temp = -1.0e10;
		else
			temp = log(temp);
		local_lnL +=  weight[ iPattern ] * temp;

		sroot += 4;
	}
	*lnL = local_lnL;

	return 0;
}

