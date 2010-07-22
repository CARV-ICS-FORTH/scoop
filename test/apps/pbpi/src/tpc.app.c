#include <vec_types.h>
#include "spu_intrinsics.h"
#include "tpc.app.h"
#include "log/log.h"
#include "tpc_common.h"
#include "tpc_ppe.h"
#include "tpc_spe.h"

#define DEBUG_SPE_L0_LP0_
#define DEBUG_SPE_L0_LP1_
#define DEBUG_SPE_L0_LP2_

/* takes first array value from two vectors. */
vector unsigned char pattern0 = {0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17};
/* takes second array value from two vectors. */
vector unsigned char pattern1 = {0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F};

int execute_task(queue_entry_t *ex_task, tpc_spe_task_state_t *task_info)
{
  int exit = 0;
  struct tpc_ds *lp0_arg5, *lp1_arg7, *lp2_arg3;
  double *lp0_arg0, *lp0_arg1, *lp0_arg2, *lp0_arg3, *lp0_arg4;
  double *lp1_arg0, *lp1_arg1, *lp1_arg2, *lp1_arg3, *lp1_arg4, *lp1_arg5, *lp1_arg6;
  long double *lp2_arg0;
  double *lp2_arg1, *lp2_arg2;
  int *lp2_arg4;

  switch(ex_task->funcid)
  {
    case LOOP_0:
      lp0_arg0 = (double *)task_info->ls_addr;
      lp0_arg1 = (double *)((void *)lp0_arg0 + ex_task->arguments[0].size);
      lp0_arg2 = (double *)((void *)lp0_arg1 + ex_task->arguments[1].size);
      lp0_arg3 = (double *)((void *)lp0_arg2 + ex_task->arguments[2].size);
      lp0_arg4 = (double *)((void *)lp0_arg3 + ex_task->arguments[3].size);
      lp0_arg5 = (struct tpc_ds *)((void *)lp0_arg4 + ex_task->arguments[4].size);
      tpc_loop0(lp0_arg0, lp0_arg1, lp0_arg2, lp0_arg3, lp0_arg4, lp0_arg5);
      task_info->state = EXECUTED;
      break;
	case LOOP_1:
      lp1_arg0 = (double *)task_info->ls_addr;
      lp1_arg1 = (double *)((void *)lp1_arg0 + ex_task->arguments[0].size);
      lp1_arg2 = (double *)((void *)lp1_arg1 + ex_task->arguments[1].size);
      lp1_arg3 = (double *)((void *)lp1_arg2 + ex_task->arguments[2].size);
      lp1_arg4 = (double *)((void *)lp1_arg3 + ex_task->arguments[3].size);
      lp1_arg5 = (double *)((void *)lp1_arg4 + ex_task->arguments[4].size);
      lp1_arg6 = (double *)((void *)lp1_arg5 + ex_task->arguments[5].size);
      lp1_arg7 = (struct tpc_ds *)((void *)lp1_arg6 + ex_task->arguments[6].size);
      tpc_loop1(lp1_arg0, lp1_arg1, lp1_arg2, lp1_arg3, lp1_arg4, lp1_arg5, lp1_arg6, lp1_arg7);
      task_info->state = EXECUTED;
      break;
    case LOOP_2:
      lp2_arg0 = (long double *)task_info->ls_addr;
      lp2_arg1 = (double *)((void *)lp2_arg0 + ex_task->arguments[0].size);
      lp2_arg2 = (double *)((void *)lp2_arg1 + ex_task->arguments[1].size);
      lp2_arg3 = (struct tpc_ds *)((void *)lp2_arg2 + ex_task->arguments[2].size);
      lp2_arg4 = (int *)((void *)lp2_arg3 + ex_task->arguments[3].size);
      tpc_loop2(lp2_arg0, lp2_arg1, lp2_arg2, lp2_arg3, lp2_arg4);
      task_info->state = EXECUTED;
      break;
    default:
      exit = 1;
    break;
  }
  return exit;
}

static int tpc_loop0(double *pn,double *pl,double *pr,double *tl,double *tr,struct tpc_ds *ds){
	unsigned int iPattern;
	double t1, t2;

	#ifdef DEBUG_SPE_L0_LP0
	printf("LOOP:0 .SPE no %d reports : START -> %d - END -> %d\n", ds->spe_no, ds->start, ds->end);
	#endif

	if(ds->vcted) {
		for(iPattern = ds->start; __builtin_expect(iPattern < ds->end,1); iPattern++) {
#if 0
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

			vector double tmp_t1_pn0 = spu_madd(tmp_pl_0_3a, tmp_tl_0_3a, spu_mul(tmp_pl_0_3b, tmp_tl_0_3b));
			vector double tmp_t2_pn0 = spu_madd(tmp_pr_0_3a, tmp_tr_0_3a, spu_mul(tmp_pr_0_3b, tmp_tr_0_3b));

			vector double tmp_t1_pn1 = spu_madd(tmp_pl_0_3a, tmp_tl_4_7a, spu_mul(tmp_pl_0_3b, tmp_tl_4_7b));
			vector double tmp_t2_pn1 = spu_madd(tmp_pr_0_3a, tmp_tr_4_7a, spu_mul(tmp_pr_0_3b, tmp_tr_4_7b));

			vector double tmp_t1_pn2 = spu_madd(tmp_pl_0_3a, tmp_tl_8_11a, spu_mul(tmp_pl_0_3b, tmp_tl_8_11b));
			vector double tmp_t2_pn2 = spu_madd(tmp_pr_0_3a, tmp_tr_8_11a, spu_mul(tmp_pr_0_3b, tmp_tr_8_11b));

			vector double tmp_t1_pn3 = spu_madd(tmp_pl_0_3a, tmp_tl_12_15a, spu_mul(tmp_pl_0_3b, tmp_tl_12_15b));
			vector double tmp_t2_pn3 = spu_madd(tmp_pr_0_3a, tmp_tr_12_15a, spu_mul(tmp_pr_0_3b, tmp_tr_12_15b));

			vector double vec_pn0  = spu_add( spu_shuffle(tmp_t1_pn0,tmp_t2_pn0,pattern0) , spu_shuffle(tmp_t1_pn0,tmp_t2_pn0,pattern1) );
			vector double vec_pn1  = spu_add( spu_shuffle(tmp_t1_pn1,tmp_t2_pn1,pattern0) , spu_shuffle(tmp_t1_pn1,tmp_t2_pn1,pattern1) );
			vector double vec_pn2  = spu_add( spu_shuffle(tmp_t1_pn2,tmp_t2_pn2,pattern0) , spu_shuffle(tmp_t1_pn2,tmp_t2_pn2,pattern1) );
			vector double vec_pn3  = spu_add( spu_shuffle(tmp_t1_pn3,tmp_t2_pn3,pattern0) , spu_shuffle(tmp_t1_pn3,tmp_t2_pn3,pattern1) );
			
			vector double vec_tmp0  = spu_mul( spu_shuffle(vec_pn0,vec_pn1,pattern0) , spu_shuffle(vec_pn0,vec_pn1,pattern1) );
			vector double vec_tmp1  = spu_mul( spu_shuffle(vec_pn2,vec_pn3,pattern0) , spu_shuffle(vec_pn2,vec_pn3,pattern1) );
			
			pn[0] = spu_extract(vec_tmp0, 0);
			pn[1] = spu_extract(vec_tmp0, 1);
			pn[2] = spu_extract(vec_tmp1, 0);
			pn[3] = spu_extract(vec_tmp1, 1);

/*
			vector double tmp_t1= spu_madd(tmp_pl_0_3a, tmp_tl_0_3a, spu_mul(tmp_pl_0_3b, tmp_tl_0_3b));
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
                        */
#endif
                        double t1, t2;
                        typedef __attribute__((vector_size(16))) double vdouble;
                        typedef __attribute__((vector_size(16))) unsigned char vuchar;
                        vdouble * vpl = (vdouble *) (&pl[(((iPattern)*4)+0)]);
                        vdouble * vpr = (vdouble *) (&pr[(((iPattern)*4)+0)]);
                        vdouble * vpn = (vdouble *) (&pn[(((iPattern)*4)+0)]);
                        vdouble * vtl = (vdouble *) tl, * vtr = (vdouble *) tr;
                        vdouble tmp1,tmp2,tmp3,tmp4;
                        vdouble t1_v0, t1_v1, t1_v2, t1_v3;
                        vdouble t2_v0, t2_v1, t2_v2, t2_v3;
                        vdouble t_10, t_11;
                        vdouble t_20, t_21;
                        static const vuchar PATTERN1 = {0x00, 0x01, 0x02, 0x03, 0x04,
                            0x05, 0x06, 0x07, 0x10, 0x11,
                            0x12, 0x13, 0x14, 0x15, 0x16, 0x17};
                        static const vuchar PATTERN2 = {0x08, 0x09, 0x0a, 0x0b, 0x0c,
                            0x0d, 0x0e, 0x0f, 0x18, 0x19,
                            0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f};
                        t1_v0 = __builtin_spu_madd(vpl[0],vtl[0],
                                __builtin_spu_mul(vpl[1],vtl[1]));
                        t1_v1 = __builtin_spu_madd(vpl[0],vtl[2],
                                __builtin_spu_mul(vpl[1],vtl[3]));
                        tmp1 = __builtin_spu_shuffle(t1_v0, t1_v1, PATTERN1);

                        t2_v0 = __builtin_spu_madd(vpr[0],vtr[0],
                                __builtin_spu_mul(vpr[1],vtr[1]));
                        tmp2 = __builtin_spu_shuffle(t1_v0, t1_v1, PATTERN2);
                        t2_v1 = __builtin_spu_madd(vpr[0],vtr[2],
                                __builtin_spu_mul(vpr[1],vtr[3]));

                        tmp3 = __builtin_spu_shuffle(t2_v0, t2_v1, PATTERN1);
                        t_10 = __builtin_spu_add(tmp1,tmp2);

                        tmp4 = __builtin_spu_shuffle(t2_v0, t2_v1, PATTERN2);

                        t_20 = __builtin_spu_add(tmp3,tmp4);
                        vpn[0] = __builtin_spu_mul(t_10,t_20);

                        t1_v2 = __builtin_spu_madd(vpl[0],vtl[4],__builtin_spu_mul(vpl[1],vtl[5]));
                        t1_v3 = __builtin_spu_madd(vpl[0],vtl[6],__builtin_spu_mul(vpl[1],vtl[7]));
                        tmp1 = __builtin_spu_shuffle(t1_v2, t1_v3, PATTERN1);
                        t2_v2 = __builtin_spu_madd(vpr[0],vtr[4],__builtin_spu_mul(vpr[1],vtr[5]));
                        tmp2 = __builtin_spu_shuffle(t1_v2, t1_v3, PATTERN2);
                        t2_v3 = __builtin_spu_madd(vpr[0],vtr[6],__builtin_spu_mul(vpr[1],vtr[7]));
                        tmp3 = __builtin_spu_shuffle(t2_v2, t2_v3, PATTERN1);
                        t_11 = __builtin_spu_add(tmp1,tmp2);
                        tmp4 = __builtin_spu_shuffle(t2_v2, t2_v3, PATTERN2);
                        t_21 = __builtin_spu_add(tmp3,tmp4);
                        vpn[1] = __builtin_spu_mul(t_11,t_21);

		//	pn += 4;
		//	pl += 4;
		//	pr += 4;
		}
	}
	else {
		for(iPattern = ds->start; iPattern < ds->end; iPattern++) {
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
			
			pn += 4;
			pl += 4;
			pr += 4;
		}
	}
	return 0;
}

static int tpc_loop1(double *pn,double *pl,double *pr,double *pp,double *tl,double *tr,double *tp,struct tpc_ds *ds){
	unsigned int iPattern;
	double t1, t2, t3;

	#ifdef DEBUG_SPE_L0_LP1
	printf("LOOP:1. SPE no %d reports : START -> %d - END -> %d\n", ds->spe_no, ds->start, ds->end);
	#endif

	if(ds->vcted) {
		for( iPattern = ds->start; __builtin_expect( iPattern < ds->end,1); iPattern++) {
#if 0
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
			
			pn += 4;
			pl += 4;
			pr += 4;
			pp += 4;
#else
           double t1, t2, t3;
                  typedef __attribute__((vector_size(16))) double vdoubl;
                  typedef __attribute__((vector_size(16))) unsigned char vucha;
                  vdoubl * vpl = (vdoubl *) (&pl[(((iPattern)*4)+0)]);
                  vdoubl * vpr = (vdoubl *) (&pr[(((iPattern)*4)+0)]);
                  vdoubl * vpp = (vdoubl *) (&pp[(((iPattern)*4)+0)]);
                  vdoubl * vpn = (vdoubl *) (&pn[(((iPattern)*4)+0)]);
                  vdoubl * vtl = (vdoubl *) tl, * vtr = (vdoubl *) tr,
                          * vtp = (vdoubl *) tp;
                  vdoubl tmp1,tmp2,tmp3,tmp4,tmp5,tmp6;
                  vdoubl t1_v0, t1_v1, t1_v2, t1_v3;
                  vdoubl t2_v0, t2_v1, t2_v2, t2_v3;
                  vdoubl t3_v0, t3_v1, t3_v2, t3_v3;
                  vdoubl t_10, t_11;
                  vdoubl t_20, t_21;
                  vdoubl t_30, t_31;
                  static const vucha PATTERN1 = {0x00, 0x01, 0x02, 0x03, 0x04,
                                           0x05, 0x06, 0x07, 0x10, 0x11,
                                           0x12, 0x13, 0x14, 0x15, 0x16, 0x17};
                  static const vucha PATTERN2 = {0x08, 0x09, 0x0a, 0x0b, 0x0c,
                                           0x0d, 0x0e, 0x0f, 0x18, 0x19,
                                           0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f};
                  t1_v0 = __builtin_spu_madd(vpl[0],vtl[0],
                                __builtin_spu_mul(vpl[1],vtl[1]));
                  t1_v1 = __builtin_spu_madd(vpl[0],vtl[2],
                                 __builtin_spu_mul(vpl[1],vtl[3]));
                  tmp1 = __builtin_spu_shuffle(t1_v0, t1_v1, PATTERN1);

                  t2_v0 = __builtin_spu_madd(vpr[0],vtr[0],
                                 __builtin_spu_mul(vpr[1],vtr[1]));
                  tmp2 = __builtin_spu_shuffle(t1_v0, t1_v1, PATTERN2);
                  t2_v1 = __builtin_spu_madd(vpr[0],vtr[2],
                                 __builtin_spu_mul(vpr[1],vtr[3]));
                  tmp3 = __builtin_spu_shuffle(t2_v0, t2_v1, PATTERN1);

                  t3_v0 = __builtin_spu_madd(vpp[0],vtp[0],
                                 __builtin_spu_mul(vpp[1],vtp[1]));
                  tmp4 = __builtin_spu_shuffle(t2_v0, t2_v1, PATTERN2);
                  t3_v1 = __builtin_spu_madd(vpp[0],vtp[2],
                                 __builtin_spu_mul(vpp[1],vtp[3]));
                  tmp5 = __builtin_spu_shuffle(t3_v0, t3_v1, PATTERN1);
                  t_10 = __builtin_spu_add(tmp1,tmp2);

                  tmp6 = __builtin_spu_shuffle(t3_v0, t3_v1, PATTERN2);

                  t_20 = __builtin_spu_add(tmp3,tmp4);
                  t_30 = __builtin_spu_add(tmp5,tmp6);
                  vpn[0] = __builtin_spu_mul(__builtin_spu_mul(t_10,t_20),t_30);


                  t1_v2 = __builtin_spu_madd(vpl[0],vtl[4],
                                __builtin_spu_mul(vpl[1],vtl[5]));
                  t1_v3 = __builtin_spu_madd(vpl[0],vtl[6],
                                 __builtin_spu_mul(vpl[1],vtl[7]));
                  tmp1 = __builtin_spu_shuffle(t1_v2, t1_v3, PATTERN1);

                  t2_v2 = __builtin_spu_madd(vpr[0],vtr[4],
                                 __builtin_spu_mul(vpr[1],vtr[5]));
                  tmp2 = __builtin_spu_shuffle(t1_v2, t1_v3, PATTERN2);
                  t2_v3 = __builtin_spu_madd(vpr[0],vtr[6],
                                 __builtin_spu_mul(vpr[1],vtr[7]));
                  tmp3 = __builtin_spu_shuffle(t2_v2, t2_v3, PATTERN1);

                  t3_v2 = __builtin_spu_madd(vpp[0],vtp[4],
                                 __builtin_spu_mul(vpp[1],vtp[5]));
                  tmp4 = __builtin_spu_shuffle(t2_v2, t2_v3, PATTERN2);
                  t3_v3 = __builtin_spu_madd(vpp[0],vtp[6],
                                 __builtin_spu_mul(vpp[1],vtp[7]));
                  tmp5 = __builtin_spu_shuffle(t3_v2, t3_v3, PATTERN1);
                  t_11 = __builtin_spu_add(tmp1,tmp2);

                  tmp6 = __builtin_spu_shuffle(t3_v2, t3_v3, PATTERN2);

                  t_21 = __builtin_spu_add(tmp3,tmp4);
                  t_31 = __builtin_spu_add(tmp5,tmp6);
                  vpn[1] = __builtin_spu_mul(__builtin_spu_mul(t_11,t_21),t_31);
#endif
		}
	}
	else {
		for( iPattern = ds->start; iPattern < ds->end; iPattern++) {
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

			pn += 4;
			pl += 4;
			pr += 4;
			pp += 4;
		}
	}
	return 0;
}

static int tpc_loop2(long double *lnL,double *sroot,double *freq,struct tpc_ds *ds,int *weight){
	unsigned int iPattern;
	long double temp, local_lnL = 0.0;
        int result_e;
        long double result[2];
	#ifdef DEBUG_SPE_L0_LP2
	printf("LOOP:2. SPE no %d reports : START -> %d - END -> %d\n", ds->spe_no, ds->start, ds->end);
	#endif


        typedef __attribute__((vector_size(16))) double vdoub;
        typedef __attribute__((vector_size(16))) unsigned char vuch;
        vdoub logd2(vdoub x);
        double log (double x);
        static const vuch PATTERN1 = {0x00, 0x01, 0x02, 0x03, 0x04,
            0x05, 0x06, 0x07, 0x10, 0x11,
            0x12, 0x13, 0x14, 0x15, 0x16, 0x17};
        static const vuch PATTERN2 = {0x08, 0x09, 0x0a, 0x0b, 0x0c,
            0x0d, 0x0e, 0x0f, 0x18, 0x19,
            0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f};
        typedef union {
            vdoub vd;
            double d[2];
        } v_t;
        v_t alnL_v;
        v_t sum_v0, sum_v1;
        vdoub * freq_v;



	if(ds->vcted) {
		for( iPattern = ds->start; __builtin_expect(iPattern < ds->end,1); iPattern++) {
#if 0
			// FIXME: NOT WORKING
                        double temp;
                        v_t temp_v0, temp_v1;
                        v_t weight_v;
                        freq_v = (vdoub *) &freq[0];
                        alnL_v.d[0] = lnL[iPattern];
                        alnL_v.d[1] = 0.0;
                        weight_v.d[0] = (double) weight[(iPattern)*2];
                        weight_v.d[1] = (double) weight[((iPattern)*2)+1];
                        vdoub * sroot_v = (vdoub *) &sroot[(((iPattern)*8)+0)];
                        temp_v0.vd = __builtin_spu_madd(sroot_v[0],freq_v[0],
                                __builtin_spu_mul(sroot_v[1],freq_v[1]));
                        temp_v1.vd = __builtin_spu_madd(sroot_v[2],freq_v[0],
                                __builtin_spu_mul(sroot_v[3],freq_v[1]));
                        sum_v0.vd = __builtin_spu_shuffle(temp_v0.vd, temp_v1.vd, PATTERN1);
                        sum_v1.vd = __builtin_spu_shuffle(temp_v0.vd, temp_v1.vd, PATTERN2);
                        sum_v0.vd = __builtin_spu_add(sum_v0.vd,sum_v1.vd);
                        sum_v0.vd = logd2(sum_v0.vd);
                        alnL_v.vd = __builtin_spu_madd(weight_v.vd,sum_v0.vd, alnL_v.vd);
                        temp =  alnL_v.d[0] + alnL_v.d[1];

                        if( temp == 0.0 ) {
				temp = -1.0e10;
			}
			else {
				temp = _log(temp);
			}

			local_lnL +=  weight[ iPattern ] * temp;
			sroot+= 4;
                                                                                                                                                                        
#else
			vector double tmp_sroot_0_3a = {sroot[0],sroot[1]};
			vector double tmp_sroot_0_3b = {sroot[2],sroot[3]};
			vector double tmp_freq_0_3a = {freq[0],freq[1]};
			vector double tmp_freq_0_3b = {freq[2],freq[3]};

			vector double tmp = spu_madd(tmp_sroot_0_3a, tmp_freq_0_3a, spu_mul(tmp_sroot_0_3b, tmp_freq_0_3b));

			temp = spu_extract(tmp, 0) + spu_extract(tmp, 1);
			
#if 1                       
			if( temp == 0.0 ) {
				temp = -1.0e10;
			}
			else {
				temp = _log(temp);
			}
#else
                        
                        result_e = (temp==0.0);
                        result[1] = -1.0e10;
                        result[0] = _log(temp);
                        temp = result[result_e];
#endif
			local_lnL +=  weight[ iPattern ] * temp;

			sroot+= 4;
#endif
		}
	}
	else {
		for( iPattern = ds->start; iPattern < ds->end; iPattern++) {
			temp = sroot[0] * freq[0] + sroot[1] * freq[1] + sroot[2] * freq[2] + sroot[3] * freq[3];
			
			if( temp == 0.0 ) {
				temp = -1.0e10;
			}
			else {
				temp = _log(temp);
			}

			local_lnL +=  weight[ iPattern ] * temp;

			sroot += 4;
		}
	}

	*lnL = local_lnL;

	return 0;
}

