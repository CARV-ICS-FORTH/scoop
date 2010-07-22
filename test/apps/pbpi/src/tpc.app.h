#include "common.h"

#ifndef _TPC_APP_H_
#define _TPC_APP_H_

#define LOOP_0 0
#define LOOP_1 1
#define LOOP_2 2

static int tpc_loop0(double *pn,double *pl,double *pr,double *tl,double *tr,struct tpc_ds *ds);
static int tpc_loop1(double *pn,double *pl,double *pr,double *pp,double *tl,double *tr,double *tp,struct tpc_ds *ds);
static int tpc_loop2(long double *lnL,double *sroot,double *freq,struct tpc_ds *ds,int *weight);

#endif

