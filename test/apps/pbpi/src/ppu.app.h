#include "tpc.app.h"

#ifndef _PPU_APP_H_
#define _PPU_APP_H_

int ppu_loop0(double *pn,double *pl,double *pr,double *tl,double *tr,struct tpc_ds *ds);
int ppu_loop1(double *pn,double *pl,double *pr,double *pp,double *tl,double *tr,double *tp,struct tpc_ds *ds);
int ppu_loop2(double *lnL,double *sroot,double *freq,struct tpc_ds *ds,int *weight);

#endif

