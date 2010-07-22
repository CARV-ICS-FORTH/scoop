#include <assert.h>
#include <stdio.h>
#include <spu_intrinsics.h>
#include "tpc_common.h"
#include "tpc_spe.h"

#include "saxpy.h"

//IN X
//INOUT Y
static inline int saxpy_leaf(float *X, float *Y, float *a)
{

    int i;
       
    // N/4
    int iters = (int)a[1]>>2;
    vector float *py = (vector float *)(Y);
    vector float *px = (vector float *)(X);

    vector float asplat;

    asplat = spu_splats(a[0]);
    
    for (i=0; i<iters; i+= 16) {
        
        vector float x0 = *(px+0);
        vector float x1 = *(px+1);
        vector float x2 = *(px+2);
        vector float x3 = *(px+3);
        vector float x4 = *(px+4);
        vector float x5 = *(px+5);
        vector float x6 = *(px+6);
        vector float x7 = *(px+7);
        vector float x8 = *(px+8);
        vector float x9 = *(px+9);
        vector float x10 = *(px+10);
        vector float x11 = *(px+11);
        vector float x12 = *(px+12);
        vector float x13 = *(px+13);
        vector float x14 = *(px+14);
        vector float x15 = *(px+15);
        
        vector float y0 = *(py+0);
        vector float y1 = *(py+1);
        vector float y2 = *(py+2);
        vector float y3 = *(py+3);
        vector float y4 = *(py+4);
        vector float y5 = *(py+5);
        vector float y6 = *(py+6);
        vector float y7 = *(py+7);
        vector float y8 = *(py+8);
        vector float y9 = *(py+9);
        vector float y10 = *(py+10);
        vector float y11 = *(py+11);
        vector float y12 = *(py+12);
        vector float y13 = *(py+13);
        vector float y14 = *(py+14);
        vector float y15 = *(py+15);
        
        y0 = spu_madd(asplat, x0, y0);
        y1 = spu_madd(asplat, x1, y1);
        y2 = spu_madd(asplat, x2, y2);
        y3 = spu_madd(asplat, x3, y3);
        y4 = spu_madd(asplat, x4, y4);
        y5 = spu_madd(asplat, x5, y5);
        y6 = spu_madd(asplat, x6, y6);
        y7 = spu_madd(asplat, x7, y7);
        y8 = spu_madd(asplat, x8, y8);
        y9 = spu_madd(asplat, x9, y9);
        y10 = spu_madd(asplat, x10, y10);
        y11 = spu_madd(asplat, x11, y11);
        y12 = spu_madd(asplat, x12, y12);
        y13 = spu_madd(asplat, x13, y13);
        y14 = spu_madd(asplat, x14, y14);
        y15 = spu_madd(asplat, x15, y15);
        
        *(py+0) = y0;
        *(py+1) = y1;
        *(py+2) = y2;
        *(py+3) = y3;
        *(py+4) = y4;
        *(py+5) = y5;
        *(py+6) = y6;
        *(py+7) = y7;
        *(py+8) = y8;
        *(py+9) = y9;
        *(py+10) = y10;
        *(py+11) = y11;
        *(py+12) = y12;
        *(py+13) = y13;
        *(py+14) = y14;
        *(py+15) = y15;

	py += 16;
        px += 16;
    }

    return 0;
}





int execute_task(queue_entry_t *ex_task,
   tpc_spe_task_state_t *task_info)
{
  int exit = 0;
  float *arg1, *arg2, *arg3;

  switch(ex_task->funcid)
  {
    case 0:
      arg1 = (float *)task_info->ls_addr;
      arg2 = (float *)((void *)arg1 + ex_task->arguments[0].size);
      arg3 = (float *)((void *)arg2 + ex_task->arguments[1].size);
      //VectAdd_vec(arg1, arg2);
      saxpy_leaf(arg2, arg1, arg3);
      task_info->state = EXECUTED;
      break;
    default:
      exit = 1;
    break;
  }

  return exit;
}

