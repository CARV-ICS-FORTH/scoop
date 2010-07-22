#include <assert.h>
#include <stdio.h>
#include <spu_intrinsics.h>
#include "tpc_common.h"
#include "tpc_spe.h"

#include "conv2d.h"
#include "conv2d_leaf_cell.h"

//void conv2d_leafext(sqArray_t *A, sqArray_t *H, sqArray_t *C);

void conv2d_leafext(float *A, float *H, float *C);

//void scalar_conv2d( float *a, float *h, float *c)
static inline void scalar_conv2d( float *a, float *h, float *c)
{
    unsigned int sN=S, sM=T;
    unsigned int sU=9, sV=9;

    int aoffset = sM+sV-1;

    int n, m, u, v;
    for (n = 0; n < sN; n++) {
        for (m = 0; m < sM; m++) {
            c[n*sM+m] = 0;
            for (u = 0; u < sU; u++) {
                for (v = 0; v < sV; v++) {
                    //c[n*sM+m] += h[u*sV+v] * a[(n+u)*(aoffset)+(m+v)];
                    c[n*sM+m] += h[u*sV+v] * a[(n+u)*(aoffset)+(m+v)];
                }
            }
        }
    }
   
}

//static float h_local[9*12] __attribute__ ((aligned (128)));

int execute_task(queue_entry_t *ex_task,
    tpc_spe_task_state_t *task_info)
{
  int exit = 0;
  int elems, elem_sz;
  float *arg1, *arg2, *arg3;

  switch(ex_task->funcid)
  {
    case 0:
      arg1 = (float *)task_info->ls_addr;
      
      elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[0].size);
      elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[0].size);
      arg2 = (float *)((void *)arg1 + (elems*elem_sz));
      
      elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[1].size);
      elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[1].size);
      arg3 = (float *)((void *)arg2 + (elems*elem_sz));
      
      conv2d_leafext(arg2, arg3, arg1);
      //scalar_conv2d(arg2, arg3, arg1);
      task_info->state = EXECUTED;
      break;
    default:
      exit = 1;
    break;
  }

  return exit;
}

