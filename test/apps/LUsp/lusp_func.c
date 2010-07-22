#include <stdio.h>
#include <spu_intrinsics.h>
#include "tpc_common.h"
#include "tpc_spe.h"
#include "lusp.h"



void spe_daxpy_vec(float *a, float *b, int n, float alpha);


// a: IN/OUT
// diag: IN
void spe_bdiv(float *a, float *diag, struct bdiv_args *args)
{
  int stride_a=args->stride_a;
  int stride_diag=args->stride_diag;
  int dimi=args->dimi;
  int dimk=args->dimk;

  int j; 
  int k;
  float alpha;


  for (k=0; k<dimk; k++) {
    for (j=k+1; j<dimk; j++) {
      alpha = -diag[k+j*stride_diag];
      spe_daxpy_vec(&a[j*stride_a], &a[k*stride_a], dimi, alpha);
      //spe_daxpy(&a[j*stride_a], &a[k*stride_a], dimi, alpha);
    }
  }
}




// c: IN/OUT
// a: IN
void spe_bmodd(float *c, float *a, struct bmodd_args *args)
{
  int dimi=args->dimi;
  int dimj=args->dimj;
  int stride_a=args->stride_a;
  int stride_c=args->stride_c;

  //int i; 
  int j; 
  int k; 
  int length;
  float alpha;

  for (k=0; k<dimi; k++) {
    for (j=0; j<dimj; j++) {
      c[k+j*stride_c] /= a[k+k*stride_a];
      alpha = -c[k+j*stride_c];
      length = dimi - k - 1;
      spe_daxpy(&c[k+1+j*stride_c], &a[k+1+k*stride_a], dimi-k-1, alpha);
      //spe_daxpy_vec(&c[k+1+j*stride_c], &a[k+1+k*stride_a], dimi-k-1, alpha);
    }
  }
}




// c: IN/OUT
// a: IN
// b: IN
void spe_bmod(float *c, float *a, float *b, struct bmod_args *args)
{
  int dimi=args->dimi;
  int dimj=args->dimj;
  int dimk=args->dimk;
  int stridea=args->stridea;
  int strideb=args->strideb;
  int stridec=args->stridec;

  //int i; 
  int j; 
  int k;
  float alpha;

  for (k=0; k<dimk; k++) {
    for (j=0; j<dimj; j++) {
      alpha = -b[k+j*strideb]; 
      spe_daxpy_vec(&c[j*stridec], &a[k*stridea], dimi, alpha);
      //spe_daxpy(&c[j*stridec], &a[k*stridea], dimi, alpha);
    }
  }
}





void spe_daxpy(float *a, float *b, int n, float alpha)
{
  int i;
  // /*if(n!=16)*/ printf("n=%d \n", n);
  
  for (i=0; i<n; i++) {
    a[i] += alpha*b[i];
  }
}





// bmod() and bdiv() alwayes call thit function with n at least 8 which is the
// least reasonable block dimension.
void spe_daxpy_vec(float *a, float *b, int n, float alpha)
{
  int i=0;
  vector float *va = (vector float *)a;
  vector float *vb = (vector float *)b;
  vector float valpha = {alpha, alpha, alpha, alpha};
  vector float vtmp0, vtmp1, vtmp2, vtmp3;

  int i_iters=n>>2;
  //if(n!=16) printf("n=%d (%p %p %p %p) \n", n, &va[0], &va[1], &va[2], &va[3]);

  for (i=0; i<i_iters; i+=4) {
    /*va[i+0] += (valpha) * (vb[i+0]);
    va[i+1] += (valpha) * (vb[i+1]);
    va[i+2] += (valpha) * (vb[i+2]);
    va[i+3] += (valpha) * (vb[i+3]);*/
    vtmp0 = va[i+0] + (valpha * vb[i+0]);
    vtmp1 = va[i+1] + (valpha * vb[i+1]);
    vtmp2 = va[i+2] + (valpha * vb[i+2]);
    vtmp3 = va[i+3] + (valpha * vb[i+3]);

    va[i+0] = vtmp0;
    va[i+1] = vtmp1;
    va[i+2] = vtmp2;
    va[i+3] = vtmp3;

  }

}




int execute_task(volatile struct queue_entry_t *ex_task,
    struct tpc_spe_task_state_t *task_info)
{
  int exit = 0;
  float *arg1, *arg2, *arg3;
  struct bmod_args *arg4;
  struct bdiv_args *arg5;
  struct bmodd_args *arg6;

  switch(ex_task->funcid)
  {
    case 0:
      arg1 = (float *)task_info->ls_addr;
      arg2 = (float *)((void *)arg1 + ex_task->arguments[0].size);
      arg5 = (struct bdiv_args *)((void *)arg2 + ex_task->arguments[1].size);
      spe_bdiv(arg1, arg2, arg5);
      task_info->state = EXECUTED;
      break;
    case 1:
      arg1 = (float *)task_info->ls_addr;
      arg2 = (float *)((void *)arg1 + ex_task->arguments[0].size);
      arg6 = (struct bmodd_args *)((void *)arg2 + ex_task->arguments[1].size);
      spe_bmodd(arg1, arg2, arg6);
      task_info->state = EXECUTED;
      break;
    case 2:
      arg1 = (float *)task_info->ls_addr;
      arg2 = (float *)((void *)arg1 + ex_task->arguments[0].size);
      arg3 = (float *)((void *)arg2 + ex_task->arguments[1].size);
      arg4 = (struct bmod_args *)((void *)arg3 + ex_task->arguments[2].size);
      spe_bmod(arg1, arg2, arg3, arg4);
      task_info->state = EXECUTED;
      break;
    default:
      exit = 1;
    break;
  }

  return exit;
}

