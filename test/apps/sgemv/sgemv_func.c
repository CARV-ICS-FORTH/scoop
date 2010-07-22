#include <assert.h>
#include <stdio.h>

#include <spu_intrinsics.h>
#include "tpc_common.h"
#include "tpc_spe.h"

#include "sgemv.h"

/* kayvonf: couldn't find an implementation of matrix vector
 * multiplication in the IBM headers so I had to implement it
 * myself.
 
 The vectorization and loop unrolling imposes the following
 constraints on this function:
 
 A is an MxN matrix, both M and N must be multiples of 4
*/
void sgemv_leaf(float *A, float *x, float *y, float *alpha)
{

    unsigned int i,j;

    unsigned int m = (unsigned int)alpha[1];
    unsigned int n = (unsigned int)alpha[2];
    unsigned int j_iters = n / 4;  
    vector float *yarray = (vector float*)y;
    vector float *xarray = (vector float*)x;

    vector float c2, c3, c4, c12, c34, result;
    vector float tmp_array0, tmp_array1, tmp_array2, tmp_array3;  


    for (i=0; i<m; i+=4) {

        vector float *A0array = (vector float *)&A[(i+0)*n];
          //(vector float*)(  ((unsigned char*)A->ptr) 
          //               + ((i+A->offset[0])*A->pitch[1] + A->offset[1])*A->elmtSize);
        vector float *A1array = (vector float *)&A[(i+1)*n];
          //(vector float*)(  ((unsigned char*)A->ptr) 
          //               + ((i+1+A->offset[0])*A->pitch[1] + A->offset[1])*A->elmtSize);
        vector float *A2array = (vector float *)&A[(i+2)*n];
          //(vector float*)(  ((unsigned char*)A->ptr) 
          //               + ((i+2+A->offset[0])*A->pitch[1] + A->offset[1])*A->elmtSize);
        vector float *A3array = (vector float *)&A[(i+3)*n];
          //(vector float*)(  ((unsigned char*)A->ptr) 
          //               + ((i+3+A->offset[0])*A->pitch[1] + A->offset[1])*A->elmtSize);
        
        tmp_array0 = spu_mul(A0array[0], xarray[0]);
        tmp_array1 = spu_mul(A1array[0], xarray[0]);
        tmp_array2 = spu_mul(A2array[0], xarray[0]);
        tmp_array3 = spu_mul(A3array[0], xarray[0]);
        
        for (j=1; j<j_iters; j++) {            
            tmp_array0 = spu_madd(A0array[j], xarray[j], tmp_array0);
            tmp_array1 = spu_madd(A1array[j], xarray[j], tmp_array1);
            tmp_array2 = spu_madd(A2array[j], xarray[j], tmp_array2);
            tmp_array3 = spu_madd(A3array[j], xarray[j], tmp_array3);
        }

        /* kayvonf: the following code sums the 4 elements in a vector
         * and adds the result into the output array (it's repeated 4
         * times for each of the output elements generated every outer
         * loop iteration.  We may need to interleave the 4
         * independent summations that are carried out here to avoid
         * instruction dependencies */

        vector float dotproduct={0.0, 0.0, 0.0, 0.0};
        vector float alphaarray = spu_splats(alpha[0]);
        
        c2 = spu_rlqwbyte(tmp_array0, 4);
        c3 = spu_rlqwbyte(tmp_array0, 8);
        c4 = spu_rlqwbyte(tmp_array0, 12);
        c12 = spu_add(tmp_array0, c2);
        c34 = spu_add(c3, c4);
        result = spu_add(c12, c34);
        dotproduct = spu_insert( spu_extract(result, 0), dotproduct, 0);

        c2 = spu_rlqwbyte(tmp_array1, 4);
        c3 = spu_rlqwbyte(tmp_array1, 8);
        c4 = spu_rlqwbyte(tmp_array1, 12);
        c12 = spu_add(tmp_array1, c2);
        c34 = spu_add(c3, c4);
        result = spu_add(c12, c34);
        dotproduct = spu_insert( spu_extract(result, 0), dotproduct, 1);

        c2 = spu_rlqwbyte(tmp_array2, 4);
        c3 = spu_rlqwbyte(tmp_array2, 8);
        c4 = spu_rlqwbyte(tmp_array2, 12);
        c12 = spu_add(tmp_array2, c2);
        c34 = spu_add(c3, c4);
        result = spu_add(c12, c34);
        dotproduct = spu_insert( spu_extract(result, 0), dotproduct, 2);

        c2 = spu_rlqwbyte(tmp_array3, 4);
        c3 = spu_rlqwbyte(tmp_array3, 8);
        c4 = spu_rlqwbyte(tmp_array3, 12);
        c12 = spu_add(tmp_array3, c2);
        c34 = spu_add(c3, c4);
        result = spu_add(c12, c34);
        dotproduct = spu_insert( spu_extract(result, 0), dotproduct, 3);

        
        yarray[(i>>2)] = spu_add( yarray[(i>>2)], spu_mul(dotproduct, alphaarray)  );
        
    }


}



void sgemv_leaf_generic(float *A, float *x, float *y, float *alpha)
{
    int i,j;
    float tmp, al;
    int m, n;
    
    al = alpha[0];
    m = alpha[1];
    n = alpha[2];


    for (i=0; i<m; i++) {
        tmp = 0.0f;
        for (j=0; j<n; j++) {
            tmp += A[i*n+j] * x[j];
        }
        y[i] = al*tmp + y[i];
    }
}




int execute_task(queue_entry_t *ex_task,
    tpc_spe_task_state_t *task_info)
{
  int exit = 0;
  float *arg1, *arg2, *arg3, *arg4;

  switch(ex_task->funcid)
  {
    case 0:
      arg1 = (float *)task_info->ls_addr;
      arg2 = (float *)((void *)arg1 + ex_task->arguments[0].size);
      arg3 = (float *)((void *)arg2 + ex_task->arguments[1].size);
      arg4 = (float *)((void *)arg3 + ex_task->arguments[2].size);
      sgemv_leaf(arg2, arg3, arg1, arg4);
      //sgemv_leaf_generic(arg2, arg3, arg1, arg4);
      task_info->state = EXECUTED;
      break;
    default:
      exit = 1;
    break;
  }

  return exit;
}

