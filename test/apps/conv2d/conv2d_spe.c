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

