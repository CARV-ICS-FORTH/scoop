/*
* Copyright (c) 2008, BSC (Barcelon Supercomputing Center)
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY BSC ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>

// #include "tpc_common.h"
// #include "tpc_ppe.h"
#include "kernels.h"

#define NB 64

void compute(struct timeval *start, struct timeval *stop, int DIM, float ***A)
{
  int i, j, k;
  tpc_reset_stats();
  float *arg1, *arg2, *arg3;
  int arg_size = NB*NB*sizeof(float);
  gettimeofday(start, NULL);
  for (j = 0; j < DIM; j++) 
  {
    for (k= 0; k< j; k++)
    {
      for (i = j+1; i < DIM; i++) 
      {
        arg1=A[i][j];
        arg2=A[i][k];
        arg3=A[j][k];
          //A[i,j] = A[i,j] - A[i,k] * (A[j,k]);
          #pragma tpc(arg1(inout, arg_size), arg2(in, arg_size), arg3(in, arg_size))
          spu_sgemm_tile( arg1, arg2, arg3 );
//           tpc_call( 1,3,
//                   (A[i][j]), NB*NB*sizeof(float), TPC_INOUT_ARG, 
//                   (A[i][k]), NB*NB*sizeof(float), TPC_IN_ARG, 
//                   (A[j][k]), NB*NB*sizeof(float), TPC_IN_ARG 
//                   );
      }  
    }
   tpc_wait_all();
    for (i = 0; i < j; i++)
    {
       arg1=A[j][j];
       arg2=A[j][i];
       //A[j,j] = A[j,j] - A[j,i] * (A[j,i]);
       #pragma tpc(arg1(inout, arg_size), arg2(in, arg_size))
       spu_ssyrk_tile(arg1,arg2);
//            tpc_call(2,2,
//                    A[j][j], NB*NB*sizeof(float), TPC_INOUT_ARG, 
//                    A[j][i], NB*NB*sizeof(float), TPC_IN_ARG 
//                    );
    }
    tpc_wait_all();
    // Cholesky Factorization of A[j,j]
    arg1=A[j][j];
    #pragma tpc(arg1(inout, arg_size))
    spu_spotrf_tile( arg1 );
//     tpc_call( 3,1,
//             A[j][j], NB*NB*sizeof(float), TPC_INOUT_ARG );
    tpc_wait_all(); 

    for (i = j+1; i < DIM; i++)
    {
       arg1=A[i][j];
       arg2=A[j][j];
       //A[i,j] <- A[i,j] = X * (A[j,j]);
       #pragma tpc(arg1(inout, arg_size), arg2(in, arg_size))
       spu_strsm_tile( arg1, arg2 );
//        tpc_call(4,2,
//                A[i][j], NB*NB*sizeof(float), TPC_INOUT_ARG,
//                A[j][j], NB*NB*sizeof(float), TPC_IN_ARG );
    }
    tpc_wait_all();
  }
  #pragma css finish 
  gettimeofday(stop, NULL);
}


float **A;

static void convert_to_blocks(int DIM, int N, float *Alin, float ***A)
{
int i, j;
  for (i = 0; i < N; i++)
  {
    for (j = 0; j < N; j++)
    {
      A[i/NB][j/NB][(i%NB)*NB+j%NB] = Alin[j*N+i];
    }
  }

}

void fill_random(float *Alin, int NN)
{
  srand(0);
  int i;
  for (i = 0; i < NN; i++)
  {
    Alin[i]=((float)rand())/((float)RAND_MAX);
  }
}

static void init(int argc, char **argv, int *N_p, int *DIM_p)
{
  int DIM;
  int i;
  
  if (argc==3)
  {
    DIM=atoi(argv[1]);
    tpc_init(atoi(argv[2]));
  }
  else
  {
    printf("usage: %s DIM #SPEs\n",argv[0]);
    exit(0);
  }

  // matrix init
  
  int N = NB*DIM;
  int NN = N * N;

  *N_p = N;
  *DIM_p = DIM;
  
  // linear matrix
  float *Alin = (float *) memalign(128,NN * sizeof(float));

  // fill the matrix with random values
  fill_random(Alin,NN);

  // make it positive definite
  for(i=0; i<N; i++)
  {
    Alin[i*N + i] += N;
  }
  
  // blocked matrix
  A = (float **)  memalign(128,DIM*DIM*sizeof(float *));
  for (i = 0; i < DIM*DIM; i++)
     A[i] = (float *)  memalign(128,NB*NB*sizeof(float));

  convert_to_blocks(DIM, N, Alin, (void *)A);
  
  free(Alin);
}

int main(int argc, char *argv[])
{
  // local vars
  int N, DIM;
  
  struct timeval start;
  struct timeval stop;
  unsigned long elapsed;

  // application inicializations
  init(argc, argv, &N, &DIM);
  
  // compute with CellSs
  compute(&start, &stop, DIM, (void *)A);
  
  tpc_print_stats(stdout);

  elapsed = 1000000 * (stop.tv_sec - start.tv_sec);
  elapsed += stop.tv_usec - start.tv_usec;
  printf("Performance: %d MFlops\n", (int)((0.33*N*N*N+0.5*N*N+0.17*N)/elapsed));
  printf ("Time: %lu useconds\n", elapsed);

  return 0;
}

