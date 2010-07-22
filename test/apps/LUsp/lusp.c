
/*************************************************************************/
/*                                                                       */
/*  Copyright (c) 1994 Stanford University                               */
/*                                                                       */
/*  All rights reserved.                                                 */
/*                                                                       */
/*  Permission is given to use, copy, and modify this software for any   */
/*  non-commercial purpose as long as this copyright notice is not       */
/*  removed.  All other uses, including redistribution in whole or in    */
/*  part, are forbidden without prior written permission.                */
/*                                                                       */
/*  This software is provided with absolutely no warranty and no         */
/*  support.                                                             */
/*                                                                       */
/*************************************************************************/

/*************************************************************************/
/*                                                                       */
/*  Parallel dense blocked LU factorization (no pivoting)                */
/*                                                                       */
/*  This version contains two dimensional arrays in which the first      */
/*  dimension is the block to be operated on, and the second contains    */
/*  all data points in that block.  In this manner, all data points in   */
/*  a block (which are operated on by the same processor) are allocated  */
/*  contiguously and locally, and false sharing is eliminated.           */
/*                                                                       */
/*  Command line options:                                                */
/*                                                                       */
/*  -nN : Decompose NxN matrix.                                          */
/*  -pP : P = number of processors.                                      */
/*  -bB : Use a block size of B. BxB elements should fit in cache for    */
/*        good performance. Small block sizes (B=8, B=16) work well.     */
/*  -s  : Print individual processor timing statistics.                  */
/*  -t  : Test output.                                                   */
/*  -o  : Print out matrix values.                                       */
/*  -h  : Print out command line options.                                */
/*                                                                       */
/*  Note: This version works under both the FORK and SPROC models        */
/*                                                                       */
/*************************************************************************/

#include <stdio.h>
#include <math.h>
#include <sys/time.h>
#include <stdlib.h>
#include <assert.h>
#include <getopt.h>
#include <ppu_intrinsics.h>

#include "tpc_common.h"
#include "tpc_ppe.h"
#include "lusp.h"

#include <stdlib.h>
#include <time.h>

#define PAGE_SIZE                        4096
#define MAXRAND                         32767.0
#define DEFAULT_N                         512
#define DEFAULT_P                           1
#define DEFAULT_ACCEL                       1
#define DEFAULT_B                          16
#define min(a,b) ((a) < (b) ? (a) : (b))

static __inline__ double lu_get_time(int x)
{
  return (double)__mftb();
    /*struct timeval tp;
    int rtn;
    rtn=gettimeofday(&tp, NULL);

    return ((double)tp.tv_sec+(1.e-6)*tp.tv_usec);*/

/*	struct tms tbuf;
	return (double)times(&tbuf) / (double)sysconf(_SC_CLK_TCK);
*/
}

struct GlobalMemory {
  double *t_in_fac;   
  double *t_in_solve;
  double *t_in_mod; 
  double *t_in_bar;
  double *completion;
  double starttime; 
  double rf; 
  double rs; 
  double done;
  int id;
  int (start);
  int (idlock);
} *Global;

struct LocalCopies {
  double t_in_fac;
  double t_in_solve;
  double t_in_mod;
  double t_in_bar;
};

int n = DEFAULT_N;          /* The size of the matrix */
int P = DEFAULT_P;          /* Number of processors */
int ACCEL = DEFAULT_ACCEL;  /* Number of TPC accelerators */
int block_size = DEFAULT_B; /* Block dimension */
int nblocks;                /* Number of blocks in each dimension */
int num_rows;               /* Number of processors per row of processor grid */
int num_cols;               /* Number of processors per col of processor grid */
float **a;                 /* a = lu; l and u both placed back in a */
float *rhs;
int *proc_bytes;            /* Bytes to malloc per processor to hold blocks 
			       of A*/
float **last_malloc;       /* Starting point of last block of A */

int test_result = 0;        /* Test result of factorization? */
int doprint = 0;            /* Print out matrix values? */
int dostats = 0;            /* Print out individual processor statistics? */

void SlaveStart();
void OneSolve(int, int, float **, int, int);
void lu0(float *,int, int);
void bdiv(float *, float *, int, int, int, int);
void bmodd(float *, float*, int, int, int, int);
void bmod(float *, float *, float *, int, int, int, int, int, int);
void daxpy(float *, float *, int, float);
int BlockOwner(int, int);
void lu(int, int, int, struct LocalCopies *, int);
void InitA(float *);
float TouchA(int, int);
void PrintA();
void CheckResult(int, float **, float *);
void printerr(char *);


int main(argc, argv)

int argc;
char *argv[];

{
  int i, j;
  int ch;
  extern char *optarg;
  int MyNum=0;
  double mint=0.0, maxt=0.0, avgt=0.0;
  double min_fac=0.0, min_solve=0.0, min_mod=0.0, min_bar=0.0;
  double max_fac=0.0, max_solve=0.0, max_mod=0.0, max_bar=0.0;
  double avg_fac=0.0, avg_solve=0.0, avg_mod=0.0, avg_bar=0.0;
  int proc_num;
  int edge;
  int size;
  double start;

  {(start) = lu_get_time(0);}

  while ((ch = getopt(argc, argv, "n:p:a:b:cstoh")) != -1) {
    switch(ch) {
    case 'n': n = atoi(optarg); break;
    case 'p': P = atoi(optarg); break;
    case 'a': ACCEL = atoi(optarg); break;
    case 'b': block_size = atoi(optarg); break;
    case 's': dostats = 1; break;
    case 't': test_result = !test_result; break;
    case 'o': doprint = !doprint; break;
    case 'h': printf("Usage: LU <options>\n\n");
	      printf("options:\n");
              printf("  -nN : Decompose NxN matrix.\n");
              printf("  -pP : P = number of processors.\n");
              printf("  -aA : A = number of accelerators.\n");
              printf("  -bB : Use a block size of B. BxB elements should fit in cache for \n");
              printf("        good performance. Small block sizes (B=8, B=16) work well.\n");
              printf("  -c  : Copy non-locally allocated blocks to local memory before use.\n");
              printf("  -s  : Print individual processor timing statistics.\n");
              printf("  -t  : Test output.\n");
              printf("  -o  : Print out matrix values.\n");
              printf("  -h  : Print out command line options.\n\n");
              printf("Default: LU -n%1d -p%1d -b%1d\n",
		     DEFAULT_N,DEFAULT_P,DEFAULT_B);
              exit(0);
              break;
    }
  }

  {;}
  /*if(ACCEL<1 || ACCEL>6) {
    fprintf(stderr, "%d accelerators are not possible\n", ACCEL);
    exit(1);
  }*/
  tpc_init(ACCEL);

  printf("\n");
  printf("Blocked Dense LU Factorization\n");
  printf("     %d by %d Matrix\n",n,n);
  printf("     %d Processors / %d Accelerators\n",P, ACCEL);
  printf("     %d by %d Element Blocks\n",block_size,block_size);
  printf("\n");
  printf("\n");

  num_rows = (int) sqrt((float) P);
  for (;;) {
    num_cols = P/num_rows;
    if (num_rows*num_cols == P)
      break;
    num_rows--;
  }
  nblocks = n/block_size;
  if (block_size * nblocks != n) {
    nblocks++;
  }

  edge = n%block_size;
  if (edge == 0) {
    edge = block_size;
  }
  proc_bytes = (int *) tpc_malloc(P*sizeof(int));
  last_malloc = (float **) tpc_malloc(P*sizeof(float *));;
  for (i=0;i<P;i++) {
    proc_bytes[i] = 0;
    last_malloc[i] = NULL;
  }
  for (i=0;i<nblocks;i++) {
    for (j=0;j<nblocks;j++) {
      proc_num = BlockOwner(i,j);
      if ((i == nblocks-1) && (j == nblocks-1)) {
        size = edge*edge;
      } else if ((i == nblocks-1) || (j == nblocks-1)) {
        size = edge*block_size;
      } else {
        size = block_size*block_size;
      }
      proc_bytes[proc_num] += size*sizeof(float);
    }
  }
  for (i=0;i<P;i++) {
    last_malloc[i] = (float *) tpc_malloc(proc_bytes[i] + PAGE_SIZE);
    if (last_malloc[i] == NULL) {
      fprintf(stderr,"Could not malloc memory blocks for proc %d\n",i);
      exit(-1);
    } 
    last_malloc[i] = (float *) (((unsigned) last_malloc[i]) + PAGE_SIZE -
                     ((unsigned) last_malloc[i]) % PAGE_SIZE);

/* Note that this causes all blocks to start out page-aligned, and that
   for block sizes that exceed cache line size, blocks start at cache-line
   aligned addresses as well.  This reduces false sharing */

  }
  a = (float **) tpc_malloc(nblocks*nblocks*sizeof(float *));;
  if (a == NULL) {
    printerr("Could not malloc memory for a\n");
    exit(-1);
  } 
  for (i=0;i<nblocks;i++) {
    for (j=0;j<nblocks;j++) {
      proc_num = BlockOwner(i,j);
      a[i+j*nblocks] = last_malloc[proc_num];
      if ((i == nblocks-1) && (j == nblocks-1)) {
        size = edge*edge;
      } else if ((i == nblocks-1) || (j == nblocks-1)) {
        size = edge*block_size;
      } else {
        size = block_size*block_size;
      }
      last_malloc[proc_num] += size;
    }
  }

  rhs = (float *) tpc_malloc(n*sizeof(float));;
  if (rhs == NULL) {
    printerr("Could not malloc memory for rhs\n");
    exit(-1);
  } 

  Global = (struct GlobalMemory *) tpc_malloc(sizeof(struct GlobalMemory));;
  Global->t_in_fac = (double *) tpc_malloc(P*sizeof(double));;
  Global->t_in_mod = (double *) tpc_malloc(P*sizeof(double));;
  Global->t_in_solve = (double *) tpc_malloc(P*sizeof(double));;
  Global->t_in_bar = (double *) tpc_malloc(P*sizeof(double));;
  Global->completion = (double *) tpc_malloc(P*sizeof(double));;

  if (Global == NULL) {
    printerr("Could not malloc memory for Global\n");
    exit(-1);
  } else if (Global->t_in_fac == NULL) {
    printerr("Could not malloc memory for Global->t_in_fac\n");
    exit(-1);
  } else if (Global->t_in_mod == NULL) {
    printerr("Could not malloc memory for Global->t_in_mod\n");
    exit(-1);
  } else if (Global->t_in_solve == NULL) {
    printerr("Could not malloc memory for Global->t_in_solve\n");
    exit(-1);
  } else if (Global->t_in_bar == NULL) {
    printerr("Could not malloc memory for Global->t_in_bar\n");
    exit(-1);
  } else if (Global->completion == NULL) {
    printerr("Could not malloc memory for Global->completion\n");
    exit(-1);
  }

/* POSSIBLE ENHANCEMENT:  Here is where one might distribute the a[i]
   blocks across physically distributed memories as desired.

   One way to do this is as follows:

   for (i=0;i<nblocks;i++) {
     for (j=0;j<nblocks;j++) {
       proc_num = BlockOwner(i,j);
       if ((i == nblocks-1) && (j == nblocks-1)) {
         size = edge*edge;
       } else if ((i == nblocks-1) || (j == nblocks-1)) {
         size = edge*block_size;
       } else {
         size = block_size*block_size;
       }

       Place all addresses x such that 
       (&(a[i+j*nblocks][0]) <= x < &(a[i+j*nblocks][size-1])) 
       on node proc_num
     }
   }
*/

  {;};
  {;};
  Global->id = 0;
  for (i=1; i<P; i++) {
    {fprintf(stderr, "No more processors -- this is a uniprocessor version!\n"); exit(-1);};
  }

  InitA(rhs);
  if (doprint) {
    printf("Matrix before decomposition:\n");
    PrintA();
  }

  SlaveStart(MyNum);

  {;}

  if (doprint) {
    printf("\nMatrix after decomposition:\n");
    PrintA();
  }

  if (dostats) {
    maxt = avgt = mint = Global->completion[0];
    for (i=1; i<P; i++) {
      if (Global->completion[i] > maxt) {
        maxt = Global->completion[i];
      }
      if (Global->completion[i] < mint) {
        mint = Global->completion[i];
      }
      avgt += Global->completion[i];
    }
    avgt = avgt / P;
  
    min_fac = max_fac = avg_fac = Global->t_in_fac[0];
    min_solve = max_solve = avg_solve = Global->t_in_solve[0];
    min_mod = max_mod = avg_mod = Global->t_in_mod[0];
    min_bar = max_bar = avg_bar = Global->t_in_bar[0];
  
    for (i=1; i<P; i++) {
      if (Global->t_in_fac[i] > max_fac) {
        max_fac = Global->t_in_fac[i];
      }
      if (Global->t_in_fac[i] < min_fac) {
        min_fac = Global->t_in_fac[i];
      }
      if (Global->t_in_solve[i] > max_solve) {
        max_solve = Global->t_in_solve[i];
      }
      if (Global->t_in_solve[i] < min_solve) {
        min_solve = Global->t_in_solve[i];
      }
      if (Global->t_in_mod[i] > max_mod) {
        max_mod = Global->t_in_mod[i];
      }
      if (Global->t_in_mod[i] < min_mod) {
        min_mod = Global->t_in_mod[i];
      }
      if (Global->t_in_bar[i] > max_bar) {
        max_bar = Global->t_in_bar[i];
      }
      if (Global->t_in_bar[i] < min_bar) {
        min_bar = Global->t_in_bar[i];
      }
      avg_fac += Global->t_in_fac[i];
      avg_solve += Global->t_in_solve[i];
      avg_mod += Global->t_in_mod[i];
      avg_bar += Global->t_in_bar[i];
    }
    avg_fac = avg_fac/P;
    avg_solve = avg_solve/P;
    avg_mod = avg_mod/P;
    avg_bar = avg_bar/P;
  }
  printf("                            PROCESS STATISTICS\n");
  printf("              Total      Diagonal     Perimeter      Interior       Barrier\n");
  printf(" Proc         Time         Time         Time           Time          Time\n");
  printf("    0    %10lf    %10lf    %10lf    %10lf    %10lf\n",
          Global->completion[0],Global->t_in_fac[0],
          Global->t_in_solve[0],Global->t_in_mod[0],
          Global->t_in_bar[0]);
  if (dostats) {
    for (i=1; i<P; i++) {
      printf("  %3d    %10.0lf    %10.0lf    %10.0lf    %10.0lf    %10.0lf\n",
              i,Global->completion[i],Global->t_in_fac[i],
	      Global->t_in_solve[i],Global->t_in_mod[i],
	      Global->t_in_bar[i]);
    }
    printf("  Avg    %10.0lf    %10.0lf    %10.0lf    %10.0lf    %10.0lf\n",
           avgt,avg_fac,avg_solve,avg_mod,avg_bar);
    printf("  Min    %10.0lf    %10.0lf    %10.0lf    %10.0lf    %10.0lf\n",
           mint,min_fac,min_solve,min_mod,min_bar);
    printf("  Max    %10.0lf    %10.0lf    %10.0lf    %10.0lf    %10.0lf\n",
           maxt,max_fac,max_solve,max_mod,max_bar);
  }
  printf("\n");
  Global->starttime = start;
  printf("                            TIMING INFORMATION\n");
  printf("Start time                        : %16f\n",
          Global->starttime);
  printf("Initialization finish time        : %16f\n",
          Global->rs);
  printf("Overall finish time               : %16f\n",
          Global->rf);
  printf("Total time with initialization    : %16f\n",
          Global->rf-Global->starttime);
  printf("Total time without initialization : %16f\n",
          Global->rf-Global->rs);
  printf("\n");

  if (test_result) {
    printf("                             TESTING RESULTS\n");
    CheckResult(n, a, rhs);
  }

  tpc_print_stats(stdout);
  tpc_shutdown();
  {exit(0);};
  return 0;
}


void SlaveStart()

{
  int MyNum;

  {;}
    MyNum = Global->id;
    Global->id ++;
  {;}

/* POSSIBLE ENHANCEMENT:  Here is where one might pin processes to
   processors to avoid migration */

  OneSolve(n, block_size, a, MyNum, dostats);
}


void OneSolve(n, block_size, a, MyNum, dostats)

float **a;
int n;
int block_size;
int MyNum;
int dostats;

{
  double myrs; 
  double myrf; 
  double mydone;
  struct LocalCopies *lc;

  lc = (struct LocalCopies *) tpc_malloc(sizeof(struct LocalCopies));
  if (lc == NULL) {
    fprintf(stderr,"Proc %d could not malloc memory for lc\n",MyNum);
    exit(-1);
  }
  lc->t_in_fac = 0.0;
  lc->t_in_solve = 0.0;
  lc->t_in_mod = 0.0;
  lc->t_in_bar = 0.0;

  /* barrier to ensure all initialization is done */
  {;};  // BARRIER

  /* to remove cold-start misses, all processors touch their own data */
  TouchA(block_size, MyNum);

  {;};  // BARRIER

/* POSSIBLE ENHANCEMENT:  Here is where one might reset the
   statistics that one is measuring about the parallel execution */

  if ((MyNum == 0) || (dostats)) {
    {(myrs) = lu_get_time(0);};
  }

  lu(n, block_size, MyNum, lc, dostats);

  if ((MyNum == 0) || (dostats)) {
    { (mydone) = lu_get_time(0);};
  }

  {;};  // BARRIER

  if ((MyNum == 0) || (dostats)) {
    Global->t_in_fac[MyNum] = lc->t_in_fac;
    Global->t_in_solve[MyNum] = lc->t_in_solve;
    Global->t_in_mod[MyNum] = lc->t_in_mod;
    Global->t_in_bar[MyNum] = lc->t_in_bar;
    Global->completion[MyNum] = mydone-myrs;
  }
  if (MyNum == 0) {
    { (myrf) = lu_get_time(0);};
    Global->rs = myrs;
    Global->done = mydone;
    Global->rf = myrf;
  }
}


void lu0(a, n, stride)

float *a;
int n; 
int stride;

{
  int j; 
  int k; 
  int length;
  float alpha;

  for (k=0; k<n; k++) {
    /* modify subsequent columns */
    for (j=k+1; j<n; j++) {
      a[k+j*stride] /= a[k+k*stride];
      alpha = -a[k+j*stride];
      length = n-k-1;
      daxpy(&a[k+1+j*stride], &a[k+1+k*stride], n-k-1, alpha);
    }
  }
}


void bdiv(a, diag, stride_a, stride_diag, dimi, dimk)

float *a; 
float *diag;
int stride_a; 
int stride_diag; 
int dimi; 
int dimk;

{
  int j; 
  int k;
  float alpha;

  //printf("--------------- bdiv (%d,%d,%d,%d) -----------------\n", stride_a, stride_diag, dimi, dimk);

  for (k=0; k<dimk; k++) {
    for (j=k+1; j<dimk; j++) {
      alpha = -diag[k+j*stride_diag];
      daxpy(&a[j*stride_a], &a[k*stride_a], dimi, alpha);
      //printf("%10p %10p %10p\n", diag[k+j*stride_diag], &a[j*stride_a], &a[k*stride_a]);
      //printf("%10d %10d %10d\n", k+j*stride_diag, j*stride_a, k*stride_a);
    }
  }
}


void bmodd(a, c, dimi, dimj, stride_a, stride_c)

float *a; 
float *c;
int dimi; 
int dimj; 
int stride_a; 
int stride_c;

{
  int j; 
  int k; 
  int length;
  float alpha;

  for (k=0; k<dimi; k++) {
    for (j=0; j<dimj; j++) {
      c[k+j*stride_c] /= a[k+k*stride_a];
      alpha = -c[k+j*stride_c];
      length = dimi - k - 1;
      daxpy(&c[k+1+j*stride_c], &a[k+1+k*stride_a], dimi-k-1, alpha);
    }
  }
}


void bmod(a, b, c, dimi, dimj, dimk, stridea, strideb, stridec)

float *a; 
float *b; 
float *c;
int dimi; 
int dimj; 
int dimk; 
int stridea;
int strideb;
int stridec;

{
  int j; 
  int k;
  float alpha;

  for (k=0; k<dimk; k++) {
    for (j=0; j<dimj; j++) {
      alpha = -b[k+j*strideb]; 
      daxpy(&c[j*stridec], &a[k*stridea], dimi, alpha);
    }
  }
}


void daxpy(a, b, n, alpha)

float *a; 
float *b; 
float alpha;
int n;

{
  int i;

  for (i=0; i<n; i++) {
    a[i] += alpha*b[i];
  }
}


int BlockOwner(I, J)

int I; 
int J;

{
  return((J%num_cols) + (I%num_rows)*num_cols); 
}


void lu(n, bs, MyNum, lc, dostats)

int n;
int bs;
int MyNum;
struct LocalCopies *lc;
int dostats;

{
  int i, il, j, jl, k, kl;
  int I, J, K;
  float *A, *B, *C, *D;
  int strI, strJ, strK;
  double t1=0.0, t2=0.0, t3=0.0, t4=0.0, t11=0.0, t22=0.0;
  int diagowner;
  int colowner;

  struct bmod_args *bmodargs;
  struct bmodd_args *bmoddargs;
  struct bdiv_args *bdivargs;
 
  int argsize = bs*bs*sizeof(float);
  //printf("argsize=%d\n", argsize);

  bmodargs = tpc_malloc(sizeof(struct bmod_args));
  bmoddargs = tpc_malloc(sizeof(struct bmodd_args));
  bdivargs = tpc_malloc(sizeof(struct bdiv_args));
  assert(bmodargs!=NULL && bmodargs!=NULL && bmodargs!=NULL);

  for (k=0, K=0; k<n; k+=bs, K++) {
    kl = k + bs; 
    if (kl > n) {
      kl = n;
      strK = kl - k;
    } else {
      strK = bs;
    }

    if ((MyNum == 0) || (dostats)) {
      {(t1) = lu_get_time(0);};
    }

    /* factor diagonal block */
    diagowner = BlockOwner(K, K);
    if (diagowner == MyNum) {
      A = a[K+K*nblocks];
      lu0(A, strK, strK);
    }

    if ((MyNum == 0) || (dostats)) {
      {(t11) = lu_get_time(0);};
    }

    //tpc_wait_all();
    {;};  // BARRIER ==========================================================

    if ((MyNum == 0) || (dostats)) {
      {(t2) = lu_get_time(0);};
    }

    /* divide column k by diagonal block */
    D = a[K+K*nblocks];
    for (i=kl, I=K+1; i<n; i+=bs, I++) {
      if (BlockOwner(I, K) == MyNum) {  /* parcel out blocks */
	il = i + bs; 
	if (il > n) {
	  il = n;
          strI = il - i;
        } else {
          strI = bs;
        }
	A = a[I+K*nblocks];
	
	bdivargs->stride_a = strI;
	bdivargs->stride_diag = strK;
	bdivargs->dimi = strI;
	bdivargs->dimk = strK;
	//bdiv(A, D, strI, strK, strI, strK);
	tpc_call(0, 3,
	    A, argsize, TPC_INOUT_ARG,
	    D, argsize, TPC_IN_ARG,
	    bdivargs, sizeof(struct bdiv_args), TPC_IN_ARG);
      }
    }

    /* modify row k by diagonal block */
    for (j=kl, J=K+1; j<n; j+=bs, J++) {
      if (BlockOwner(K, J) == MyNum) {  /* parcel out blocks */
	jl = j+bs; 
	if (jl > n) {
	  jl = n;
          strJ = jl - j;
        } else {
          strJ = bs;
        }
        A = a[K+J*nblocks];

	bmoddargs->dimi = strK;
	bmoddargs->dimj = strJ;
	bmoddargs->stride_a = strK;
	bmoddargs->stride_c = strK;
	//bmodd(D, A, strK, strJ, strK, strK);
	tpc_call(1, 3,
	    A, argsize, TPC_INOUT_ARG,
	    D, argsize, TPC_IN_ARG,
	    bmoddargs, sizeof(struct bmodd_args), TPC_IN_ARG);
      }
    }

    if ((MyNum == 0) || (dostats)) {
      {(t22) = lu_get_time(0);};
    }   

    tpc_wait_all();
    {;};  // BARRIER ==========================================================

    if ((MyNum == 0) || (dostats)) {
      {(t3) = lu_get_time(0);};
    }

    /* modify subsequent block columns */
    for (i=kl, I=K+1; i<n; i+=bs, I++) {
      il = i+bs; 
      if (il > n) {
	il = n;
        strI = il - i;
      } else {
        strI = bs;
      }
      colowner = BlockOwner(I,K);
      A = a[I+K*nblocks]; 
      for (j=kl, J=K+1; j<n; j+=bs, J++) {
	jl = j + bs; 
	if (jl > n) {
	  jl = n;
          strJ= jl - j;
        } else {
          strJ = bs;
        }
	if (BlockOwner(I, J) == MyNum) {  /* parcel out blocks */
	  B = a[K+J*nblocks]; 
	  C = a[I+J*nblocks];

	  bmodargs->dimi = strI;
	  bmodargs->dimj = strJ;
	  bmodargs->dimk = strK;
	  bmodargs->stridea = strI;
	  bmodargs->strideb = strK;
	  bmodargs->stridec = strI;

	  //bmod(A, B, C, strI, strJ, strK, strI, strK, strI);
	  tpc_call(2, 4,
	      C, argsize, TPC_INOUT_ARG,
	      A, argsize, TPC_IN_ARG,
	      B, argsize, TPC_IN_ARG,
	      bmodargs, sizeof(struct bmod_args), TPC_IN_ARG );
	}
      }
    }
    
    tpc_wait_all();
    
    if ((MyNum == 0) || (dostats)) {
      {(t4) = lu_get_time(0);};
      lc->t_in_fac += (t11-t1);
      lc->t_in_solve += (t22-t2);
      lc->t_in_mod += (t4-t3);
      lc->t_in_bar += (t2-t11) + (t3-t22);
    }
  }
}


void InitA(rhs)

float *rhs;

{
  int i, j;
  int ii, jj;
  int edge;
  int ibs;
  int jbs, skip;

  srand48((long) 1);
  edge = n%block_size;
  for (j=0; j<n; j++) {
    for (i=0; i<n; i++) {
      if ((n - i) <= edge) {
	ibs = edge;
	ibs = n-edge;
	skip = edge;
      } else {
	ibs = block_size;
	skip = block_size;
      }
      if ((n - j) <= edge) {
	jbs = edge;
	jbs = n-edge;
      } else {
	jbs = block_size;
      }
      ii = (i/block_size) + (j/block_size)*nblocks;
      jj = (i%ibs)+(j%jbs)*skip;
      a[ii][jj] = ((float) lrand48())/MAXRAND;
      if (i == j) {
	a[ii][jj] *= 10;
      }
    }
  }

  for (j=0; j<n; j++) {
    rhs[j] = 0.0;
  }
  for (j=0; j<n; j++) {
    for (i=0; i<n; i++) {
      if ((n - i) <= edge) {
	ibs = edge;
	ibs = n-edge;
	skip = edge;
      } else {
	ibs = block_size;
	skip = block_size;
      }
      if ((n - j) <= edge) {
	jbs = edge;
	jbs = n-edge;
      } else {
	jbs = block_size;
      }
      ii = (i/block_size) + (j/block_size)*nblocks;
      jj = (i%ibs)+(j%jbs)*skip;
      rhs[i] += a[ii][jj];
    }
  }
}


float TouchA(bs, MyNum)

int bs; 
int MyNum;

{
  int i, j, I, J;
  float tot = 0.0;
  int ibs;
  int jbs;

  /* touch my portion of A[] */

  for (J=0; J<nblocks; J++) {
    for (I=0; I<nblocks; I++) {
      if (BlockOwner(I, J) == MyNum) {
	if (J == nblocks-1) {
	  jbs = n%bs;
	  if (jbs == 0) {
	    jbs = bs;
          }
	} else {
	  jbs = bs;
	}
	if (I == nblocks-1) {
	  ibs = n%bs;
	  if (ibs == 0) {
	    ibs = bs;
          }
	} else {
	  ibs = bs;
	}
	for (j=0; j<jbs; j++) {
	  for (i=0; i<ibs; i++) {
	    tot += a[I+J*nblocks][i+j*ibs];
          }
	}
      }
    }
  } 
  return(tot);
}


void PrintA()

{
  int i, j;
  int ii, jj;
  int edge;
  int ibs, jbs, skip;

  edge = n%block_size;
  for (i=0; i<n; i++) {
    for (j=0; j<n; j++) {
      if ((n - i) <= edge) {
	ibs = edge;
	ibs = n-edge;
        skip = edge;
      } else {
	ibs = block_size;
        skip = block_size;
      }
      if ((n - j) <= edge) {
	jbs = edge;
	jbs = n-edge;
      } else {
	jbs = block_size;
      }
      ii = (i/block_size) + (j/block_size)*nblocks;
      jj = (i%ibs)+(j%jbs)*skip;
      printf("%8.1f ", a[ii][jj]);   
    }
    printf("\n");
  }
  fflush(stdout);
}


void CheckResult(n, a, rhs)

int n;
float **a; 
float *rhs;

{
  int i, j, bogus = 0;
  float *y, diff, max_diff;
  int ii, jj;
  int edge;
  int ibs, jbs, skip;

  edge = n%block_size;
  y = (float *) tpc_malloc(n*sizeof(float));  
  if (y == NULL) {
    printerr("Could not malloc memory for y\n");
    exit(-1);
  }
  for (j=0; j<n; j++) {
    y[j] = rhs[j];
  }
  for (j=0; j<n; j++) {
    if ((n - j) <= edge) {
      jbs = edge;
      jbs = n-edge;
      skip = edge;
    } else {
      jbs = block_size;
      skip = block_size;
    }
    ii = (j/block_size) + (j/block_size)*nblocks;
    jj = (j%jbs)+(j%jbs)*skip;

    y[j] = y[j]/a[ii][jj];
    for (i=j+1; i<n; i++) {
      if ((n - i) <= edge) {
        ibs = edge;
        ibs = n-edge;
        skip = edge;
      } else {
        ibs = block_size;
        skip = block_size;
      }
      ii = (i/block_size) + (j/block_size)*nblocks;
      jj = (i%ibs)+(j%jbs)*skip;

      y[i] -= a[ii][jj]*y[j];
    }
  }

  for (j=n-1; j>=0; j--) {
    for (i=0; i<j; i++) {
      if ((n - i) <= edge) {
	ibs = edge;
        ibs = n-edge;
        skip = edge;
      } else {
	ibs = block_size;
        skip = block_size;
      }
      if ((n - j) <= edge) {
	jbs = edge;
        jbs = n-edge;
      } else {
	jbs = block_size;
      }
      ii = (i/block_size) + (j/block_size)*nblocks;
      jj = (i%ibs)+(j%jbs)*skip;
      y[i] -= a[ii][jj]*y[j];
    }
  }

  max_diff = 0.0;
  for (j=0; j<n; j++) {
    diff = y[j] - 1.0;
    if (fabs(diff) > 0.00001) {
      bogus = 1;
      max_diff = diff;
    }
  }
  if (bogus) {
    printf("TEST FAILED: (%.5f diff)\n", max_diff);
  } else {
    printf("TEST PASSED\n");
  }
  free(y);
}


void printerr(s)

char *s;

{
  fprintf(stderr,"ERROR: %s\n",s);
}
