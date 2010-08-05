#include <stdio.h>
#include <malloc.h>
#include <spu_intrinsics.h>
#include "tpc_common.h"
#include "tpc_spe.h"
#include "fft.h"

#define SWAP(a,b) {double tmp; tmp=a; a=b; b=tmp;}




void Scale(n1, N, x)
int n1; 
int N;
double *x;
{
  int i;

  for (i=0; i<n1; i++) {
    x[2*i] /= N;
    x[2*i+1] /= N;
  }
}


int BitReverse(M, k)
int M; 
int k;
{
  int i; 
  int j; 
  int tmp;

  j = 0;
  tmp = k;
  for (i=0; i<M; i++) {
    j = 2*j + (tmp&0x1);
    tmp = tmp>>1;
  }
  return(j);
}


void Reverse(N, M, x)
int N; 
int M;
double *x;
{
  int j, k;

  for (k=0; k<N; k++) {
    j = BitReverse(M, k);
    if (j > k) {
      SWAP(x[2*j], x[2*k]);
      SWAP(x[2*j+1], x[2*k+1]);
    }
  }
}


void TwiddleOneCol(direction, n1, N, u, x, pad_length)
int direction; 
int n1;
int N;
double *u;
double *x;
int pad_length;
{
  int i;
  double omega_r; 
  double omega_c; 
  double x_r; 
  double x_c;

  for (i=0; i<n1; i++) {
    /* @@@ tzenakis
     * The required initial offset for u is calculated by the caller
     */
    omega_r = u[2*i];
    omega_c = direction*u[2*i+1];
    x_r = x[2*i];
    x_c = x[2*i+1];
    x[2*i] = omega_r*x_r - omega_c*x_c;
    x[2*i+1] = omega_r*x_c + omega_c*x_r;
  }
}



static __inline__ vector double fft_twiddle(vector double vecu,
    vector double vecx, vector double vdir)
{
  vector double omega, omega_b;
  vector double tmpx;
  vector double rrcc, rcrc;
  vector double rrcc_b, rcrc_b;
  
  vector unsigned char vpat1 = (vector unsigned char) { 0x00, 0x01, 0x02, 0x03,
                                                        0x04, 0x05, 0x06, 0x07,
                                                        0x18, 0x19, 0x1A, 0x1B,
                                                        0x1C, 0x1D, 0x1E, 0x1F };
  omega = vecu;
  omega *= vdir;
  omega_b = spu_rlqwbyte(omega, 8); // invert omega
  tmpx = vecx;
  
  rrcc = omega * tmpx;
  rcrc = omega_b * tmpx;
  
  rrcc_b = spu_rlqwbyte(rrcc, 8);
  rcrc_b = spu_rlqwbyte(rcrc, 8);

  rrcc = rrcc - rrcc_b;
  rcrc = rcrc + rcrc_b;

  return spu_shuffle(rrcc, rcrc, vpat1);
}

void TwiddleOneCol_vec(direction, n1, N, u, x, pad_length)
int direction; 
int n1;
int N;
double *u;
double *x;
int pad_length;
{
  int i;
  vector double *vecu = (vector double *)u;
  vector double *vecx = (vector double *)x;
  vector double omega_dir={1.0, (double)direction};

  // Deeply unrolled. Least M argument now is "FFT -m6".

  for (i=0; i<n1; i+=8) {
    vecx[i  ] = fft_twiddle(vecu[i  ], vecx[i  ], omega_dir);
    vecx[i+1] = fft_twiddle(vecu[i+1], vecx[i+1], omega_dir);
    vecx[i+2] = fft_twiddle(vecu[i+2], vecx[i+2], omega_dir);
    vecx[i+3] = fft_twiddle(vecu[i+3], vecx[i+3], omega_dir);
    vecx[i+4] = fft_twiddle(vecu[i+4], vecx[i+4], omega_dir);
    vecx[i+5] = fft_twiddle(vecu[i+5], vecx[i+5], omega_dir);
    vecx[i+6] = fft_twiddle(vecu[i+6], vecx[i+6], omega_dir);
    vecx[i+7] = fft_twiddle(vecu[i+7], vecx[i+7], omega_dir);
  }
}


void FFT1DOnce_vec(direction, M, N, u, x)
int direction; 
int M; 
int N;
double *u; 
double *x;
{
  int j; 
  int k; 
  int q; 
  int L; 
  int r; 
  int Lstar;

  vector double *vecu = (vector double *)u;
  vector double *vecu1;
  vector double *vecx = (vector double *)x;
  vector double *vecx1;
  vector double *vecx2;
  vector double tau;
  vector double tmpx;
  vector double omega_dir={1.0, (double)direction};



  Reverse(N, M, x);

  for (q=1; q<=M; q++) {
    L = 1<<q; r = N/L; Lstar = L/2;
    vecu1 = &vecu[Lstar-1];
    for (k=0; k<r; k++) {
      vecx1 = &vecx[k*L];
      vecx2 = &vecx[k*L+Lstar];
      for (j=0; j<Lstar; j++) {
	tau = fft_twiddle(vecu1[j], vecx2[j], omega_dir);
	tmpx = vecx1[j];
	vecx2[j] = tmpx - tau;
	vecx1[j] = tmpx + tau;
      }
    }
  }
}



void FFT1DOnce(direction, M, N, u, x)
int direction; 
int M; 
int N;
double *u; 
double *x;
{
  int j; 
  int k; 
  int q; 
  int L; 
  int r; 
  int Lstar;
  double *u1; 
  double *x1; 
  double *x2;
  double omega_r; 
  double omega_c; 
  double tau_r; 
  double tau_c; 
  double x_r; 
  double x_c;

  Reverse(N, M, x);

  for (q=1; q<=M; q++) {
    L = 1<<q; r = N/L; Lstar = L/2;
    u1 = &u[2*(Lstar-1)];
    for (k=0; k<r; k++) {
      x1 = &x[2*(k*L)];
      x2 = &x[2*(k*L+Lstar)];
      for (j=0; j<Lstar; j++) {
	omega_r = u1[2*j]; 
        omega_c = direction*u1[2*j+1];
	x_r = x2[2*j]; 
        x_c = x2[2*j+1];
	tau_r = omega_r*x_r - omega_c*x_c;
	tau_c = omega_r*x_c + omega_c*x_r;
	x_r = x1[2*j]; 
        x_c = x1[2*j+1];
	x2[2*j] = x_r - tau_r;
	x2[2*j+1] = x_c - tau_c;
	x1[2*j] = x_r + tau_r;
	x1[2*j+1] = x_c + tau_c;
      }
    }
  }
}


int FFTsteps23(double *scratch, double *upriv, double *umain2,
    struct FFTsteps23args *args)
{
  int i=0;

  for(i=0; i<args->rows; ++i) {
    FFT1DOnce_vec(args->direction, args->m1, args->n1, upriv,
        &scratch[2*i*(args->n1+args->pad_length)]);

    TwiddleOneCol_vec(args->direction, args->n1, args->N,
        &umain2[2*i*(args->n1+args->pad_length)],
        &scratch[2*i*(args->n1+args->pad_length)], args->pad_length);
  }

  return 0;
}



int FFTsteps5(double *x, double *upriv, struct FFTsteps23args *args )
{
  int i=0;

  for(i=0; i<args->rows; ++i) {
    FFT1DOnce_vec(args->direction, args->m1, args->n1, upriv,
        &x[2*i*(args->n1+args->pad_length)]);

    if (args->direction == -1) {
      Scale(args->n1, args->N, &x[2*i*(args->n1+args->pad_length)]);
    }
  }

  return 0;
}


static double Buf[2*32*32] __attribute__ ((aligned(128)));

int blockcomplex_transose_swap_vec(double *block1, double *block2, struct transargs *args)
{
  int i, j;
  int block_dim=args->block_dim;
  vector double *vblock1, *vblock2, *vBuf;

  //printf("TRANS_SWAP: %d\n", block_dim);
  /*if(Buf==NULL) {
    Buf = (double*)memalign(16, block_dim*block_dim*2*sizeof(double));
  }*/

  vblock1 = (vector double *)block1;
  vblock2 = (vector double *)block2;
  vBuf = (vector double *)Buf;
  
  for(i=0; i<block_dim; ++i) {
    for(j=0; j<block_dim; ++j) {
      vBuf[block_dim*j+i] = vblock1[block_dim*i+j];
    }
  }

  for(i=0; i<block_dim; ++i) {
    for(j=0; j<block_dim; ++j) {
      vblock1[block_dim*j+i] = vblock2[block_dim*i+j];
    }
  }

  block_dim = block_dim*block_dim;
  for(i=0; i<block_dim; i+=4) {
    vblock2[i  ] = vBuf[i  ];
    vblock2[i+1] = vBuf[i+1];
    vblock2[i+2] = vBuf[i+2];
    vblock2[i+3] = vBuf[i+3];
  }

  return 0;
}

int blockcomplex_transose_vec(double *block, struct transargs *args)
{
  int i, j;
  int block_dim=args->block_dim;
  vector double *vblock, *vBuf;

  /*if(Buf==NULL) {
    Buf = (double*)memalign(16, block_dim*block_dim*2*sizeof(double));
  }*/

  vblock = (vector double *)block;
  vBuf = (vector double *)Buf;

  for(i=0; i<block_dim; ++i) {
    for(j=0; j<block_dim; ++j) {
      vBuf[block_dim*j+i] = vblock[block_dim*i+j];
    }
  }

  block_dim = block_dim*block_dim;
  for(i=0; i<block_dim; i+=4) {
    vblock[i  ] = vBuf[i  ];
    vblock[i+1] = vBuf[i+1];
    vblock[i+2] = vBuf[i+2];
    vblock[i+3] = vBuf[i+3];
    //printf("TRANS: %d\n", block_dim);
  }

  return 0;
}

/*
int execute_task(queue_entry_t *ex_task,
    tpc_spe_task_state_t *task_info)
{
  int exit = 0;
  double *arg1, *arg2, *arg3;
  struct FFTsteps23args *s23args;
  struct transargs *argints;
  int elems, elem_sz;

  switch(ex_task->funcid)
  {
    case 0:
      arg1 = (double *)task_info->ls_addr;
      arg2 = (double *)((void *)arg1 + ex_task->arguments[0].size);
      arg3 = (double *)((void *)arg2 + ex_task->arguments[1].size);
      s23args = (struct FFTsteps23args *)((void *)arg3 + ex_task->arguments[2].size);
      FFTsteps23(arg1, arg2, arg3, s23args);
      task_info->state = EXECUTED;
      break;
    case 1:
      arg1 = (double *)task_info->ls_addr;
      arg2 = (double *)((void *)arg1 + ex_task->arguments[0].size);
      s23args = (struct FFTsteps23args *)((void *)arg2 + ex_task->arguments[1].size);
      FFTsteps5(arg1, arg2, s23args);
      task_info->state = EXECUTED;
      break;
    default:
      exit = 1;
    break;
  }

  return exit;
}*/

