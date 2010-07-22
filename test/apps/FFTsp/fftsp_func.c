#include <stdio.h>
#include <malloc.h>
#include <spu_intrinsics.h>
#include "tpc_common.h"
#include "tpc_spe.h"
#include "fftsp.h"

#define SWAP(a,b) {float tmp; tmp=a; a=b; b=tmp;}




void Scale(n1, N, x)
int n1; 
int N;
float *x;
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


/*int BitReverse_vec(M, k)
int M; 
int k;
{
  int i; 
  vector unsigned int j = {0, 0, 0, 0};
  vector unsigned int tmp = {k, k, k, k};
  vector unsigned int mask = {1, 1, 1, 1};

  for (i=0; i<M; i++) {
    //j = 2*j + (tmp&0x1);
    //tmp = tmp>>1;
    j = spu_sl(j, 1) + spu_and(tmp, mask);
    tmp = spu_rlmask(tmp, 1);;
  }
  return(spu_extract(j, 0));
}*/


void Reverse(N, M, x)
int N; 
int M;
float *x;
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


/*void TwiddleOneCol(direction, n1, N, u, x, pad_length)
int direction; 
int n1;
int N;
float *u;
float *x;
int pad_length;
{
  int i;
  float omega_r; 
  float omega_c; 
  float x_r; 
  float x_c;

  for (i=0; i<n1; i++) {
    // @@@ tzenakis
    // The required initial offset for u is calculated by the caller
    //
    omega_r = u[2*i];
    omega_c = direction*u[2*i+1];
    x_r = x[2*i];
    x_c = x[2*i+1];
    x[2*i] = omega_r*x_r - omega_c*x_c;
    x[2*i+1] = omega_r*x_c + omega_c*x_r;
  }
}*/



// original fft_twiddle, operating on single complex double element
/*static __inline__ vector double fft_twiddle(vector double vecu,
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
}*/



// fff_twiddle now operates on 2 complex float elements.
static __inline__ vector float fft_twiddle_2v(vector float vecu,
    vector float vecx, vector float vdir)
{
  vector float omega;
  vector float tmpx;
  
  vector float vperm0, vperm1, vperm2;
  vector float vbdfh, vaceg;
  vector float norma = {-1.0, 1.0, -1.0, 1.0};

  vector unsigned char vpat0 = (vector unsigned char) { 0x04, 0x05, 0x06, 0x07,
                                                        0x04, 0x05, 0x06, 0x07,
                                                        0x0C, 0x0D, 0x0E, 0x0F,
                                                        0x0C, 0x0D, 0x0E, 0x0F };

  vector unsigned char vpat1 = (vector unsigned char) { 0x14, 0x15, 0x16, 0x17,
                                                        0x10, 0x11, 0x12, 0x13,
                                                        0x1C, 0x1D, 0x1E, 0x1F,
							0x18, 0x19, 0x1A, 0x1B };
 
  vector unsigned char vpat2 = (vector unsigned char) { 0x00, 0x01, 0x02, 0x03,
                                                        0x00, 0x01, 0x02, 0x03,
                                                        0x08, 0x09, 0x0A, 0x0B,
                                                        0x08, 0x09, 0x0A, 0x0B };

  omega = vecu*vdir;
  tmpx = vecx;

  vperm0 = spu_shuffle(omega, tmpx, vpat0);
  vperm1 = spu_shuffle(omega, tmpx, vpat1);
  vperm2 = spu_shuffle(omega, tmpx, vpat2);

  vbdfh = vperm0 * vperm1;
  vaceg = vperm2 * tmpx;

  return spu_madd(vbdfh, norma, vaceg);
}

void TwiddleOneCol_vec(direction, n1, N, u, x, pad_length)
int direction; 
int n1;
int N;
float *u;
float *x;
int pad_length;
{
  int i;
  vector float *vecu = (vector float *)u;
  vector float *vecx = (vector float *)x;
  vector float omega_dir={1.0, (float)direction, 1.0, (float)direction};
  vector float tmp0, tmp1, tmp2, tmp3;
  vector float tmp4, tmp5, tmp6, tmp7;

  // Deeply unrolled. Least M argument now is "FFT -m8".

  n1 = n1>>1; // n1/2

  for (i=0; i<n1; i+=8) {
    tmp0 = fft_twiddle_2v(vecu[i  ], vecx[i  ], omega_dir);
    tmp1 = fft_twiddle_2v(vecu[i+1], vecx[i+1], omega_dir);
    tmp2 = fft_twiddle_2v(vecu[i+2], vecx[i+2], omega_dir);
    tmp3 = fft_twiddle_2v(vecu[i+3], vecx[i+3], omega_dir);
    tmp4 = fft_twiddle_2v(vecu[i+4], vecx[i+4], omega_dir);
    tmp5 = fft_twiddle_2v(vecu[i+5], vecx[i+5], omega_dir);
    tmp6 = fft_twiddle_2v(vecu[i+6], vecx[i+6], omega_dir);
    tmp7 = fft_twiddle_2v(vecu[i+7], vecx[i+7], omega_dir);

    vecx[i  ] = tmp0;
    vecx[i+1] = tmp1;
    vecx[i+2] = tmp2;
    vecx[i+3] = tmp3;
    vecx[i+4] = tmp4;
    vecx[i+5] = tmp5;
    vecx[i+6] = tmp6;
    vecx[i+7] = tmp7;
  }
}


void FFT1DOnce_vec(direction, M, N, u, x)
int direction; 
int M; 
int N;
float *u; 
float *x;
{
  int j; 
  int k; 
  int q; 
  int L; 
  int r; 
  int Lstar;

  vector float *vecu = (vector float *)u;
  vector float *vecu1;
  vector float *vecx = (vector float *)x;
  vector float *vecx1;
  vector float *vecx2;
  vector float tau;
  vector float tmpx;
  vector float omega_dir={1.0, (float)direction, 1.0, (float)direction};


  vector unsigned char vpat = (vector unsigned char) { 0x08, 0x09, 0x0A, 0x0B,
                                                       0x0C, 0x0D, 0x0E, 0x0F,
                                                       0x18, 0x19, 0x1A, 0x1B,
                                                       0x1C, 0x1D, 0x1E, 0x1F };


  Reverse(N, M, x);

  q=1;
  {
    vector float vu1 = *vecu;
    vector float vx1_0, vx2_0, vtx_0;
    vector float vx1_1, vx2_1, vtx_1;
    vector float vx1_2, vx2_2, vtx_2;
    vector float vx1_3, vx2_3, vtx_3;

    vector float tau0, tau1, tau2, tau3;

    r = N/2;
    for (k=0; k<r; k+=4) {
      // loop i+0
      vx2_0 = vecx[k+0];
      tau0 = fft_twiddle_2v(vu1, vx2_0, omega_dir);
      vtx_0 = spu_rlqwbyte(vecx[k+0], 8);
      vx2_0 = vtx_0 - tau0;
      vx1_0 = vtx_0 + tau0;
      vecx[k+0] = spu_shuffle(vx1_0, vx2_0, vpat);

      // loop i+1
      vx2_1 = vecx[k+1];
      tau1 = fft_twiddle_2v(vu1, vx2_1, omega_dir);
      vtx_1 = spu_rlqwbyte(vecx[k+1], 8);
      vx2_1 = vtx_1 - tau1;
      vx1_1 = vtx_1 + tau1;
      vecx[k+1] = spu_shuffle(vx1_1, vx2_1, vpat);

      // loop i+2
      vx2_2 = vecx[k+2];
      tau2 = fft_twiddle_2v(vu1, vx2_2, omega_dir);
      vtx_2 = spu_rlqwbyte(vecx[k+2], 8);
      vx2_2 = vtx_2 - tau2;
      vx1_2 = vtx_2 + tau2;
      vecx[k+2] = spu_shuffle(vx1_2, vx2_2, vpat);

      // loop i+3
      vx2_3 = vecx[k+3];
      tau3 = fft_twiddle_2v(vu1, vx2_3, omega_dir);
      vtx_3 = spu_rlqwbyte(vecx[k+3], 8);
      vx2_3 = vtx_3 - tau3;
      vx1_3 = vtx_3 + tau3;
      vecx[k+3] = spu_shuffle(vx1_3, vx2_3, vpat);
    }
  }

  for (q=2; q<=M; q++) {
    L = 1<<q; r = N/L; Lstar = L/2;
    vecu1 = (vector float *)&u[2*(Lstar)];  //&vecu[(Lstar-1)/2]; 
    for (k=0; k<r; k++) {
      vecx1 = (vector float *)&x[2*(k*L)];  //&vecx[(k*L)/2];
      vecx2 = (vector float *)&x[2*(k*L+Lstar)];  //&vecx[(k*L+Lstar)/2];
      for (j=0; j<Lstar/2; j++) {
	tau = fft_twiddle_2v(vecu1[j], vecx2[j], omega_dir);
	tmpx = vecx1[j];
	vecx2[j] = tmpx - tau;
	vecx1[j] = tmpx + tau;
      }
    }
  }
}



/*void FFT1DOnce(direction, M, N, u, x)
int direction; 
int M; 
int N;
float *u; 
float *x;
{
  int j; 
  int k; 
  int q; 
  int L; 
  int r; 
  int Lstar;
  float *u1; 
  float *x1; 
  float *x2;
  float omega_r; 
  float omega_c; 
  float tau_r; 
  float tau_c; 
  float x_r; 
  float x_c;

  Reverse(N, M, x);

  for (q=1; q<=M; q++) {
    L = 1<<q; r = N/L; Lstar = L/2;
    u1 = &u[2*(Lstar)];
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
}*/


int FFTsteps23(float *scratch, float *upriv, float *umain2,
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



int FFTsteps5(float *x, float *upriv, struct FFTsteps23args *args )
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


static float Buf[2*32*32] __attribute__ ((aligned(128)));

int blockcomplex_transose_swap_vec(float *block1, float *block2, struct transargs *args)
{
  int i, j;
  int idim, jdim;
  int index0, index1, off0, off1;
  int block_dim=args->block_dim;
  vector float *vblock1, *vblock2, *vBuf;
  vector unsigned char leftpat  = (vector unsigned char) { 0x00, 0x01, 0x02, 0x03,
                                                           0x04, 0x05, 0x06, 0x07,
                                                           0x10, 0x11, 0x12, 0x13,
                                                           0x14, 0x15, 0x16, 0x17 };
  
  vector unsigned char rightpat = (vector unsigned char) { 0x08, 0x09, 0x0A, 0x0B,
                                                           0x0C, 0x0D, 0x0E, 0x0F,
                                                           0x18, 0x19, 0x1A, 0x1B,
                                                           0x1C, 0x1D, 0x1E, 0x1F };
  vector float tmp0, tmp1;
  //if(Buf==NULL) {
  //  Buf = (float*)memalign(16, block_dim*block_dim*2*sizeof(float));
  //}

  idim = block_dim;
  jdim = block_dim>>1; //block_dim/2
  vblock1 = (vector float *)block1;
  vblock2 = (vector float *)block2;
  vBuf = (vector float *)Buf;

  for(i=0; i<idim; i+=2) {
    for(j=0; j<jdim; j+=1) {
      //printf("%d, %d\n", i, j);
      index0 = i*jdim+j;
      index1 = (i+1)*jdim+j;
      off0   = j*idim+i/2;
      off1   = j*idim+(i/2+jdim);

      //printf("vBuf[%d] = shuffle(vblock[%d], vblock[%d]\n", index0, off0 , off1 );
      //printf("vBuf[%d] = shuffle(vblock[], vblock[]\n", index1  );
      tmp0 = spu_shuffle(vblock1[off0], vblock1[off1], leftpat);
      tmp1 = spu_shuffle(vblock1[off0], vblock1[off1], rightpat);

      vBuf[index0] = tmp0;
      vBuf[index1] = tmp1;
    }
  }

  for(i=0; i<idim; i+=2) {
    for(j=0; j<jdim; j+=1) {
      //printf("%d, %d\n", i, j);
      index0 = i*jdim+j;
      index1 = (i+1)*jdim+j;
      off0   = j*idim+i/2;
      off1   = j*idim+(i/2+jdim);

      //printf("vBuf[%d] = shuffle(vblock[%d], vblock[%d]\n", index0, off0 , off1 );
      //printf("vBuf[%d] = shuffle(vblock[], vblock[]\n", index1  );
      vblock1[index0] = spu_shuffle(vblock2[off0], vblock2[off1], leftpat);
      vblock1[index1] = spu_shuffle(vblock2[off0], vblock2[off1], rightpat);
    }
  }

  block_dim = idim*jdim;
  for(i=0; i<block_dim; i+=16) {
    vector float tmpvblock20 = vBuf[i];
    vector float tmpvblock21 = vBuf[i+1];
    vector float tmpvblock22 = vBuf[i+2];
    vector float tmpvblock23 = vBuf[i+3];
    vector float tmpvblock24 = vBuf[i+4];
    vector float tmpvblock25 = vBuf[i+5];
    vector float tmpvblock26 = vBuf[i+6];
    vector float tmpvblock27 = vBuf[i+7];
    
    vector float tmpvblock28 = vBuf[i+8];
    vector float tmpvblock29 = vBuf[i+9];
    vector float tmpvblock210 = vBuf[i+10];
    vector float tmpvblock211 = vBuf[i+11];
    vector float tmpvblock212 = vBuf[i+12];
    vector float tmpvblock213 = vBuf[i+13];
    vector float tmpvblock214 = vBuf[i+14];
    vector float tmpvblock215 = vBuf[i+15];

    vblock2[i+0] = tmpvblock20;
    vblock2[i+1] = tmpvblock21;
    vblock2[i+2] = tmpvblock22;
    vblock2[i+3] = tmpvblock23;
    vblock2[i+4] = tmpvblock24;
    vblock2[i+5] = tmpvblock25;
    vblock2[i+6] = tmpvblock26;
    vblock2[i+7] = tmpvblock27;
    vblock2[i+8] = tmpvblock28;
    vblock2[i+9] = tmpvblock29;
    vblock2[i+10] = tmpvblock210;
    vblock2[i+11] = tmpvblock211;
    vblock2[i+12] = tmpvblock212;
    vblock2[i+13] = tmpvblock213;
    vblock2[i+14] = tmpvblock214;
    vblock2[i+15] = tmpvblock215;
  }

  return 0;
}



int blockcomplex_transose_vec(float *block, struct transargs *args)
{
  int i, j;
  int idim, jdim;
  int index0, index1, off0, off1;
  int block_dim=args->block_dim;
  vector float *vblock, *vBuf;
  //vector float blk0,blk1,blk2,blk3;
  vector unsigned char leftpat  = (vector unsigned char) { 0x00, 0x01, 0x02, 0x03,
                                                           0x04, 0x05, 0x06, 0x07,
                                                           0x10, 0x11, 0x12, 0x13,
                                                           0x14, 0x15, 0x16, 0x17 };
  
  vector unsigned char rightpat = (vector unsigned char) { 0x08, 0x09, 0x0A, 0x0B,
                                                           0x0C, 0x0D, 0x0E, 0x0F,
                                                           0x18, 0x19, 0x1A, 0x1B,
                                                           0x1C, 0x1D, 0x1E, 0x1F };
  /*if(Buf==NULL) {
    Buf = (float*)memalign(16, block_dim*block_dim*2*sizeof(float));
  }*/

  idim = block_dim;
  jdim = block_dim>>1; //block_dim/2
  vblock = (vector float *)block;
  vBuf = (vector float *)Buf;

  //printf("\n\n");

  for(i=0; i<idim; i+=2) {
    for(j=0; j<jdim; j+=1) {
      vector float tmp0, tmp1;
      //printf("%d, %d\n", i, j);
      index0 = i*jdim+j;
      index1 = (i+1)*jdim+j;
      off0   = j*idim+i/2;
      off1   = j*idim+(i/2+jdim);

      //printf("vBuf[%d] = shuffle(vblock[%d], vblock[%d]\n", index0, off0 , off1 );
      //printf("vBuf[%d] = shuffle(vblock[], vblock[]\n", index1  );
      tmp0 = spu_shuffle(vblock[off0], vblock[off1], leftpat);
      tmp1 = spu_shuffle(vblock[off0], vblock[off1], rightpat);

      vBuf[index0] = tmp0;
      vBuf[index1] = tmp1;
    }
  }

  block_dim = idim*jdim;
  for(i=0; i<block_dim; i+=16) {
    vector float tmpvblock0 = vBuf[i];
    vector float tmpvblock1 = vBuf[i+1];
    vector float tmpvblock2 = vBuf[i+2];
    vector float tmpvblock3 = vBuf[i+3];
    vector float tmpvblock4 = vBuf[i+4];
    vector float tmpvblock5 = vBuf[i+5];
    vector float tmpvblock6 = vBuf[i+6];
    vector float tmpvblock7 = vBuf[i+7];
    
    vector float tmpvblock8 = vBuf[i+8];
    vector float tmpvblock9 = vBuf[i+9];
    vector float tmpvblock10 = vBuf[i+10];
    vector float tmpvblock11 = vBuf[i+11];
    vector float tmpvblock12 = vBuf[i+12];
    vector float tmpvblock13 = vBuf[i+13];
    vector float tmpvblock14 = vBuf[i+14];
    vector float tmpvblock15 = vBuf[i+15];

    vblock[i+0] = tmpvblock0;
    vblock[i+1] = tmpvblock1;
    vblock[i+2] = tmpvblock2;
    vblock[i+3] = tmpvblock3;
    vblock[i+4] = tmpvblock4;
    vblock[i+5] = tmpvblock5;
    vblock[i+6] = tmpvblock6;
    vblock[i+7] = tmpvblock7;
    vblock[i+8] = tmpvblock8;
    vblock[i+9] = tmpvblock9;
    vblock[i+10] = tmpvblock10;
    vblock[i+11] = tmpvblock11;
    vblock[i+12] = tmpvblock12;
    vblock[i+13] = tmpvblock13;
    vblock[i+14] = tmpvblock14;
    vblock[i+15] = tmpvblock15;
  }

  return 0;
}


int execute_task(queue_entry_t *ex_task,
    tpc_spe_task_state_t *task_info)
{
  int exit = 0;
  float *arg1, *arg2, *arg3;
  struct FFTsteps23args *s23args;
  struct transargs *argints;
  int elems, elem_sz;

  switch(ex_task->funcid)
  {
    case 0:
      arg1 = (float *)task_info->ls_addr;
      arg2 = (float *)((void *)arg1 + ex_task->arguments[0].size);
      arg3 = (float *)((void *)arg2 + ex_task->arguments[1].size);
      s23args = (struct FFTsteps23args *)((void *)arg3 + ex_task->arguments[2].size);
      FFTsteps23(arg1, arg2, arg3, s23args);
      task_info->state = EXECUTED;
      break;
    case 1:
      arg1 = (float *)task_info->ls_addr;
      arg2 = (float *)((void *)arg1 + ex_task->arguments[0].size);
      s23args = (struct FFTsteps23args *)((void *)arg2 + ex_task->arguments[1].size);
      FFTsteps5(arg1, arg2, s23args);
      task_info->state = EXECUTED;
      break;
    case 2:
      arg1 = (float *)task_info->ls_addr;
      elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[0].size);
      elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[0].size);
      argints = (struct transargs *)((void *)arg1 + (elems*elem_sz));
      blockcomplex_transose_vec(arg1, argints);
      task_info->state = EXECUTED;
      break;
    case 3:
      arg1 = (float *)task_info->ls_addr;
      elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[0].size);
      elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[0].size);
      arg2 = (float *)((void *)arg1 + (elems*elem_sz));
      elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[1].size);
      elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[1].size);
      argints = (struct transargs *)((void *)arg2 + (elems*elem_sz));
      blockcomplex_transose_swap_vec(arg1, arg2, argints);
      task_info->state = EXECUTED;
      break;
    default:
      exit = 1;
    break;
  }

  return exit;
}

