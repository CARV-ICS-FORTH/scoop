#ifndef __LU_H__
#define __LU_H__


struct bdiv_args {
  int stride_a;
  int stride_diag;
  int dimi;
  int dimk;
} __attribute__ ((aligned(128)));


struct bmodd_args {
  int dimi;
  int dimj;
  int stride_a;
  int stride_c;
} __attribute__ ((aligned(128)));


struct bmod_args {
  int dimi;
  int dimj;
  int dimk;
  int stridea;
  int strideb;
  int stridec;
} __attribute__ ((aligned(128)));

void spe_daxpy(float *a, float *b, int n, float alpha);
void spe_bmod(float *a, float *b, float *c, struct bmod_args *args);
void spe_bmodd(float *a, float *c, struct bmodd_args *args);
void spe_bdiv(float *a, float *diag, struct bdiv_args *args);

#endif
