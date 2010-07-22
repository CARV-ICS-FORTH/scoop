#ifndef __LU_H__
#define __LU_H__


struct bdiv_args {
  int stride_a;
  int stride_diag;
  int dimi;
  int dimk;
} __attribute__ ((aligned(16)));


struct bmodd_args {
  int dimi;
  int dimj;
  int stride_a;
  int stride_c;
} __attribute__ ((aligned(16)));


struct bmod_args {
  int dimi;
  int dimj;
  int dimk;
  int stridea;
  int strideb;
  int stridec;
} __attribute__ ((aligned(16)));

void spe_daxpy(double *a, double *b, int n, double alpha);
void spe_bmod(double *a, double *b, double *c, struct bmod_args *args);
void spe_bmodd(double *a, double *c, struct bmodd_args *args);
void spe_bdiv(double *a, double *diag, struct bdiv_args *args);

#endif
