#ifndef _PBPI_RAND_H
#define _PBPI_RAND_H

double	expodev(double lamda);
double	gammdev(double alpha);
double	normdev();
void	randseed(unsigned long init_key[], int key_length);
double	unifdev(void);

double	randbeta(double alpha, double beta);
void	randdirichlet(int n, double *alpha, double *x);
double	randexponential(double lamda);
double	randgama(double alpha, double beta);
double	randnormal(double mu, double theta);

double	PointNormal(double prob);
double	PointChi2 (double prob, double v);
#define PointGamma(prob,alpha,beta) PointChi2(prob,2.0*(alpha))/(2.0*(beta))

int histogram(int n, double X[], double Xmin, double Xmax, int m, double Xbin[], int Cbin[]);
int basicstatistic(int n, double *X, double *Xmin, double *Xmax, double *Xmean, double *Xvar2);

//generate a random integer r which falls between lbound and rbound,ibound<=r<=rbound
int	irand(int lbound, int rbound);

//permutate elements in array s[]
void	permutate(int *s, int size);
void RNG1_seed(unsigned long s);

#endif

