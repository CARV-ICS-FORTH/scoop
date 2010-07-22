#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "rand.h"
#include "error.h"

//DECLARATIONS: functions called locally
double gammadev_1(double alpha);
double gammadev_2(double alpha);

//void error(char *s)
//{
//	fprintf(stderr, "%s", s);
//	exit(0);
//}

/*********************************************************Z********************\
 *																			 *
 * MT1937: random number generator written by Takuji Nishimura				 *
 *																			 *
\*****************************************************************************/

/* 
   A C-program for MT19937, with initialization improved 2002/1/26.
   Coded by Takuji Nishimura and Makoto Matsumoto.

   Before using, initialize the state by using init_genrand(seed)  
   or init_by_array(init_key, key_length).

   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
   All rights reserved.                          

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote 
        products derived from this software without specific prior written 
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


   Any feedback is very welcome.
   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
   email: m-mat @ math.sci.hiroshima-u.ac.jp (remove space)
*/

/* Period parameters */  
#define N 624
#define M 397
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UPPER_MASK 0x80000000UL /* most significant w-r bits */
#define LOWER_MASK 0x7fffffffUL /* least significant r bits */

static unsigned long mt[N]; /* the array for the state vector  */
static int mti=N+1; /* mti==N+1 means mt[N] is not initialized */

/* initializes mt[N] with a seed */
void init_genrand(unsigned long s)
{
    mt[0]= s & 0xffffffffUL;
    for (mti=1; mti<N; mti++) {
        mt[mti] = 
	    (1812433253UL * (mt[mti-1] ^ (mt[mti-1] >> 30)) + mti); 
        /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
        /* In the previous versions, MSBs of the seed affect   */
        /* only MSBs of the array mt[].                        */
        /* 2002/01/09 modified by Makoto Matsumoto             */
        mt[mti] &= 0xffffffffUL;
        /* for >32 bit machines */
    }
}

/* initializes mt[N] with a seed */
void RNG1_seed(unsigned long s)
{
    mt[0]= s & 0xffffffffUL;
    for (mti=1; mti<N; mti++) {
        mt[mti] = 
	    (1812433253UL * (mt[mti-1] ^ (mt[mti-1] >> 30)) + mti); 
        /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
        /* In the previous versions, MSBs of the seed affect   */
        /* only MSBs of the array mt[].                        */
        /* 2002/01/09 modified by Makoto Matsumoto             */
        mt[mti] &= 0xffffffffUL;
        /* for >32 bit machines */
    }
}
/* initialize by an array with array-length */
/* init_key is the array for initializing keys */
/* key_length is its length */
/* slight change for C++, 2004/2/26 */
void init_by_array(unsigned long init_key[], int key_length)
{
    int i, j, k;
    init_genrand(19650218UL);
    i=1; j=0;
    k = (N>key_length ? N : key_length);
    for (; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1664525UL))
          + init_key[j] + j; /* non linear */
        mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++; j++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
        if (j>=key_length) j=0;
    }
    for (k=N-1; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1566083941UL))
          - i; /* non linear */
        mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
    }

    mt[0] = 0x80000000UL; /* MSB is 1; assuring non-zero initial array */ 
}

/* generates a random number on [0,0xffffffff]-interval */
unsigned long genrand_int32(void)
{
    unsigned long y;
    static unsigned long mag01[2]={0x0UL, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    if (mti >= N) { /* generate N words at one time */
        int kk;

        if (mti == N+1)   /* if init_genrand() has not been called, */
            init_genrand(5489UL); /* a default initial seed is used */

        for (kk=0;kk<N-M;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        for (;kk<N-1;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
        mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1UL];

        mti = 0;
    }
  
    y = mt[mti++];

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);

    return y;
}

/* generates a random number on [0,0x7fffffff]-interval */
long genrand_int31(void)
{
    return (long)(genrand_int32()>>1);
}

/* generates a random number on [0,1]-real-interval */
double genrand_real1(void)
{
    return genrand_int32()*(1.0/4294967295.0); 
    /* divided by 2^32-1 */ 
}

/* generates a random number on [0,1)-real-interval */
double genrand_real2(void)
{
    return genrand_int32()*(1.0/4294967296.0); 
    /* divided by 2^32 */
}

/* generates a random number on (0,1)-real-interval */
double genrand_real3(void)
{
    return (((double)genrand_int32()) + 0.5)*(1.0/4294967296.0); 
    /* divided by 2^32 */
}

/* generates a random number on [0,1) with 53-bit resolution*/
double genrand_res53(void) 
{ 
    unsigned long a=genrand_int32()>>5, b=genrand_int32()>>6; 
    return(a*67108864.0+b)*(1.0/9007199254740992.0); 
}


/* initialize by an array with array-length */
/* init_key is the array for initializing keys */
/* key_length is its length */
/* slight change for C++, 2004/2/26 */
//void useed_mt(unsigned long init_key[], int key_length)
void randseed(unsigned long init_key[], int key_length)
//void init_by_array(unsigned long init_key[], int key_length)
{
    int i, j, k;
    init_genrand(19650218UL);
    i=1; j=0;
    k = (N>key_length ? N : key_length);
    for (; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1664525UL))
          + init_key[j] + j; /* non linear */
        mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++; j++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
        if (j>=key_length) j=0;
    }
    for (k=N-1; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1566083941UL))
          - i; /* non linear */
        mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
    }

    mt[0] = 0x80000000UL; /* MSB is 1; assuring non-zero initial array */ 
}

double unifdev(void)
//OVERVIEW:	generate a standard uniform variate in [0, 1]
//REQUIRES:	none
//EFFECTS:	return a random uniform variate
//MODIFIES:	the status of random number generator
//ALGORITHM:MT1937 written by Takuji Nishimura
{   
	unsigned long y;
    static unsigned long mag01[2]={0x0UL, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    if (mti >= N) { /* generate N words at one time */
        int kk;

        if (mti == N+1)   /* if init_genrand() has not been called, */
            init_genrand(5489UL); /* a default initial seed is used */

        for (kk=0;kk<N-M;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        for (;kk<N-1;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
        mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1UL];

        mti = 0;
    }
  
    y = mt[mti++];

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);

    return y*(1.0/4294967295.0);
}


/*
	Minimal random number generator proposed by Park and Miller.
	The code is adapted from "Numerical Recipes in C", page 279.
*/
/*
	“Minimal” random number generator of Park and Miller. Returns a uniform random deviate
	between 0.0 and 1.0. Set or reset idum to any integer value (except the unlikely value MASK)
	to initialize the sequence; idum must not be altered between calls for successive deviates in
	a sequence.
*/
#define IA 16807
#define IM 2147483647
#define AM (1.0/IM)
#define IQ 127773
#define IR 2836
#define MASK 123459876
double ran0(long *idum)
{
	long k;
	double ans;
	*idum ^= MASK; /*XORing with MASK allows use of zero and other simple bit patterns for idum.*/
	k=(*idum)/IQ;
	*idum=IA*(*idum-k*IQ)-IR*k; /* Compute idum=(IA*idum) IM without overflows by Schrage’s method. */
	if (*idum < 0) *idum += IM;
	ans=AM*(*idum);				/*Convert idum to a floating result.*/
	*idum ^= MASK;				/*Unmask before return.*/
	return ans;
}



double expodev(double lamda)
//OVERVIEW:	generate a standard exponential variate with mean 1/lamda.
//REQUIRES:	lamda > 0
//EFFECTS:	return a random expoential variate
//MODIFIES:	the status of random number generator
//ALGORITHM:The inversion method (LOGRITHM method) described in "Numerical Receipes  in C"
{
	return -log(unifdev())/lamda;
}



double normdev()
//OVERVIEW:	generate a standard normal variate with zero mean and unit variance
//REQUIRES:	none
//EFFECTS:	return a random normal variate
//MODIFIES:	the status of random number generator
//ALGORITHM:Box-miller methods described in Kunth's bok and "Numerical Receipes  in C"
{
	double v1, v2, s, r, small=1.0e-20;
	static double	xx;
	static int		flag=0;

	if(!flag)
	{
		do{	
			v1 = unifdev(); v1 += v1-1.0;
			v2 = unifdev(); v2 += v2-1.0;
			s = v1 * v1 + v2 * v2;
		}while(s>=1.0 || s<=0.0);

		//if( s<small) s+= small;

		r = sqrt( -2.0*log(s) / s);
		xx = v1 * r;
		flag = 1;
		return v2 * r;
	}
	else
	{
		flag = 0;
		return xx;
	}
}

double gammdev(double alpha)
//OVERVIEW:	generate a standard gamma variate. 
//			alpha is the shape parameter.
//REQUIRES:	alpha > 0
//EFFECTS:	return a random gamma variate
//MODIFIES:	the status of random number generator
{
	double gamma;
	if( alpha <= 0 )
		//violates REQUIRES
	{
		fprintf(stderr, "bug: gammadev: shape parameter must be positive\n");
		exit(0);
	}
	else if (alpha < 1.0)
	{
		gamma = gammadev_1(alpha);
	}
	else if (alpha > 1.0)
	{
		gamma = gammadev_2(alpha);
	}
	else 
		//when alpha=1.0, the gamma distribution becoomes the exponential distribution
	{
		gamma = -log( unifdev() );
	}
	return gamma;
}

double gammadev_1(double alpha)
//OVERVIEW:	generate a standard gamma variate with alpha > 0 && alpha < 1
//			alpha is the shape parameter.
//REQUIRES:	alpha > 0 && alpha < 1
//EFFECTS:	return a random gamma variate
//MODIFIES:	the status of random number generator
//ALGORITHM:rejection algorithms proposed by Ahren, described in Knuth book
//			adapted from the F90 implementation of Alan Miller
//			which is adapted from the F77 code in the book "Principles of random variate generation"
//			by Dagpunar J. Clarendon Press Oxford, 1988. ISBN 0-19-852202-9
{
	static double	vsmall=1.0e-37;
	static double	last_alpha = 100.0;	
	static double	a, p, uf, vr;
	double r, w, c, d, x;
		
	//S1: setup
    if( alpha != last_alpha)
		//as 0<alpha<1, this step will be executed on the first call.
	{
		a = 1.0 - alpha;
		p = a / ( a + alpha * exp( -a ));
		c = 1.0 / alpha;
		uf = p * pow( vsmall/a, alpha);
		vr = 1 - vsmall;
		d = a * log (a);
		last_alpha = alpha;
	}

	for (;;) 
	{
		r = unifdev();
		if ( r >= vr )
		{
			continue;
		}
		else if ( r > p )
		{
			x = a - log ( (1.0-r) / (1.0 - p));
			w = a * log(x) - d;
		}
		else if (r>uf)
		{
			x = a * pow ( r/p, 1.0 /alpha );
			w = x;
		}
		else
		{
			return 0.0;
		}

		r = unifdev();
		if ( 1.0 - r <= w && r > 0.0 )
			if (r*(w + 1.0) >= 1.0 || -log(r) <= w)  
				continue;
		break;
	}
	return x; 
}

double gammadev_2(double alpha)
//OVERVIEW:	generate a standard gamma variate with alpha > 1
//			alpha is the shape parameter.
//REQUIRES:	alpha > 1
//EFFECTS:	return a random gamma variate
//MODIFIES:	the status of random number generator
//REFERENCE:Marsgalia. G. and Tsang, W.W (2000) 
//			'A simple method for generating gamma variables'
//			Trans. on Math. Software (TOMS). vol.26(3). pp363-372
//ALGORITHM:
//	S1: d = alpha - 1/3;	c = 1/sqrt(9*d)
//	S2:	generate x~N(0,1); v = (1 + x * c)^3; repeat until v>0
//	S3: u~U(0,1)
//	S4:	if u < 1-0.0331 * x^4 return d * v;
//	S5:	if log(u)< 0.5*x^2 + d - d*v + d*log(v) return d * v
//	S6:	goto S2
{
	static double last_alpha = 1.0;			//track if alpha chage to save the S1
	static double c=0.408248,d=0.666667;	//initialized as alpha=1.0
	double x, u, v;

	//S1
	if( alpha != last_alpha)
		//initialize d and c
	{
		d = alpha - 1.0/3.0;
		c = 1.0/sqrt(9.0*d);
		last_alpha = alpha;
	}

	for(;;)
	{
		//S2
		do
		{
			x = normdev();
			v = 1.0 + c * x;
		}while(v<=0);

		//S3
		v = v * v * v;
		u = unifdev();

		//S4
		if ( u < 1.0 - 0.0331 * (x * x) * (x * x) )
			return d * v;

		//S5
		if ( log(u) < 0.5*x*x + d*(1.0 - v + log(v)) )
			return (d*v);
	}
}

double randgama(double alpha, double beta)
//OVERVIEW:	generate a gamma variate with parameter (alpha, beta)
//			alpha is the shape parameter.
//			beta is the scale parameter
//REQUIRES:	alpha > 0, beta > 0
//EFFECTS:	return a random gamma variate with mean alpha/beta
//			and variance alpha/beta^2
//MODIFIES:	the status of random number generator
{
	return gammdev(alpha)/beta;
}

void randdirichlet(int n, double *alpha, double *x)
//OVERVIEW:	generate dirichlet variates with control parameters alpha[i], i=0..n-1
//REQUIRES:	n >= 1 and alpha[i]>=0 for i=0..n-1
//EFFECTS:	x[i] (i=0..n-1) are filled with dirichlet variates
//MODIFIES:	the status of random number generator
//ALGORITHM:
//	S1:	generate n independent random gamma variates y[i]
//	S2: sum = sum_i=0^i=n (Y[i])
//	S2:	set x[i] = y[i]/sum	
{
	int i;
	double sum = 0.0;
	for (i=0; i<n; i++)
	{
        x[i] = gammdev( alpha[i] );
		sum += x[i];
	}

	for (i=0; i<n; i++)
	{
		x[i] /= sum;
	}
}


double randbeta(double alpha, double beta)
//OVERVIEW:	generate beta variates with control parameters alpha and beta
//REQUIRES:	alpha>0 and beta>0
//EFFECTS:	return a beta deviate
//MODIFIES:	the status of random number generator
//ALGORITHM:
//	S1:	generate two random gamma variates x and y with order alpha and beta respectively
//	S2: return x/(x+y)
//REFERENCE: D. Knuth, Seminumerical Algorithms (1998), Vol. 2, p. 134.
{
	double x, y;
	x = gammdev(alpha);
	y = gammdev(beta);
	return x/(x+y);
}

double	randnormal(double mu, double theta)
//OVERVIEW:	generate normal variates with mean mu and variance theta^2
//REQUIRES:	theta>0
//EFFECTS:	return a normal deviate
//MODIFIES:	the status of random number generator
{
    return normdev()*theta + mu;
}

double	randexponential(double lamda)
//OVERVIEW:	generate exponential variates with mean 1/lamda
//REQUIRES:	lamda>0
//EFFECTS:	return a expoential deviate
//MODIFIES:	the status of random number generator
{
	return -log(unifdev())/lamda;
}

long factorial(int n)
{
   long f, i;
   if (n>10) bug("n>10 in factorial");
   for (i=2,f=1; i<=(long)n; i++) f*=i;
   return (f);
}

double LnGamma (double x)
{
/* returns ln(gamma(x)) for x>0, accurate to 10 decimal places.
   Stirling's formula is used for the central polynomial part of the procedure.

   Pike MC & Hill ID (1966) Algorithm 291: Logarithm of the gamma function.
   Communications of the Association for Computing Machinery, 9:684
*/
   double f=0, fneg=0, z, lng;
   int nx=(int)x-1;

   if((double)nx==x && nx>0 && nx<10)
      lng=log((double)factorial(nx));
   else {
      if(x<=0) {
		 bug("lnGamma not implemented for x<0"); 
         if((int)x-x==0) { puts("lnGamma undefined"); return(-1); }
         for (fneg=1; x<0; x++) fneg/=x;
         if(fneg<0) bug("strange!! check lngamma");
         fneg=log(fneg);
      }
      if (x<7) {
         f=1;  z=x-1;
         while (++z<7)  f*=z;
         x=z;   f=-log(f);
      }
      z = 1/(x*x);
      lng = fneg+ f + (x-0.5)*log(x) - x + .918938533204673 
             + (((-.000595238095238*z+.000793650793651)*z-.002777777777778)*z
                  +.083333333333333)/x;
   }
   return  lng;
}

double IncompleteGamma (double x, double alpha, double ln_gamma_alpha)
{
/* returns the incomplete gamma ratio I(x,alpha) where x is the upper 
           limit of the integration and alpha is the shape parameter.
   returns (-1) if in error
   ln_gamma_alpha = ln(Gamma(alpha)), is almost redundant.
   (1) series expansion     if (alpha>x || x<=1)
   (2) continued fraction   otherwise
   RATNEST FORTRAN by
   Bhattacharjee GP (1970) The incomplete gamma integral.  Applied Statistics,
   19: 285-287 (AS32)
*/
   int i;
   double p=alpha, g=ln_gamma_alpha;
   /* double accurate=1e-8, overflow=1e30; */
   double accurate=1e-10, overflow=1e60;
   double factor, gin=0, rn=0, a=0,b=0,an=0,dif=0, term=0, pn[6];

   if (x==0) return (0);
   if (x<0 || p<=0) return (-1);

   factor=exp(p*log(x)-x-g);   
   if (x>1 && x>=p) goto l30;
   /* (1) series expansion */
   gin=1;  term=1;  rn=p;
 l20:
   rn++;
   term*=x/rn;   gin+=term;

   if (term > accurate) goto l20;
   gin*=factor/p;
   goto l50;
 l30:
   /* (2) continued fraction */
   a=1-p;   b=a+x+1;  term=0;
   pn[0]=1;  pn[1]=x;  pn[2]=x+1;  pn[3]=x*b;
   gin=pn[2]/pn[3];
 l32:
   a++;  b+=2;  term++;   an=a*term;
   for (i=0; i<2; i++) pn[i+4]=b*pn[i+2]-an*pn[i];
   if (pn[5] == 0) goto l35;
   rn=pn[4]/pn[5];   dif=fabs(gin-rn);
   if (dif>accurate) goto l34;
   if (dif<=accurate*rn) goto l42;
 l34:
   gin=rn;
 l35:
   for (i=0; i<4; i++) pn[i]=pn[i+2];
   if (fabs(pn[4]) < overflow) goto l32;
   for (i=0; i<4; i++) pn[i]/=overflow;
   goto l32;
 l42:
   gin=1-factor*gin;

 l50:
   return (gin);
}

double	PointChi2 (double prob, double v)
{
/* returns z so that Prob{x<z}=prob where x is Chi2 distributed with df=v
   returns -1 if in error.   0.000002<prob<0.999998
   RATNEST FORTRAN by
       Best DJ & Roberts DE (1975) The percentage points of the 
       Chi2 distribution.  Applied Statistics 24: 385-388.  (AS91)
   Converted into C by Ziheng Yang, Oct. 1993.
*/
   double e=.5e-6, aa=.6931471805, p=prob, g, small=1e-6;
   double xx, c, ch, a=0,q=0,p1=0,p2=0,t=0,x=0,b=0,s1,s2,s3,s4,s5,s6;

   if (p<small)   return(0);
   if (p>1-small) return(9999);
   if (v<=0)      return (-1);

   g = LnGamma (v/2);
   xx=v/2;   c=xx-1;
   if (v >= -1.24*log(p)) goto l1;

   ch=pow((p*xx*exp(g+xx*aa)), 1/xx);
   if (ch-e<0) return (ch);
   goto l4;
l1:
   if (v>.32) goto l3;
   ch=0.4;   a=log(1-p);
l2:
   q=ch;  p1=1+ch*(4.67+ch);  p2=ch*(6.73+ch*(6.66+ch));
   t=-0.5+(4.67+2*ch)/p1 - (6.73+ch*(13.32+3*ch))/p2;
   ch-=(1-exp(a+g+.5*ch+c*aa)*p2/p1)/t;
   if (fabs(q/ch-1)-.01 <= 0) goto l4;
   else                       goto l2;
  
l3: 
   x=PointNormal (p);
   p1=0.222222/v;   ch=v*pow((x*sqrt(p1)+1-p1), 3.0);
   if (ch>2.2*v+6)  ch=-2*(log(1-p)-c*log(.5*ch)+g);
l4:
   q=ch;   p1=.5*ch;
   if ((t=IncompleteGamma (p1, xx, g))<0)
      bug ("\nIncompleteGamma");
   p2=p-t;
   t=p2*exp(xx*aa+g+p1-c*log(ch));   
   b=t/ch;  a=0.5*t-b*c;

   s1=(210+a*(140+a*(105+a*(84+a*(70+60*a))))) / 420;
   s2=(420+a*(735+a*(966+a*(1141+1278*a))))/2520;
   s3=(210+a*(462+a*(707+932*a)))/2520;
   s4=(252+a*(672+1182*a)+c*(294+a*(889+1740*a)))/5040;
   s5=(84+264*a+c*(175+606*a))/2520;
   s6=(120+c*(346+127*c))/5040;
   ch+=t*(1+0.5*t*s1-b*c*(s1-b*(s2-b*(s3-b*(s4-b*(s5-b*s6))))));
   if (fabs(q/ch-1) > e) goto l4;

   return (ch);
}

double	PointNormal(double prob)
//OVERVIEW:	compute the percentage point of normal distribution
//			i.e. return Z such that prob(x<Z) = prob.
//			code is adapted from MrBayes 3.0
//REQUIRES:	1.e-12 < prob < 1-1.e-12
//EFFECTS:	return the percentage point of the normal distribution with mean 0 and variance 1
//MODIFIES:	none
/*---------------------------------------------------------------------------------
|
|   PointNormal
|
|   Returns z so That Prob{x<z} = prob where x ~ N(0,1) and
|   (1e-12) < prob < 1-(1e-12).  Returns (-9999) if in error. 
|
|   Odeh, R. E. and J. O. Evans.  1974.  The percentage points of the normal
|     distribution.  Applied Statistics, 22:96-97 (AS70).
|
|   Newer methods:
|
|   Wichura, M. J.  1988.  Algorithm AS 241: The percentage points of the
|      normal distribution.  37:477-484.
|   Beasley, JD & S. G. Springer.  1977.  Algorithm AS 111: The percentage
|      points of the normal distribution.  26:118-121.
|
---------------------------------------------------------------------------------*/
{

	double 		a0 = -0.322232431088, a1 = -1.0, a2 = -0.342242088547, a3 = -0.0204231210245,
 					a4 = -0.453642210148e-4, b0 = 0.0993484626060, b1 = 0.588581570495,
 					b2 = 0.531103462366, b3 = 0.103537752850, b4 = 0.0038560700634,
 					y, z = 0, p = prob, p1;

	p1 = (p<0.5 ? p : 1-p);
	if (p1<1e-20) 
	   return (-9999.0);
	y = sqrt (log(1/(p1*p1)));   
	z = y + ((((y*a4+a3)*y+a2)*y+a1)*y+a0) / ((((y*b4+b3)*y+b2)*y+b1)*y+b0);

	return (p<0.5 ? -z : z);
}

//int DiscreteGamma (double freqK[], double rK[], double alpha, double beta, int K, int median)
int DiscreteGamma (double alpha, double beta, int median, int K, double rK[], double freqK[])
//OVERVIEW:  discretization of gamma distribution with equal proportions in each category
//			alpha, beta:	(double)	the control parameter of gamma function
//			median:			(boolean)	use the median to represent the average rate
//			K:				(integer)	the number of categories
//			rK:				(double[])	the rates for each category
//			freqK:			(double[])	the propability of each rate = 1/K
//REQUIRES:	alpha>0, beta>0, K>=1, median=0|1
//EFFECTS:	rK[] is filled with rates of each category, the probability of each category is 1/k
{
   int i;
   double t, factor=alpha/beta*K, lnga1;

   if (median) {
	   //use median to represent the rate
	   double gap = 1./(2.0*K);
	   for(i=0; i<K; i++)
		   rK[i] = PointGamma( (i*2.0+1)*gap, alpha, beta);
	   t = 0.0;
	   for(i=0; i<K; i++)
		   t += rK[i];
	   for(i=0; i<K; i++)
		   rK[i] *= factor/t;
   }
   else {
   	   //use the average rate
      lnga1=LnGamma(alpha+1);	

	  for (i=0; i<K-1; i++) /* cutting points, Eq. 9 */
         freqK[i]=PointGamma((i+1.0)/K, alpha, beta);

      for (i=0; i<K-1; i++) /* Eq. 10 */
         freqK[i]=IncompleteGamma(freqK[i]*beta, alpha+1, lnga1);

      rK[0] = freqK[0]*factor;
      rK[K-1] = (1-freqK[K-2])*factor;
      for (i=1; i<K-1; i++)  rK[i] = (freqK[i]-freqK[i-1])*factor;
   }

   for (i=0; i<K; i++) freqK[i]=1.0/K;
   return (0);
}

int histogram(int n, double *X, double Xmin, double Xmax, int m, double *Xbin, int *Cbin)
//OVERVIEW:  summarize the data with histogram
//			n:		the number of samples
//			X[]:	the samples	i=0..n-1
//			Xmin:	the lower bound of the samples
//			Xmax:	the upper bound of the samples
//			m:		the number of bins
//			Xbin[]:	the range of each bin	X[i]<x<X[i+1]	i=0..m
//			Cbin[]:	the count of each bin	
//REQUIRES:	n>0, m>0, m>=1, Xmin<Xmax
//EFFECTS:	histogram information is stored in Xbin[] and Cbin[]
{
	int i, k;
    double dX, delta=1.0e-10;
	double dXI;

	 
	if(Xmin>=Xmax)
		return -1;	//illegal range

	dX = (Xmax - Xmin)/m;
	dXI = 1.0 / dX;
	Xbin[0] = Xmin; Cbin[0] = 0;
	for(i=1; i<m; i++) 
	{
		Cbin[i] = 0;
		Xbin[i] = Xbin[i-1] + dX;
	}
	Xbin[m] = Xmax + delta;

	for(i=0; i<n; i++)
	{
		if( X[i] >= Xmin && X[i] <= Xmax )
		{
			k = (int)((X[i] - Xmin) * dXI);
			Cbin[k]++;
		}
	}
	return 0;
}


int basicstatistic(int n, double *X, double *Xmin, double *Xmax, double *Xmean, double *Xvar2)
//OVERVIEW:  summarize the data with basic statistics
//			Xmin:		the minimum value
//			Xmax:		the maximum value
//			Xmean:		the mean of the samples
//			Xvar:		the variance
//REQUIRES:	n>0
//EFFECTS:	basic statiics information is computed
{
    int i;
	double xmin, xmax, xmean, xvar2;

	xmin = X[0];
	xmax = X[0];
	xmean = X[0];
	for(i=1; i<n; i++)
	{
		xmean += X[i];
		if( X[i] < xmin ) xmin = X[i];
		if( X[i] > xmax ) xmax = X[i];
		
	}
	printf("mean=%g\n", xmean/n);
	printf("min=%g\n", xmin);

	*Xmin = xmin;
	*Xmax = xmax;
	xmean/=n;
	*Xmean = xmean;

	xvar2 = 0.0;
	for(i=0; i<n; i++)
	{
		xvar2 += (X[i]-xmean) * (X[i]-xmean);
	}
	xvar2 /= (n-1);
	*Xvar2 = xvar2;

	printf("%10.2g\t%10.2g\t%10.2g\t%10.2g\n", *Xmin, *Xmax, xmean, *Xvar2);
	return 0;
}


//generate a random integer r which falls between lbound and rbound,ibound<=r<=rbound
int	irand(int lbound, int rbound)
{
	double x =floor(unifdev()*(rbound-lbound+1));
	return lbound+(int)x;
	/*
	double x = unifdev();
	x = lbound + x * (rbound - lbound) + 0.5;
	*/

	/*
	double x;
	x = rand()/(RAND_MAX+1.0);
	x = lbound + x * (rbound - lbound) + 0.5;
	return (int)x;
	*/
}

//function:		creatPermutation( int theArray[], int size)
//description:	compute a random permutation for array of integers <A[]> with size <size>
//algorithm:	random-in-place algorithm
//requires:		theArray[] holds the list of integers
//effects:		theArray[] holds the permutation of the input list of integers
void	creatPermutation(int *theArray, int size)
{
    int i, j, tmp;

	assert( theArray );

	//init A
	for(i=0; i<size; i++)
		theArray[i] = i;

	for(i=0; i<size; i++){	
		//swap A[i] and A[irand(i, size)]
        j = irand(i, size-1);	
		tmp = theArray[i]; 			
		theArray[i] = theArray[j];	
		theArray[j] = tmp;
    }
}

