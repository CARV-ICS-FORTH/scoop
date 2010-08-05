#include <stdio.h>
#include <spu_intrinsics.h>
#include <spu_mfcio.h>

#include <simdmath.h>

#include "../../libtpc/include/tpc_common.h"
#include "../../libtpc/include/tpc_spe.h"


#include "config/precision.h"

#define SYNC()  __asm__ __volatile__ ("sync" : : : "memory");

#if 0

/* Kernels for discretize */

/* 
 * The core upwinded advection/diffusion equation.
 * c = conc, w = wind, d = diff
 * x2l is the 2-left neighbor of x, etc.
 * x2r is the 2-right neighbor of x, etc.
 * out = output variable
 */
#if 0
static inline real_t                                  \
advec_diff(real_t cell_size,                   \
           real_t c2l, real_t w2l, real_t d2l, \
           real_t c1l, real_t w1l, real_t d1l, \
           real_t   c, real_t   w, real_t   d, \
           real_t c1r, real_t w1r, real_t d1r, \
           real_t c2r, real_t w2r, real_t d2r)
{
    real_t wind, diff_term, advec_term, advec_termL, advec_termR;
    
    wind = (w1l + w) / 2.0;
    if(wind >= 0.0) advec_termL = (1.0/6.0) * ( -c2l + 5.0*c1l + 2.0*c );
    else advec_termL = (1.0/6.0) * ( 2.0*c1l + 5.0*c - c1r );
    advec_termL *= wind;
    wind = (w1r + w) / 2.0;
    if(wind >= 0.0) advec_termR = (1.0/6.0) * ( -c1l + 5.0*c + 2.0*c1r );
    else advec_termR = (1.0/6.0) * ( 2.0*c + 5.0*c1r - c2r );
    advec_termR *= wind;
    advec_term = (advec_termL - advec_termR) / cell_size;
    diff_term = ( ((d1l+d)/2)*(c1l-c) - ((d+d1r)/2)*(c-c1r) ) / (cell_size * cell_size);
    return advec_term + diff_term;
}
#else

static inline real_t                                  \
advec_diff(const real_t cell_size,                   \
           real_t c2l, real_t w2l, real_t d2l, \
           real_t c1l, real_t w1l, real_t d1l, \
           real_t   c, real_t   w, real_t   d, \
           real_t c1r, real_t w1r, real_t d1r, \
           real_t c2r, real_t w2r, real_t d2r)
{
    real_t diff_term, advec_term, advec_termL, advec_termR;
    real_t windl, windr,  advec_termL_0,  advec_termL_1, advec_termR_0, advec_termR_1     ;
    real_t tmp0, tmp1, tmp2;
    real_t selectL[2];
    real_t selectR[2];
    uint32_t resL;
    uint32_t resR;

    windl = (w1l + w) / 2.0;
    windr = (w1r + w) / 2.0;

    selectL[1] =  (1.0/6.0) * ( -c2l + 5.0*c1l + 2.0*c );
    selectL[0] =  (1.0/6.0) * ( 2.0*c1l + 5.0*c - c1r );

    selectR[1] = (1.0/6.0) * ( -c1l + 5.0*c + 2.0*c1r );
    selectR[0] = (1.0/6.0) * ( 2.0*c + 5.0*c1r - c2r );

     
    resL = ((windl>=0.0));
    resR = ((windr>=0.0));
    

    advec_termL = selectL[resL];
    advec_termR = selectR[resR];


    advec_termL *= windl;
    advec_termR *= windr;

    advec_term = (advec_termL - advec_termR) / cell_size;
    diff_term = ( ((d1l+d)/2)*(c1l-c) - ((d+d1r)/2)*(c-c1r) ) / (cell_size * cell_size);


    return advec_term + diff_term;
}

#endif
static inline vector real_t 
advec_diff_v(const vector real_t cell_size,
             vector real_t *c2l, vector real_t *w2l, vector real_t *d2l, 
             vector real_t *c1l, vector real_t *w1l, vector real_t *d1l, 
             vector real_t   *c, vector real_t   *w, vector real_t   *d, 
             vector real_t *c1r, vector real_t *w1r, vector real_t *d1r, 
             vector real_t *c2r, vector real_t *w2r, vector real_t *d2r)
{    
    vector real_t acc1, acc2, acc3;
    vector real_t wind, diff_term, advec_term;
    vector real_t advec_term_pos, advec_term_neg;
    vector real_t advec_termR, advec_termL;
    
    const vector real_t FIVE  = SPLAT_CONST(5.0);
    const vector real_t TWO   = SPLAT_CONST(2.0);
    const vector real_t ZERO  = SPLAT_CONST(0.0);
    const vector real_t HALF  = SPLAT_CONST(0.5);
    const vector real_t SIXTH = SPLAT_CONST(1.0/6.0);

   /* wind = (w1l + w) / 2.0; */
    acc1 = spu_add(*w1l, *w);
    wind = spu_mul(acc1, HALF);

    // if(wind >= 0.0) advec_termL = (1.0/6.0) * ( -c2l + 5.0*c1l + 2.0*c );
    // else advec_termL = (1.0/6.0) * ( 2.0*c1l + 5.0*c - c1r );

    acc1 = spu_mul(*c1l, FIVE);
    acc2 = spu_mul(*c, TWO);
    advec_term_pos = spu_add(acc1, acc2);
    advec_term_pos = spu_sub(advec_term_pos, *c2l);
    acc1 = spu_mul(*c1l, TWO);
    acc2 = spu_mul(*c, FIVE);
    advec_term_neg = spu_add(acc1, acc2);
    advec_term_neg = spu_sub(advec_term_neg, *c1r);
    acc1 = (vector real_t)spu_cmpgt(wind, ZERO);
    acc1 = spu_and(acc1, advec_term_pos);
    acc2 = (vector real_t)spu_cmpgt(ZERO, wind);
    acc2 = spu_and(acc2, advec_term_neg);
    advec_termL = spu_add(acc1, acc2);
    advec_termL = spu_mul(advec_termL, SIXTH);

    /* advec_termL *= wind; */
    advec_termL = spu_mul(advec_termL, wind);

    /* wind = (w1r + w) / 2.0; */
    acc1 = spu_add(*w1r, *w);
    wind = spu_mul(acc1, HALF);

    // if(wind >= 0.0) advec_termR = (1.0/6.0) * ( -c1l + 5.0*c + 2.0*c1r );
    // else advec_termR = (1.0/6.0) * ( 2.0*c + 5.0*c1r - c2r );
    acc1 = spu_mul(*c, FIVE);
    acc2 = spu_mul(*c1r, TWO);
    advec_term_pos = spu_add(acc1, acc2);
    advec_term_pos = spu_sub(advec_term_pos, *c1l);
    acc1 = spu_mul(*c, TWO);
    acc2 = spu_mul(*c1r, FIVE);
    advec_term_neg = spu_add(acc1, acc2);
    advec_term_neg = spu_sub(advec_term_neg, *c2r);
    acc1 = (vector real_t)spu_cmpgt(wind, ZERO);
    acc1 = spu_and(acc1, advec_term_pos);
    acc2 = (vector real_t)spu_cmpgt(ZERO, wind);
    acc2 = spu_and(acc2, advec_term_neg);
    advec_termR = spu_add(acc1, acc2);
    advec_termR = spu_mul(advec_termR, SIXTH);
    advec_termR = spu_mul(advec_termR, wind);



    // advec_term = (advec_termL - advec_termR) / cell_size;
    acc1 = spu_sub(advec_termL, advec_termR);
    advec_term = VEC_DIVIDE(acc1, cell_size);


    // diff_term = ( ((d1l+d)/2)*(c1l-c) - ((d+d1r)/2)*(c-c1r) ) / (cell_size * cell_size);
    acc1 = spu_add(*d1l, *d);
    acc1 = spu_mul(acc1, HALF);
    acc3 = spu_sub(*c1l, *c);
    acc1 = spu_mul(acc1, acc3);
    acc2 = spu_add(*d, *d1r);
    acc2 = spu_mul(acc2, HALF);
    acc3 = spu_sub(*c, *c1r);
    acc2 = spu_mul(acc2, acc3);
    acc1 = spu_sub(acc1, acc2);
    acc2 = spu_mul(cell_size, cell_size);
    diff_term = VEC_DIVIDE(acc1, acc2);


    return spu_add(advec_term, diff_term);
}



/*
 * Applies the advection / diffusion equation to scalar data
 */
static void inline space_advec_diff(int n,  \
                      real_t *c,         \
                      real_t *w,         \
                      real_t *d,         \
                      real_t *cb,        \
                      real_t *wb,        \
                      real_t *db,        \
                      real_t cell_size,  \
                      real_t *dcdx)      
{
    int i,j;
    vector real_t cell_size_v = spu_splats( cell_size);
    vector real_t *dcdx_v ;

    /* Do boundary cell c[0] explicitly */
    dcdx[0] = advec_diff(cell_size,
                         cb[0], wb[0], db[0],  /* 2-left neighbors */
                         cb[1], wb[1], db[1],  /* 1-left neighbors */
                         c[0], w[0], d[0],     /* Values */
                         c[1], w[1], d[1],     /* 1-right neighbors */
                         c[2], w[2], d[2]);    /* 2-right neighbors */
    
    /* Do boundary cell c[1] explicitly */    
    dcdx[1] = advec_diff(cell_size,
                         cb[1], wb[1], db[1],  /* 2-left neighbors */
                         cb[2], wb[2], db[2],  /* 1-left neighbors */
                         c[1], w[1], d[1],     /* Values */
                         c[2], w[2], d[2],     /* 1-right neighbors */
                         c[3], w[3], d[3]);    /* 2-right neighbors */
    for(i=2; i<n-2; i++){

			dcdx[i] = advec_diff(cell_size,
					c[i-2], w[i-2], d[i-2],  /* 2-left neighbors */
					c[i-1], w[i-1], d[i-1],  /* 1-left neighbors */
					c[i],   w[i],   d[i],    /* Values */
					c[i+1], w[i+1], d[i+1],  /* 1-right neighbors */
					c[i+2], w[i+2], d[i+2]); /* 2-right neighbors */
    }
    
    /* Do boundary cell c[n-2] explicitly */
    dcdx[n-2] = advec_diff(cell_size,
                           c[n-4], w[n-4], d[n-4],  /* 2-left neighbors */
                           c[n-3], w[n-3], d[n-3],  /* 1-left neighbors */
                           c[n-2], w[n-2], d[n-2],  /* Values */
                           cb[1],  wb[1],  db[1],   /* 1-right neighbors */
                           cb[2],  wb[2],  db[2]);  /* 2-right neighbors */
    
    /* Do boundary cell c[n-1] explicitly */
    dcdx[n-1] = advec_diff(cell_size,
                           c[n-3], w[n-3], d[n-3],  /* 2-left neighbors */
                           c[n-2], w[n-2], d[n-2],  /* 1-left neighbors */
                           c[n-1], w[n-1], d[n-1],  /* Values */
                           cb[2],  wb[2],  db[2],   /* 1-right neighbors */
                           cb[3],  wb[3],  db[3]);  /* 2-right neighbors */
}








static inline void discretize(const int n, real_t *conc_in, real_t *wind, real_t *diff, real_t cell_size, real_t dt, real_t *conc_out)
{
    int i;
    real_t c[n];
    real_t dcdx[n];

    vector real_t *concOut_v ;
    vector real_t *ctemp_v, *dcdx_v;
    vector unsigned long long res;
    const vector real_t multi_v = { 0.5, 0.5};
    const vector real_t dt_v = { dt, dt};
    const vector real_t ZERO  = SPLAT_CONST(0.0);

    real_t concbound[4] __attribute__((aligned(128)));
    real_t windbound[4] __attribute__((aligned(128)));
    real_t diffbound[4] __attribute__((aligned(128)));

    concbound[0] = conc_in[n-2];
    concbound[1] = conc_in[n-1];
    concbound[2] = conc_in[0];
    concbound[3] = conc_in[1];
    windbound[0] = wind[n-2];
    windbound[1] = wind[n-1];
    windbound[2] = wind[0];
    windbound[3] = wind[1];
    diffbound[0] = diff[n-2];
    diffbound[1] = diff[n-1];
    diffbound[2] = diff[0];
    diffbound[3] = diff[1];

    for(i=0; i<n; i+= 16/sizeof(real_t))
    {
	    *(vector unsigned char *) &c[i] = *(vector unsigned char *) &conc_out[i] = *(vector unsigned char *) &conc_in[i];
    }

    space_advec_diff(n, conc_in, wind, diff, concbound, windbound, diffbound, cell_size, dcdx);


    for(i=0; i<n; i+= 16/sizeof(real_t) )
    {
	    ctemp_v = &c[i];
	    dcdx_v = &dcdx[i];
	    *ctemp_v =  ( dt_v * (*dcdx_v)) + *ctemp_v;
    }

    space_advec_diff(n, c, wind, diff, concbound, windbound, diffbound, cell_size, dcdx);


    for(i=0; i<n; i+= 16/sizeof(real_t) )
    {
	    ctemp_v = &c[i];
	    dcdx_v = &dcdx[i];
	    concOut_v = &conc_out[i];

	    *ctemp_v =  ( dt_v * (*dcdx_v)) + *ctemp_v;
	    *concOut_v = multi_v * ( *concOut_v + *ctemp_v );
	    res = spu_cmpgt( *concOut_v, ZERO);
	    *concOut_v = spu_and( *concOut_v, (vector double) res); 
    }



}
#else


//extern fixedgrid_t G_GLOBAL;

/* 
 * The core upwinded advection/diffusion equation.
 * c = conc, w = wind, d = diff
 * x2l is the 2-left neighbor of x, etc.
 * x2r is the 2-right neighbor of x, etc.
 * out = output variable
 */
inline real_t 
advec_diff(real_t cell_size,
           real_t c2l, real_t w2l, real_t d2l, 
           real_t c1l, real_t w1l, real_t d1l, 
           real_t   c, real_t   w, real_t   d, 
           real_t c1r, real_t w1r, real_t d1r, 
           real_t c2r, real_t w2r, real_t d2r)
{
    real_t wind, diff_term, advec_term, advec_termL, advec_termR;
    
    wind = (w1l + w) / 2.0;
    if(wind >= 0.0) advec_termL = (1.0/6.0) * ( -c2l + 5.0*c1l + 2.0*c );
    else advec_termL = (1.0/6.0) * ( 2.0*c1l + 5.0*c - c1r );
    advec_termL *= wind;
    wind = (w1r + w) / 2.0;
    if(wind >= 0.0) advec_termR = (1.0/6.0) * ( -c1l + 5.0*c + 2.0*c1r );
    else advec_termR = (1.0/6.0) * ( 2.0*c + 5.0*c1r - c2r );
    advec_termR *= wind;
    advec_term = (advec_termL - advec_termR) / cell_size;
    diff_term = ( ((d1l+d)/2)*(c1l-c) - ((d+d1r)/2)*(c-c1r) ) / (cell_size * cell_size);
    return advec_term + diff_term;
}

/*
 * Applies the advection / diffusion equation to scalar data
 */
void space_advec_diff(const uint32_t n, 
                      real_t *__restrict__ c, 
                      real_t *__restrict__ w, 
                      real_t *__restrict__ d, 
                      real_t *__restrict__ cb, 
                      real_t *__restrict__ wb, 
                      real_t *__restrict__ db, 
                      real_t cell_size, 
                      real_t *__restrict__ dcdx)
{
    uint32_t i;
    
    /* Do boundary cell c[0] explicitly */
    dcdx[0] = advec_diff(cell_size,
                         cb[0], wb[0], db[0],  /* 2-left neighbors */
                         cb[1], wb[1], db[1],  /* 1-left neighbors */
                         c[0], w[0], d[0],     /* Values */
                         c[1], w[1], d[1],     /* 1-right neighbors */
                         c[2], w[2], d[2]);    /* 2-right neighbors */
    
    /* Do boundary cell c[1] explicitly */    
    dcdx[1] = advec_diff(cell_size,
                         cb[1], wb[1], db[1],  /* 2-left neighbors */
                         cb[2], wb[2], db[2],  /* 1-left neighbors */
                         c[1], w[1], d[1],     /* Values */
                         c[2], w[2], d[2],     /* 1-right neighbors */
                         c[3], w[3], d[3]);    /* 2-right neighbors */
    
    for(i=2; i<n-2; i++)
    {
        dcdx[i] = advec_diff(cell_size,
                             c[i-2], w[i-2], d[i-2],  /* 2-left neighbors */
                             c[i-1], w[i-1], d[i-1],  /* 1-left neighbors */
                             c[i],   w[i],   d[i],    /* Values */
                             c[i+1], w[i+1], d[i+1],  /* 1-right neighbors */
                             c[i+2], w[i+2], d[i+2]); /* 2-right neighbors */
    }
    
    /* Do boundary cell c[n-2] explicitly */
    dcdx[n-2] = advec_diff(cell_size,
                           c[n-4], w[n-4], d[n-4],  /* 2-left neighbors */
                           c[n-3], w[n-3], d[n-3],  /* 1-left neighbors */
                           c[n-2], w[n-2], d[n-2],  /* Values */
                           cb[1],  wb[1],  db[1],   /* 1-right neighbors */
                           cb[2],  wb[2],  db[2]);  /* 2-right neighbors */
    
    /* Do boundary cell c[n-1] explicitly */
    dcdx[n-1] = advec_diff(cell_size,
                           c[n-3], w[n-3], d[n-3],  /* 2-left neighbors */
                           c[n-2], w[n-2], d[n-2],  /* 1-left neighbors */
                           c[n-1], w[n-1], d[n-1],  /* Values */
                           cb[2],  wb[2],  db[2],   /* 1-right neighbors */
                           cb[3],  wb[3],  db[3]);  /* 2-right neighbors */
}

inline void this_vectorizes(const int n, real_t *__restrict__ c, real_t dt, real_t *__restrict__ dcdx)
{
    int i;
    for(i=0; i<n; i++)
        c[i] += dt*dcdx[i];
}

void discretize(const int n, real_t *__restrict__ conc_in, real_t *__restrict__ wind, real_t *__restrict__ diff, real_t cell_size, real_t dt, real_t 
*__restrict__ conc_out)
{
    int i;
    real_t c[n];
    real_t dcdx[n];
//    fixedgrid_t* G = &G_GLOBAL;    

    real_t concbound[4] __attribute__((aligned(128)));
    real_t windbound[4] __attribute__((aligned(128)));
    real_t diffbound[4] __attribute__((aligned(128)));

//    printf ("disc %d %p %p\n", n, conc_in, conc_out);
//    printf ("Starting discretize\n");
//    fflush (0);

    concbound[0] = conc_in[n-2];
    concbound[1] = conc_in[n-1];
    concbound[2] = conc_in[0];
    concbound[3] = conc_in[1];
    windbound[0] = wind[n-2];
    windbound[1] = wind[n-1];
    windbound[2] = wind[0];
    windbound[3] = wind[1];
    diffbound[0] = diff[n-2];
    diffbound[1] = diff[n-1];
    diffbound[2] = diff[0];
    diffbound[3] = diff[1];

    //timer_start(&G->metrics.array_copy);
    for(i=0; i<n; i++)
    {
        c[i] = conc_out[i] = conc_in[i];
    }
    //timer_stop(&G->metrics.array_copy);
//    printf ("Calling space_advec_diff\n");
//    fflush (0);
    space_advec_diff(n, conc_in, wind, diff, concbound, windbound, diffbound, cell_size, dcdx);
//    printf ("Leaving space_advec_diff\n");
//    fflush (0);
    
    for(i=0; i<n; i++)
        c[i] += dt*dcdx[i];
    //this_vectorizes(n,c,dt,dcdx);
    
//    printf ("Calling space_advec_diff2\n");
//    fflush (0);
    space_advec_diff(n, c, wind, diff, concbound, windbound, diffbound, cell_size, dcdx);
//    printf ("Leaving space_advec_diff2\n");
//    fflush (0);
    
    //for(i=0; i<n2; i++)
    //    c[i] += dt*dcdx[i];
    this_vectorizes(n,c,dt,dcdx);
    
    for(i=0; i<n; i++)
    {
        conc_out[i] = 0.5 * (conc_out[i] + c[i]);
        if(conc_out[i] < 0.0)
            conc_out[i] = 0.0;
    }
//    printf ("Leaving discrtize\n");
//    fflush (0);
}

#endif




//pattern0 takes first two values in a double vector
//pattern1 takes second two values in a double vector
const vector unsigned char HIGH={0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17};
const vector unsigned char LOW ={0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F};

/* indexes[0] used for #rows, indexes[1] for #cols */
static inline void SPE_transpose (real_t *dst, real_t *src, uint32_t *indexes){
        uint32_t i, j;
        uint32_t rows = indexes[0];
        uint32_t cols = indexes[1];

#ifndef VECTORIZED
        for(i=0; i<rows; i++){
                for(j=0; j<cols; j++){
                        dst[j*rows+i] = src[i*rows+j];
                }
        }
#else
	register vector double *s0;
	register vector double *s1;
	register vector double *d0;
	register vector double *d1;

	register vector double r0;
	register vector double r1;

        for(i=0; __builtin_expect(i<rows,1); i+=2){
                for(j=0;   __builtin_expect(j<cols,1); j+=2){
			s0 = (vector double *)&src[i*rows+j];
			s1 = (vector double *)&src[(i+1)*rows+j];

			d0 = (vector double *)&dst[j*rows+i];
			d1 = (vector double *)&dst[(j+1)*rows+i];

			r0 = spu_shuffle(*s0, *s1, HIGH);
			r1 = spu_shuffle(*s0, *s1, LOW);

			*d0 = r0;
			*d1 = r1;
			
                }
        }
#endif
}



/* This is entry function of TPC */

int execute_task(queue_entry_t *ex_task, tpc_spe_task_state_t *task_info)
{
    int exit = 0;
    int elems, elem_sz;
    void *arg0, *arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8, *arg9;
    void *arg10, *arg11, *arg12, *arg13, *arg14, *arg15;


    real_t *dtmp0, *dtmp1, *dtmp2, *dtmp3, *dtmp4, *dtmp5;
    int *itmp0, *itmp1, *itmp2, *itmp3;
    int its, i,j;


    switch(ex_task->funcid)
    {
        case 0:       
            break;


            /**
             * Discretize rows 1/2 timestep 
             * @param s     Species index
             */
        case 1:

            arg0 = ( void *) task_info->ls_addr;
            dtmp0 = arg0;

            arg1 = (((void *)arg0) + ex_task->arguments[0].size);
            dtmp1 = arg1;

            arg2 = ((void *)arg1 + ex_task->arguments[1].size);
            dtmp2 = arg2;

            arg3 = ((void *)arg2 + ex_task->arguments[2].size);
            dtmp3 = arg3;



           discretize(  ex_task->arguments[0].size/sizeof(real_t), 
                        dtmp0, 
                        dtmp1, 
                        dtmp2, 
                        dtmp3[0], 
                        dtmp3[1], 
                        dtmp0 );




            break;



        case 2:
            arg0 = ( void *) task_info->ls_addr;
            dtmp0 = arg0;

            arg1 = (((void *)arg0) + ex_task->arguments[0].size);
            dtmp1 = arg1;

            arg2 = ((void *)arg1 + ex_task->arguments[1].size);
            dtmp2 = arg2;

            arg3 = ((void *)arg2 + ex_task->arguments[2].size);
            dtmp3 = arg3;


            discretize(  ex_task->arguments[0].size/sizeof(real_t), 
                    dtmp0, 
                    dtmp1, 
                    dtmp2, 
                    dtmp3[0], 
                    dtmp3[1], 
                    dtmp0 );



                break;

	case 3:
		arg0 = ( void *) task_info->ls_addr;
		dtmp0 = arg0;
	        elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[0].size);
	        elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[0].size);


		arg1 = (((void *)arg0) +  elems*elem_sz );
		dtmp1 = arg1;
	        elems = TPC_EXTRACT_STRIDEARG_ELEMS(ex_task->arguments[1].size);
	        elem_sz = TPC_EXTRACT_STRIDEARG_ELEMSZ(ex_task->arguments[1].size);


		arg2 = ((void *)arg1  +  elems*elem_sz );
		dtmp2 = arg2;

		SPE_transpose ( arg0, arg1, arg2);
		

		break;

 

        default:
            exit = 1;
            fprintf(stderr, ">>>>>>>>>>> EXIT" );
            SYNC();
            break;
    }


    task_info->state = EXECUTED;
    return exit;
}

