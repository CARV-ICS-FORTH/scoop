/*
 *  transport.c
 *  fixedgrid sequoia
 *
 *  Jae-Seung Yeom <jyeom@cs.vt.edu>
 *
 *  Computer Science Dept. VirginiaTech
 *
 */

#include "transport.h"
#include "tpc_common.h"
#if 0

double DXYT[2];

/**
 * Discretize rows 1/2 timestep 
 * @param s     Species index
 */
void discretize_all_rows(fixedgrid_t* G, real_t dt)
{
#if 0
#if DO_ROW_DISCRET == 1
    
    timer_start(&G->metrics.row_discret);
    
    timer_start(&G->metrics.row_kernel);
    ((int*)(sq_dims->ptr))[0] = NROWS; // COL_LENGTH
    ((int*)(sq_dims->ptr))[1] = NSPEC;
    ((int*)(sq_dims->ptr))[2] = NCOLS; // ROW_LENGTH
    ((int*)(sq_dims->ptr))[3] = IS_ROW; // ROW function
    ((double*)(sq_DXYT->ptr))[0] = DX;
    ((double*)(sq_DXYT->ptr))[1] = dt;

    sq_conc->ptr = (void*) ;
    sq_wind->ptr = (void*);
    sq_diff->ptr = (void*)

    discretize_all_entrypoint(sq_dims, sq_DXYT, sq_wind, sq_diff, sq_conc);
    timer_stop(&G->metrics.row_kernel);
    
    timer_stop(&G->metrics.row_discret);
    
#endif
#endif

//void discretize_all_rows_leaf( int sq_dims, double *sq_DXYT, double *sq_wind, double *sq_diff, double *sq_concIn, double *sq_concOut)
 DXYT[0]= DX;
 DXYT[1]= dt;

#if DO_ROW_DISCRET == 1
    timer_start(&G->metrics.row_discret);
    timer_start(&G->metrics.row_kernel);
    discretize_all_rows_leaf( NROWS,  DXYT ,  &(G->wind_u[0][0]),  &(G->diff[0][0]), &(G->conc[0][0][0]),  &(G->conc[0][0][0])  );
    timer_stop(&G->metrics.row_kernel);
    timer_stop(&G->metrics.row_discret);
#endif

}



/**
 * Discretize rows 1/2 timestep 
 * @param s     Species index
 */
void discretize_all_rows(fixedgrid_t* G, real_t dt)
{
#if DO_ROW_DISCRET == 1
    
    int i, j, k;
    
    real_t buff[ROW_LENGTH]  __attribute__((aligned(128)));
    
    
    timer_start(&G->metrics.row_discret);
    
    for(k=0; k<NLOOKAT; k++)
    {
        for(i=0; i<NROWS; i++)
        {
            timer_start(&G->metrics.row_kernel);
            discretize(NCOLS, G->conc[k][i], G->wind_u[i], G->diff[i], DX, dt, buff);
            timer_stop(&G->metrics.row_kernel);
            
            timer_start(&G->metrics.row_array_copy);
            for(j=0; j<NCOLS; j++)
                G->conc[k][i][j] = buff[j];
            timer_stop(&G->metrics.row_array_copy);
        }
    }
    
    timer_stop(&G->metrics.row_discret);
    
#endif
}









/**
 * Discretize colums 1 timestep 
 * @param s     Species index
 */
void discretize_all_cols(fixedgrid_t* G, real_t dt)
{
#if 0
#if DO_COL_DISCRET == 1
    
    int i, j, k;
    
    timer_start(&G->metrics.col_discret);
    

    timer_start(&G->metrics.col_kernel);
    ((int*)(sq_dims->ptr))[0] = NCOLS;
    ((int*)(sq_dims->ptr))[1] = NSPEC;
    ((int*)(sq_dims->ptr))[2] = NROWS;
    ((int*)(sq_dims->ptr))[3] = IS_COL; // COL function
    ((double*)(sq_DXYT->ptr))[0] = DY;
    ((double*)(sq_DXYT->ptr))[1] = dt;

    sq_conc->ptr = (void*) &(G->conc[0][0][0]);
    sq_wind->ptr = (void*) &(G->wind_v[0][0]);
    sq_diff->ptr = (void*) &(G->diff[0][0]);

    discretize_all_entrypoint(sq_dims, sq_DXYT, sq_wind, sq_diff, sq_conc);
    timer_stop(&G->metrics.col_kernel);
    
    timer_stop(&G->metrics.col_discret);
    
#endif
#endif
 DXYT[0]= DY;
 DXYT[1]= dt;
#if DO_COL_DISCRET == 1
  timer_start(&G->metrics.col_discret);
  timer_start(&G->metrics.col_kernel);
  discretize_all_rows_leaf( NROWS,  DXYT ,  &(G->wind_u[0][0]),  &(G->diff[0][0]), &(G->conc[0][0][0]),  &(G->conc[0][0][0])  );
  timer_stop(&G->metrics.col_kernel);
  timer_stop(&G->metrics.col_discret);
#endif

}


#include "params.h"


/* 
 * The core upwinded advection/diffusion equation.
 * c = conc, w = wind, d = diff
 * x2l is the 2-left neighbor of x, etc.
 * x2r is the 2-right neighbor of x, etc.
 * out = output variable
 */

inline real_t                                  \
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

/*
 * Applies the advection / diffusion equation to scalar data
 */
void space_advec_diff(int n,  \
                      real_t *c,         \
                      real_t *w,         \
                      real_t *d,         \
                      real_t *cb,        \
                      real_t *wb,        \
                      real_t *db,        \
                      real_t cell_size,  \
                      real_t *dcdx)      
{
    int i;
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







void discretize_leaf( int SZ, double *DXYT, double *wind,   double *diff,   double *concIn,    double *concOut )
{
    int i;



    double cell_size = DXYT[0];
    double dt = DXYT[1];

    double ctemp[SZ] __attribute__((aligned(128)));
    double dcdx[SZ] __attribute__((aligned(128)));
    double cbound[4] __attribute__((aligned(128)));
    double wbound[4] __attribute__((aligned(128)));
    double dbound[4] __attribute__((aligned(128)));

    cbound[0] = concIn[SZ-2];
    cbound[1] = concIn[SZ-1];
    cbound[2] = concIn[0];
    cbound[3] = concIn[1];
    wbound[0] = wind[SZ-2];
    wbound[1] = wind[SZ-1];
    wbound[2] = wind[0];
    wbound[3] = wind[1];
    dbound[0] = diff[SZ-2];
    dbound[1] = diff[SZ-1];
    dbound[2] = diff[0];
    dbound[3] = diff[1];

    for(i=0; i<SZ; i++)
    {
        ctemp[i] = concOut[i] = concIn[i];
    }

    space_advec_diff(SZ, concIn, wind, diff, cbound, wbound, dbound, cell_size, dcdx);

    for(i=0; i<SZ; i++)
        ctemp[i] += dt*dcdx[i];

    space_advec_diff(SZ, ctemp, wind, diff, cbound, wbound, dbound, cell_size, dcdx);

    for(i=0; i<SZ; i++)
        ctemp[i] += dt*dcdx[i];

    for(i=0; i<SZ; i++)
    {
        concOut[i] = 0.5 * (concOut[i] + ctemp[i]);
        if(concOut[i] < 0.0)
            concOut[i] = 0.0;
    }
}
void discretize_all_rows_leaf( int sq_dims, double *sq_DXYT, double *sq_wind, double *sq_diff, double *sq_concIn, double *sq_concOut)
{
   discretize_leaf(sq_dims, sq_DXYT, sq_wind, sq_diff, sq_concIn, sq_concOut);
}

void discretize_all_cols_leaf( int sq_dims, double *sq_DXYT, double *sq_wind, double *sq_diff, double *sq_concIn, double *sq_concOut)
{
    discretize_leaf(sq_dims, sq_DXYT, sq_wind, sq_diff, sq_concIn, sq_concOut);
}

#endif


/**
 * Discretize rows 1/2 timestep 
 * @param s     Species index
 */
void discretize_all_rows(fixedgrid_t* G, real_t dt)
{
#if DO_ROW_DISCRET == 1
    
    int i, j, k;

#if 1

    real_t data[2] __attribute__((aligned(16)));

    data[0] = DX;
    data[1] = dt;
    timer_start(&G->metrics.row_discret);

    timer_start(&G->metrics.row_kernel);
    for(k=0; k<NLOOKAT; k++)
    {
        for(i=0; i<NROWS; i++)
        {

            tpc_call( 1, 4,
                    G->conc[k][i], NCOLS*sizeof(real_t), TPC_INOUT_ARG,
                    G->wind_u[i], NCOLS*sizeof(real_t) ,TPC_IN_ARG,
                    G->diff[i], NCOLS*sizeof(real_t) ,TPC_IN_ARG,
                    data, 16, TPC_IN_ARG
                    );
        }
    }

    tpc_wait_all();
    timer_stop(&G->metrics.row_kernel);
#else

    real_t buff[ROW_LENGTH]  __attribute__((aligned(128)));
    
    
    timer_start(&G->metrics.row_discret);
    
    for(k=0; k<NLOOKAT; k++)
    {
        for(i=0; i<NROWS; i++)
        {
            timer_start(&G->metrics.row_kernel);

            discretize(NCOLS, G->conc[k][i], G->wind_u[i], G->diff[i], DX, dt, buff);

            timer_stop(&G->metrics.row_kernel);
            timer_start(&G->metrics.row_array_copy);

            for(j=0; j<NCOLS; j++)
                G->conc[k][i][j] = buff[j];

            timer_stop(&G->metrics.row_array_copy);
        }
    }
    
#endif

    timer_stop(&G->metrics.row_discret);

#endif
}

/**
 * Discretize colums 1 timestep 
 * @param s     Species index
 */
void discretize_all_cols(fixedgrid_t* G, real_t dt)
{
#if DO_COL_DISCRET == 1
    
    int i, j, k;
    
    /* Buffers */
    //real_t buff[COL_LENGTH]  __attribute__((aligned(128)));


#if 1

    real_t data[2] __attribute__((aligned(16)));

    data[0] = DY;
    data[1] = dt;




    timer_start(&G->metrics.col_discret);
    
    timer_start(&G->metrics.col_array_copy);

    uint32_t task_ids[NLOOKAT][NCOLS];


#define BLOCK_SIZE 60

    static uint32_t desc[4] __attribute__((aligned(16))) = {BLOCK_SIZE, BLOCK_SIZE, BLOCK_SIZE, BLOCK_SIZE };


    for(k=0; k<NLOOKAT; k++){
	    for(j=0; j<NCOLS; j+=BLOCK_SIZE){
		    for(i=0; i<NROWS; i+=BLOCK_SIZE){
			    tpc_call(
					    3, 3,

					    &G->conc_T[k][j][i],
					    TPC_BUILD_STRIDEARG(BLOCK_SIZE, BLOCK_SIZE*sizeof(real_t)),
					    TPC_STRIDE_ARG|TPC_OUT_ARG, NROWS*sizeof(real_t),

					    &G->conc[k][i][j],
					    TPC_BUILD_STRIDEARG(BLOCK_SIZE, BLOCK_SIZE*sizeof(real_t)),
					    TPC_STRIDE_ARG|TPC_IN_ARG, NCOLS*sizeof(real_t),

					    desc,
					    4*sizeof(uint32_t), TPC_IN_ARG
				    );
		    }
	    }

    }


    for(j=0; j<NCOLS; j+=BLOCK_SIZE){
	    for(i=0; i<NROWS; i+=BLOCK_SIZE){
		    tpc_call(
				    3, 3,

				    &G->wind_T[j][i],
				    TPC_BUILD_STRIDEARG(BLOCK_SIZE, BLOCK_SIZE*sizeof(real_t)),
				    TPC_STRIDE_ARG|TPC_OUT_ARG, NROWS*sizeof(real_t),

				    &G->wind_v[i][j],
				    TPC_BUILD_STRIDEARG(BLOCK_SIZE, BLOCK_SIZE*sizeof(real_t)),
				    TPC_STRIDE_ARG|TPC_IN_ARG, NCOLS*sizeof(real_t),

				    desc,
				    4*sizeof(uint32_t), TPC_IN_ARG
			    );
	    }
    }

    for(j=0; j<NCOLS; j+=BLOCK_SIZE){
	    for(i=0; i<NROWS; i+=BLOCK_SIZE){
		    tpc_call(
				    3, 3,

				    &G->diff_T[j][i],
				    TPC_BUILD_STRIDEARG(BLOCK_SIZE, BLOCK_SIZE*sizeof(real_t)),
				    TPC_STRIDE_ARG|TPC_OUT_ARG, NROWS*sizeof(real_t),

				    &G->diff[i][j],
				    TPC_BUILD_STRIDEARG(BLOCK_SIZE, BLOCK_SIZE*sizeof(real_t)),
				    TPC_STRIDE_ARG|TPC_IN_ARG, NCOLS*sizeof(real_t),

				    desc,
				    4*sizeof(uint32_t), TPC_IN_ARG
			    );
	    }
    }

    tpc_wait_all();

    timer_stop(&G->metrics.col_array_copy);

    timer_start(&G->metrics.col_kernel);
    for(k=0; k<NLOOKAT; k++)
    {
        for(j=0; j<NCOLS; j++)
        {
           task_ids[k][j] = tpc_call( 1, 4,
                    G->conc_T[k][j], NROWS*sizeof(real_t), TPC_INOUT_ARG,
                    G->wind_T[j], NROWS*sizeof(real_t) ,TPC_IN_ARG,
                    G->diff_T[j], NROWS*sizeof(real_t) ,TPC_IN_ARG,
                    data, 16, TPC_IN_ARG
                    );;

        }
    }
    timer_stop(&G->metrics.col_kernel);
    timer_start(&G->metrics.col_array_copy);

    for(k=0; k<NLOOKAT; k++){
	    for(j=0; j<NCOLS; j+=BLOCK_SIZE){
		    tpc_wait(task_ids[k][j]);
		    for(i=0; i<NROWS; i+=BLOCK_SIZE){
			    tpc_call(
					    3, 3,

					    &G->conc[k][i][j],
					    TPC_BUILD_STRIDEARG(BLOCK_SIZE, BLOCK_SIZE*sizeof(real_t)),
					    TPC_STRIDE_ARG|TPC_OUT_ARG, NCOLS*sizeof(real_t),

					    &G->conc_T[k][j][i],
					    TPC_BUILD_STRIDEARG(BLOCK_SIZE, BLOCK_SIZE*sizeof(real_t)),
					    TPC_STRIDE_ARG|TPC_IN_ARG, NROWS*sizeof(real_t),

					    desc,
					    4*sizeof(uint32_t), TPC_IN_ARG
				    );
		    }
	    }

    }
    tpc_wait_all();

    timer_stop(&G->metrics.col_array_copy);
    timer_stop(&G->metrics.col_discret);

#else

    timer_start(&G->metrics.col_discret);
    
    timer_start(&G->metrics.col_array_copy);

    for(k=0; k<NLOOKAT; k++)
        for(j=0; j<NCOLS; j++)
            for(i=0; i<NROWS; i++)
                G->conc_T[k][j][i] = G->conc[k][i][j];

    for(j=0; j<NCOLS; j++)
        for(i=0; i<NROWS; i++)
            G->wind_T[j][i]  = G->wind_v[i][j];

    for(j=0; j<NCOLS; j++)
        for(i=0; i<NROWS; i++)
            G->diff_T[j][i]  = G->diff[i][j];

    timer_stop(&G->metrics.col_array_copy);

    for(k=0; k<NLOOKAT; k++)
    {
        for(j=0; j<NCOLS; j++)
        {
            timer_start(&G->metrics.col_kernel);
            discretize(NROWS, G->conc_T[k][j], G->wind_T[j], G->diff_T[j], DY, dt, G->conc_T[k][j]);
            timer_stop(&G->metrics.col_kernel);
        }
    }
    
    timer_start(&G->metrics.col_array_copy);
    for(k=0; k<NLOOKAT; k++)
    {
        for(j=0; j<NCOLS; j++)
        {
            for(i=0; i<NROWS; i++)
                G->conc[k][i][j] = G->conc_T[k][j][i];
        }
    }
    timer_stop(&G->metrics.col_array_copy);
    timer_stop(&G->metrics.col_discret);
#endif

#endif
}

extern fixedgrid_t G_GLOBAL;

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
                      real_t *c, 
                      real_t *w, 
                      real_t *d, 
                      real_t *cb, 
                      real_t *wb, 
                      real_t *db, 
                      real_t cell_size, 
                      real_t *dcdx)
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


void discretize(const int n, real_t *conc_in, real_t *wind, real_t *diff, real_t cell_size, real_t dt, real_t *conc_out)
{
    int i;
    real_t c[n];
    real_t dcdx[n];

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

    for(i=0; i<n; i++)
    {
        c[i] = conc_out[i] = conc_in[i];
    }

    space_advec_diff(n, conc_in, wind, diff, concbound, windbound, diffbound, cell_size, dcdx);
    
    for(i=0; i<n; i++)
        c[i] += dt*dcdx[i];
    
    space_advec_diff(n, c, wind, diff, concbound, windbound, diffbound, cell_size, dcdx);

    for(i=0; i<n; i++)
        c[i] += dt*dcdx[i];
    
    for(i=0; i<n; i++)
    {
        conc_out[i] = 0.5 * (conc_out[i] + c[i]);
        if(conc_out[i] < 0.0)
            conc_out[i] = 0.0;
    }
}


