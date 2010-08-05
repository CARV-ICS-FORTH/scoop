/*
 *  fixedgrid.h
 *
 *  Created by John Linford on 4/8/08.
 *  Copyright 2008 Transatlantic Giraffe. All rights reserved.
 *
 */

#ifndef __FIXEDGRID_H__
#define __FIXEDGRID_H__

/**************************************************
 * Includes                                       *
 **************************************************/

#include <stdint.h>

#include "params.h"
#include "timer.h"


#define ALIGN(x,a) (((x)+((a)-1))&~((a)-1))


/**************************************************
 * Macros                                         *
 **************************************************/

#define TRUE  1
#define FALSE 0


/**************************************************
 * Data types                                     *
 **************************************************/


/* Program state (global variables) */
typedef struct fixedgrid
{
    /* 2D concentration data: NROWS x NCOLS x NSPEC */
    real_t conc[NSPEC][NROWS][ROW_LENGTH] __attribute__((aligned(128)));

    /* 2D wind field data: NROWS x NCOLS */
    real_t wind_u[NROWS][ROW_LENGTH] __attribute__((aligned(128)));
    real_t wind_v[NROWS][ROW_LENGTH] __attribute__((aligned(128)));

    /* 2D diffusion tensor data: NROWS x NCOLS */
    real_t diff[NROWS][ROW_LENGTH] __attribute__((aligned(128)));


    real_t conc_T[NSPEC][NCOLS][COL_LENGTH] __attribute__((aligned(128)));
    real_t wind_T[NCOLS][COL_LENGTH] __attribute__((aligned(128)));
    real_t diff_T[NCOLS][COL_LENGTH] __attribute__((aligned(128)));
    
    /* Time (seconds) */
    real_t time;
    real_t tstart;
    real_t tend;
    real_t dt;
    
    /* Parallelization */
    /* This is always == 1 for serial code */
    uint32_t nprocs;
    
    /* Metrics */
    metrics_t metrics;
    
} fixedgrid_t;


#endif
