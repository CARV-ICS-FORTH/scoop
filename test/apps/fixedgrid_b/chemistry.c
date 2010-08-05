/*
 *  chemistry.c
 *  fixedgrid_serial
 *
 *  Created by John Linford on 6/23/08.
 *  Copyright 2008 Transatlantic Giraffe. All rights reserved.
 *
 */

#include <stdio.h>
#include "chemistry.h"
#include "saprc99_Global.h"

int Rosenbrock( double Y[], double Tstart, double Tend,
               double AbsTol[],  double RelTol[],
               double RPAR[], int IPAR[]);

/**
 * Applies saprc99 chemical mechanism to all chemical species
 */
void saprc99_chem(fixedgrid_t* G)
{
#if DO_CHEMISTRY == 1
    int i, j, k;
    
    /* Integration method statistics.
     * (Used to detect limit violation.) */
    static int stats[8];
    
    /* Integration method parameters */
    double RPAR[20];
    int    IPAR[20];
    int    IERR;
        
    /* Chemistry buffer */
    double buff[NSPEC];
    
    /* Initialize method globals */
    TIME = G->time;
    DT = G->dt;
    
    /* Initalize parameters */
    for(i=0; i<20; i++)
    {
        IPAR[i] = 0;
        RPAR[i] = 0.0;
    }
    IPAR[0] = 0;        /* non-autonomous */
    IPAR[1] = 1;        /* scalar tolerances */
    RPAR[2] = STEPMIN;  /* starting step */
    IPAR[3] = 5;        /* method selection: Rodas4 */
    
    timer_start(&G->metrics.chem);
    for(i=0; i<NROWS; i++)
    {
        printf("Chem on row %d of %d...\n", i, NROWS);
        for(j=0; j<NCOLS; j++)
        {
            for(k=0; k<NSPEC; k++)
            {
                buff[k] = G->conc[k][i][j];
            }
            
            /* Point method at current data */
            C   = &buff[0];
            VAR = &buff[0];
            FIX = &buff[NFIXST];
            
            /* Reset statistics for each integration */
            for(k=0; k<8; k++)
            {
                IPAR[10+k] = stats[k];
            }
            
            /* Integrate */
            IERR = Rosenbrock(VAR, TIME, TIME+DT, ATOL, RTOL, RPAR, IPAR);
            
            if(IERR < 0)
            {
                printf("\n Rosenbrock: Unsucessful step at T=%g: IERR=%d\n", TIME, IERR);
            }            
            
            for(k=0; k<NSPEC; k++)
            {
                G->conc[k][i][j] = buff[k];
            }
        }
    }
    
    /* Record final statistics */
    for(k=0; k<8; k++)
    {
        stats[k] = IPAR[10+k];
    }
    
    /* Record last step for next method invocation */
    STEPMIN = RPAR[11];
    
    timer_stop(&G->metrics.chem);
#endif
}

