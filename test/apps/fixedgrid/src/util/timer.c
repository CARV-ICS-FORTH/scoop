/*
 *  timer.c
 *  
 *  Common timer functionality
 *
 *  Created by John Linford on 4/8/08.
 *  Copyright 2008 Transatlantic Giraffe. All rights reserved.
 *
 */

#include <stdio.h>
#include <string.h>
#include <sys/time.h>

#include "timer.h"

/* Human-readable labels for timers.
 * MUST appear in the same order as the timer declarations
 * in the metrics_t type definition.
 */
char* timer_names[NUM_TIMERS] = 
{
    "Wallclock    ",
    "Array Init   ",
    "Row Array Copy",
    "Col Array Copy",
    "File I/O     ",
    "Row discret  ",
    "Col discret  ",
    "Row kernel   ",
    "Col kernel   ",
    "Chemistry    "
};

float elapsed_time()
{
	static int sec = -1;
	struct timeval tv;
	gettimeofday(&tv, 0);
	if(sec < 0) sec = tv.tv_sec;
	return (tv.tv_sec - sec) + 1.0e-6*tv.tv_usec;
}

void metrics_init( metrics_t* m, char* name)
{
    int i;
    
    stopwatch_t* tptr = (stopwatch_t*)m;
    
    for(i=0; i<NUM_TIMERS; i++, tptr++)
    {
        tptr->start = 0.0;
        tptr->elapsed = 0.0;
    }
    
    strncpy(m->name, name, 2*128-NUM_TIMERS*sizeof(stopwatch_t));
}

void print_metrics( metrics_t* m)
{
    int i;
    stopwatch_t* tptr = (stopwatch_t*)m;
    
    printf("\n===== %s =====\n", m->name);
    
    for(i=0; i<NUM_TIMERS; i++, tptr++)
    {
        printf("%s: %f\n", timer_names[i], tptr->elapsed);
    }

    printf("%s: %f\n", "Total Array Copy", m->row_array_copy.elapsed + m->col_array_copy.elapsed);
    printf("%s: %f\n", "Total kernel", m->row_kernel.elapsed + m->col_kernel.elapsed);
}
