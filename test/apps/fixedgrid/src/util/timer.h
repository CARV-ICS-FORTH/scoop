/*
 *  timer.h
 *  
 *  Common timer functionality
 *
 *  Created by John Linford on 4/8/08.
 *  Copyright 2008 Transatlantic Giraffe. All rights reserved.
 *
 */

#ifndef __TIMER_H__
#define __TIMER_H__

/**************************************************
 * Includes                                       *
 **************************************************/

#include <stdint.h>

/**************************************************
 * Macros                                         *
 **************************************************/

#define NUM_TIMERS 10

/**************************************************
 * Data types                                     *
 **************************************************/

/* Stopwtch for gathering metrics */
typedef struct stopwatch
{
    float start;
    float elapsed;
} stopwatch_t;

/* Thread metrics */
typedef struct metrics
{
    stopwatch_t wallclock;
    stopwatch_t array_init;
    stopwatch_t row_array_copy;
    stopwatch_t col_array_copy;
    stopwatch_t file_io;
    stopwatch_t row_discret;
    stopwatch_t col_discret;
    stopwatch_t row_kernel;
    stopwatch_t col_kernel;
    stopwatch_t chem;
    char name[2*128-NUM_TIMERS*sizeof(stopwatch_t)];    
} metrics_t;

/**************************************************
 * Globals                                        *
 **************************************************/

extern char* timer_names[NUM_TIMERS];

/**************************************************
 * Function Prototypes                            *
 **************************************************/

float elapsed_time();

void metrics_init( metrics_t* m, char* name);

/**************************************************
 * Inline fuctions                                *
 **************************************************/

static inline void timer_start( stopwatch_t* t)
{
	t->start = elapsed_time();
}

static inline void timer_stop( stopwatch_t* t)
{
    t->elapsed += elapsed_time() - t->start;
}

static inline int64_t year2sec(int32_t years)
{
    return years * 31556926;
}

static inline int32_t day2sec(int32_t days)
{
    return days * 86400;
}

static inline int32_t hour2sec(int32_t hours)
{
    return hours * 3600;
}

static inline int32_t minute2sec(int32_t minutes)
{
    return minutes * 60;
}

static inline int32_t sec2year(int64_t seconds)
{
    return seconds / 31556926;
}

static inline int32_t sec2day(int32_t seconds)
{
    return seconds / 86400;
}

static inline int32_t sec2hour(int32_t seconds)
{
    return seconds / 3600;
}

static inline int32_t sec2minute(int32_t seconds)
{
    return seconds / 60;
}

#endif
