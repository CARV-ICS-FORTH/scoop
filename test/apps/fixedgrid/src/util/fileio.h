/*
 *  fileio.h
 *  
 *  File I/O functions.
 *
 *  Created by John Linford on 4/8/08.
 *  Copyright 2008 Transatlantic Giraffe. All rights reserved.
 *
 */

#ifndef __FILEIO_H__
#define __FILEIO_H__

/**************************************************
 * Includes                                       *
 **************************************************/

#include <stdint.h>
#include "fixedgrid.h"

/**************************************************
 * Function Prototypes                            *
 **************************************************/

void write_metrics_as_csv(fixedgrid_t* G, char* platform);

void write_conc(fixedgrid_t* G, uint32_t iter, uint32_t proc);

void print_metrics( metrics_t* m);

#endif
