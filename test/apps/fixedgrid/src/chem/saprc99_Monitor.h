/*
 *  saprc99_Monitor.h
 *  
 *
 *  Created by John Linford on 4/11/08.
 *  Copyright 2008 Transatlantic Giraffe. All rights reserved.
 *
 */

#ifndef __SAPRC99_MONITOR_H__
#define __SAPRC99_MONITOR_H__

/**************************************************
 * Includes                                       *
 **************************************************/

#include "saprc99_Parameters.h"

/**************************************************
 * External Definitions                           *
 **************************************************/

extern int LOOKAT[NLOOKAT];                     /* Indexes of species to look at */
extern int MONITOR[NMONITOR];                   /* Indexes of species to monitor */
extern char * SPC_NAMES[];                      /* Names of chemical species */
extern char * EQN_NAMES[NREACT];                /* Equation names */

#endif
