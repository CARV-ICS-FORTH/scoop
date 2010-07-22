/*
 *  transport.h
 *  fixedgrid_serial
 *
 *  Created by John Linford on 6/23/08.
 *  Copyright 2008 Transatlantic Giraffe. All rights reserved.
 *
 */

#ifndef __TRANSPORT_H__
#define __TRANSPORT_H__

#include "fixedgrid.h"
#include "params.h"
#include "timer.h"

void discretize_all_rows(fixedgrid_t* G, real_t dt);

void discretize_all_cols(fixedgrid_t* G, real_t dt);

#endif

