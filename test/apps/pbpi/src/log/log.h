/* --------------------------------------------------------------  */
/* (C)Copyright 2001,2006,                                         */
/* International Business Machines Corporation,                    */
/* Sony Computer Entertainment, Incorporated,                      */
/* Toshiba Corporation,                                            */
/*                                                                 */
/* All Rights Reserved.                                            */
/* --------------------------------------------------------------  */
/* PROLOG END TAG zYx                                              */

#ifndef _LOG_H_
#define _LOG_H_	1

#include <spu_intrinsics.h>
#include "log2_v.h"

#ifndef M_LN2
#define M_LN2		0.69314718055994530942
#endif /* M_LN2 */


/*
 * FUNCTION
 *	double _log(double x)
 *
 * DESCRIPTION
 *	_log computes the natural log of the input x. log is
 *     computed using log2 as follows:
 *
 *	log(x) = log2(x) / log2(e) = log2(x) * log(2)
 */

static __inline double _log(double x)
{
  return (spu_extract(_log2_v(spu_promote(x, 0)), 0) * M_LN2);
}

#endif /* _LOG_H_ */
