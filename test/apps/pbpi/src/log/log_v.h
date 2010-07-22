/* --------------------------------------------------------------  */
/* (C)Copyright 2001,2006,                                         */
/* International Business Machines Corporation,                    */
/* Sony Computer Entertainment, Incorporated,                      */
/* Toshiba Corporation,                                            */
/*                                                                 */
/* All Rights Reserved.                                            */
/* --------------------------------------------------------------  */
/* PROLOG END TAG zYx                                              */

#ifndef _LOG_V_H_
#define _LOG_V_H_	1

#include <spu_intrinsics.h>
#include "log2_v.h"
#include <vec_literal.h>


#ifndef M_LN2
#define M_LN2		0.69314718055994530942
#endif /* M_LN2 */


/*
 * FUNCTION
 *	vector double _log_v(vector double x)
 *
 * DESCRIPTION
 *	_log_v computes the natural log for each double word element
 *	of the input x. log_v is computed using log2_v as follows:
 *
 *	log_v(x) = log2_v(x) / log2_v(e) = log2_v(x) * log_v(2)
 */

static __inline vector double _log_v(vector double x)
{
  return (spu_mul(_log2_v(x), VEC_SPLAT_F64(M_LN2)));
}

#endif /* _LOG_V_H_ */
