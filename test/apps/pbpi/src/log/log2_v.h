/* --------------------------------------------------------------  */
/* (C)Copyright 2001,2006,                                         */
/* International Business Machines Corporation,                    */
/* Sony Computer Entertainment, Incorporated,                      */
/* Toshiba Corporation,                                            */
/*                                                                 */
/* All Rights Reserved.                                            */
/* --------------------------------------------------------------  */
/* PROLOG END TAG zYx                                              */

#ifndef _LOG2_V_H_
#define _LOG2_V_H_	1

#ifdef __SPU__
#include <spu_intrinsics.h>
#endif
#include "vec_literal.h"

/*
 * FUNCTION
 *	vector double _log2_v(vector double x)
 *
 * DESCRIPTION
 *	_log2_v computes log base 2 of the input x for each
 *	of the double word elements of x. The log2 is decomposed
 *      into two parts, log2 of the exponent and log2 of the 
 *	fraction. The log2 of the fraction is approximated 
 *	using a 21st order polynomial of the form:
 *
 *                        __20_
 *                        \
 *	log(x) = x * (1 +  \   (Ci * x^i))
 *                         /
 *                        /____
 *                         i=0
 *
 *      for x in the range 0-1.
 */


#define LOG_C00    0.44269504088896339E+00
#define LOG_C01   -7.21347520444469934E-01
#define LOG_C02    4.80898346961226595E-01
#define LOG_C03   -3.60673760117245982E-01
#define LOG_C04    2.88539004851839364E-01
#define LOG_C05   -2.40449108727688962E-01
#define LOG_C06    2.06098446037376922E-01
#define LOG_C07   -1.80329036970820794E-01
#define LOG_C08    1.60245637034704267E-01
#define LOG_C09   -1.43988260692073185E-01
#define LOG_C10    1.30009193360025350E-01
#define LOG_C11   -1.16530490533844182E-01
#define LOG_C12    1.01392360727236079E-01
#define LOG_C13   -8.27937055456904317E-02
#define LOG_C14    6.08335872067172597E-02
#define LOG_C15   -3.84093543662501949E-02
#define LOG_C16    1.98461565426430164E-02
#define LOG_C17   -7.93793829370930689E-03
#define LOG_C18    2.28193656337578229E-03
#define LOG_C19   -4.16662127033480827E-04
#define LOG_C20    3.61276447184348752E-05


static __inline vector double _log2_v(vector double vx) 
{
   vec_int4 addval;
   vec_ullong2 exp_mask = VEC_SPLAT_U64(0x7FF0000000000000ULL);
   vec_double2 vy, vxw;
   vec_double2 v1 = VEC_SPLAT_F64(1.0);
   vec_double2 x2, x4, x8, x10, p1, p2;

   /* Extract the fraction component of input by forcing
    * its exponent so that input is in the range [1.0, 2.0)
    * and then subtract 1.0 to force it in the range 
    * [0.0, 1.0).
    */
   vxw = spu_sub(spu_sel(vx, v1, exp_mask), v1);

   /* Compute the log2 of the exponent as exp - 1023.
    */
   addval = spu_add(spu_rlmask((vec_int4)vx, -20), -1023);

   /* Compute the log2 of the fractional component using a 21st 
    * order polynomial. The polynomial is evaluated in two halves 
    * to improve efficiency.
    */
   p1 = spu_madd(VEC_SPLAT_F64(LOG_C20), vxw, VEC_SPLAT_F64(LOG_C19));
   p2 = spu_madd(VEC_SPLAT_F64(LOG_C09), vxw, VEC_SPLAT_F64(LOG_C08));
   p1 = spu_madd(vxw, p1, VEC_SPLAT_F64(LOG_C18));
   p2 = spu_madd(vxw, p2, VEC_SPLAT_F64(LOG_C07));
   p1 = spu_madd(vxw, p1, VEC_SPLAT_F64(LOG_C17));
   p2 = spu_madd(vxw, p2, VEC_SPLAT_F64(LOG_C06));
   p1 = spu_madd(vxw, p1, VEC_SPLAT_F64(LOG_C16));
   p2 = spu_madd(vxw, p2, VEC_SPLAT_F64(LOG_C05));
   p1 = spu_madd(vxw, p1, VEC_SPLAT_F64(LOG_C15));
   p2 = spu_madd(vxw, p2, VEC_SPLAT_F64(LOG_C04));
   p1 = spu_madd(vxw, p1, VEC_SPLAT_F64(LOG_C14));
   p2 = spu_madd(vxw, p2, VEC_SPLAT_F64(LOG_C03));
   p1 = spu_madd(vxw, p1, VEC_SPLAT_F64(LOG_C13));
   p2 = spu_madd(vxw, p2, VEC_SPLAT_F64(LOG_C02));
   p1 = spu_madd(vxw, p1, VEC_SPLAT_F64(LOG_C12));
   p2 = spu_madd(vxw, p2, VEC_SPLAT_F64(LOG_C01));
   p1 = spu_madd(vxw, p1, VEC_SPLAT_F64(LOG_C11));
   p2 = spu_madd(vxw, p2, VEC_SPLAT_F64(LOG_C00));
   p1 = spu_madd(vxw, p1, VEC_SPLAT_F64(LOG_C10));

   x2 = spu_mul(vxw, vxw);
   x4 = spu_mul(x2, x2);
   x8 = spu_mul(x4, x4);
   x10 = spu_mul(x8, x2);

   vy = spu_madd(spu_madd(x10, p1, p2), vxw, vxw);

   /* Add the log2(exponent) and the log2(fraction) to 
    * compute the final result.
    */
   vy = spu_add(vy, spu_extend(spu_convtf(addval, 0))); 

   vxw = spu_extend(spu_convtf(addval, 20));

   return(vy);
}

#endif /* _LOG2_V_H_ */
