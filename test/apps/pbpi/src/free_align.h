/* --------------------------------------------------------------  */
/* (C)Copyright 2001,2006,                                         */
/* International Business Machines Corporation,                    */
/* Sony Computer Entertainment, Incorporated,                      */
/* Toshiba Corporation,                                            */
/*                                                                 */
/* All Rights Reserved.                                            */
/* --------------------------------------------------------------  */
/* PROLOG END TAG zYx                                              */
#ifndef _FREE_ALIGN_H_
#define _FREE_ALIGN_H_	1

#include <stdlib.h>

/* Function
 *
 *	void free_align(void *ptr)
 *
 * Description
 * 	The free_align routine frees a memory buffer allocate by the
 *	malloc_align routine. See malloc_align for complete details.
 */

static __inline void _free_align(void *ptr)
{
  void * real;

  if (ptr) {
    real = *((void **)(ptr)-1);
    free(real);
  }
}

#endif /* _FREE_ALIGN_H_ */
