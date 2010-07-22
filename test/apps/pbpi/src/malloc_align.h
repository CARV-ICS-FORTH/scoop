/* --------------------------------------------------------------  */
/* (C)Copyright 2001,2006,                                         */
/* International Business Machines Corporation,                    */
/* Sony Computer Entertainment, Incorporated,                      */
/* Toshiba Corporation,                                            */
/*                                                                 */
/* All Rights Reserved.                                            */
/* --------------------------------------------------------------  */
/* PROLOG END TAG zYx                                              */

#ifndef _MALLOC_ALIGN_H_
#define _MALLOC_ALIGN_H_	1

#include <stdlib.h>

/* Function
 *
 *	void * malloc_align(size_t size, unsigned int log2_align)
 *
 * Description
 * 	The malloc_align routine allocates a memory buffer of <size>
 * 	bytes aligned to the power of 2 alignment specified by <log2_align>.
 *	For example, malloc_align(4096, 7) will allocate a memory heap 
 *	buffer of 4096 bytes aligned on a 128 byte boundary.
 *
 * 	The aligned malloc routine allocates an enlarged buffer
 * 	from the standard memory heap. Space for the real allocated memory
 * 	pointer is reserved on the front of the memory buffer.
 *
 *	      ----------------- <--- start of allocated memory
 *	     |    pad 0 to     |
 *	     |(1<<log2_align)-1|
 *	     |     bytes       |
 *	     |-----------------|
 *	     | real buffer ptr |
 *	     |-----------------|<---- returned aligned memory pointer
 *	     |                 |
 *	     |    requested    |
 *	     |     memory      |
 *	     |     buffer      |
 *	     |      size       |
 *	     |      bytes      |
 *	     |_________________|
 *
 *      Memory allocated by this routine must be freed using the free_align
 *	routine.
 */

static __inline void * _malloc_align(size_t size, unsigned int log2_align)
{
  void *ret;
  char *real;
  unsigned long offset;
  unsigned long align;
  
  align = 1 << log2_align;
  real = (char *)malloc(size + sizeof(void *) + (align-1));
  if (real) {
    offset = (align - (unsigned long)(real + sizeof(void *))) & (align-1);
    ret = (void *)(real + sizeof(void *)) + offset;
    *((void **)(ret)-1) = (void *)(real);
  } else {
    ret = (void *)(real);
  }
  return (ret);
}

#endif /* _MALLOC_ALIGN_H_ */
