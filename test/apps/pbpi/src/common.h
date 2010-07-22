#ifndef _COMMON_H_
#define _COMMON_H_

#define MAX_PROCS_NO 16
#define MAX_LP0_IT 1360
#define MAX_LP1_IT 1008
#define MAX_LP2_IT 3632

#define __PS3__
#undef __PS3__

#define true 1
#define false 0

struct tpc_ds{
	unsigned int start, end, spe_no, vcted;
} __attribute__ ((aligned(16)));

#ifdef __powerpc64__
#define __mftb() __extension__                  \
  ({ unsigned long long result;                 \
  __asm__ volatile ("mftb %0" : "=r" (result)); \
  result; })
#else
#define __mftb() __extension__                  \
  ({ unsigned long long result;                 \
  unsigned long t;                              \
  __asm__ volatile ("1:\n"                      \
                    "\tmftbu %0\n"              \
                    "\tmftb %L0\n"              \
                    "\tmftbu %1\n"              \
                    "\tcmpw %0,%1\n"            \
                    "\tbne 1b"                  \
                    : "=r" (result), "=r" (t)); \
  result; })
#endif /* __powerpc64__ */

#ifdef __PS3__
#define TIMER_FREQ 79800000
#else
#define TIMER_FREQ 14318000
#endif /* TIMER_FREQ */

#endif
