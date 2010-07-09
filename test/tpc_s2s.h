#ifdef CIL
	#define __VEC__
	#define __ALTIVEC__
	#define vector __attribute__((vector_size(8)))
	#define __vector __attribute__((vector_size(8)))
	#define TBR 79800000.0
#endif

#include "include/tpc_common.h"

extern unsigned int G_max_spes;
extern int s_available_spe;
extern struct completions_status_t *compl_queue[MAX_SPES];
extern int task_queue_tail[MAX_SPES] __attribute__ ((aligned (128)));
extern int g_task_current_id[MAX_SPES] __attribute__ ((aligned (64)));
extern unsigned int g_task_id_queue[MAX_SPES][MAX_QUEUE_ENTRIES] __attribute__ ((aligned (128)));
extern queue_entry_t *task_queue[MAX_SPES];
extern struct tpc_ppe_statistics_t  G_ppe_stats;  // in MM
extern struct tpc_spe_statistics_t *G_spe_stats[MAX_SPES];  // in LS

#if defined STATISTICS && CIL
// Statistics
	#define READ_TIME_REG(var)     \
	{  				  \
   		(var) = __mftb(); \
   	}  				  \

#else

   #define READ_TIME_REG(var)  

#endif
