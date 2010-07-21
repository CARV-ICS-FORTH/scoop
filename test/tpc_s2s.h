#define __VEC__
#define __ALTIVEC__
#define TBR 79800000.0

#if defined STATISTICS
// Statistics
  #define READ_TIME_REG(var)    \
  {                             \
    (var) = __mftb();           \
  }
#else
   #define READ_TIME_REG(var)  
#endif

#if defined PPU
  #define vector __attribute__((altivec(vector__)))
  #define __vector __attribute__((altivec(vector__)))
  #include "ppu_intrinsics.h"
  #include "include/tpc_common.h"
  #include "include/tpc_ppe.h"
extern int s_available_spe;
extern unsigned int G_max_spes;
extern struct completions_status_t *compl_queue[MAX_SPES];
extern int task_queue_tail[MAX_SPES] __attribute__ ((aligned (128)));
extern int g_task_current_id[MAX_SPES] __attribute__ ((aligned (64)));
extern unsigned int g_task_id_queue[MAX_SPES][MAX_QUEUE_ENTRIES] __attribute__ ((aligned (128)));
extern queue_entry_t *task_queue[MAX_SPES];
extern struct tpc_ppe_statistics_t  G_ppe_stats;  // in MM
  #include "tpc_skeleton_tpc.c"
#elif defined SPU
  #define vector __attribute__((spu_vector))
  #define __vector __attribute__((spu_vector))
  #include "include/tpc_common.h"
  #include "include/tpc_spe.h"
extern struct tpc_spe_statistics_t *G_spe_stats[MAX_SPES];  // in LS
#endif


