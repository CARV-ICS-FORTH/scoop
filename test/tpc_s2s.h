#if defined PPU || SPU
  #define __VEC__
  #define __ALTIVEC__
  #define TBR 79800000.0

  #if defined STATISTICS && PPU
  // Statistics
    #define READ_TIME_REG(var)    \
    {                             \
      (var) = __mftb();           \
    }
  #else
    #define READ_TIME_REG(var)  
  #endif
#endif

#if defined PPU
//   #define vector __attribute__((altivec(vector__)))
  #define __vector __attribute__((altivec(vector__)))
  #define __powerpc64__
  #include <ppu_intrinsics.h>
  #include <altivec.h>
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
  #include "tpc_skeleton_cell.c"
#elif defined SPU
  #define vector __attribute__((spu_vector))
  #define __vector __attribute__((spu_vector))
vector signed char __builtin_si_frest(vector signed char);
vector signed char __builtin_si_fi(vector signed char, vector signed char);
vector signed char __builtin_si_frsqest(vector signed char);
void __builtin_si_wrch(int, vector signed char);
vector signed char __builtin_si_rdch(int);
vector signed char __builtin_si_from_ptr(void *);
vector signed char __builtin_si_from_uint(unsigned int);
unsigned int __builtin_si_to_uint(vector signed char);
//  #include <spu_intrinsics.h>
  #include "include/tpc_common.h"
  #include "include/tpc_spe.h"
extern struct tpc_spe_statistics_t *G_spe_stats[MAX_SPES];  // in LS
#else
// x86
  #include "tpc_skeleton_x86.c"
#endif


