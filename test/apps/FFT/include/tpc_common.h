#ifndef _TPC_COMMON_H_
#define _TPC_COMMON_H_

#include <stdio.h>
#include <stdint.h>

#ifdef CIL
  #define vector __attribute__((altivec(vector__)))
#endif

#ifndef MAX_QUEUE_ENTRIES
#define MAX_QUEUE_ENTRIES 32U	// Maximum entries
#endif
#define MAX_ARGS	  18U	// Maximum number of arguments per RPC
#define MAX_SPES	  16U

// Bit-masks for supported argument flags and helping macros
#define TPC_IN_ARG      0x1U
#define TPC_OUT_ARG     0x2U
#define TPC_INOUT_ARG   0x3U
#define TPC_STRIDE_ARG	0x4U

#define TPC_IS_INARG(x)    ((x) & TPC_IN_ARG)
#define TPC_IS_OUTARG(x)   ((x) & TPC_OUT_ARG)
#define TPC_IS_INOUTARG(x) ((x) == TPC_INOUT_ARG)
#define TPC_IS_STRIDEARG(x) ((x) & TPC_STRIDE_ARG)

// elems  : number of elements
// elemsz : size of each element
#define TPC_BUILD_STRIDEARG(elems, elemsz)    (((elems)<<16U) | (elemsz))
#define TPC_EXTRACT_STRIDEARG_ELEMS(arg)      ((arg) >> 16U)
#define TPC_EXTRACT_STRIDEARG_ELEMSZ(arg)     ((arg) & 0x0000FFFFU)


#define ceil16(v)   (((v) + 15U) & ~15U)
#define ceil128(v)   (((v) + 127U) & ~127U)
#define ceil4096(v)   (((v) + 4095U) & ~4095U)

enum compl_status {WAITING=0, COMPLETED=1};
//enum queue_status {WAIT, FETCHED, COMPLETED};
enum entry_status {INACTIVE=0, ACTIVE=1};


// Describes an argument triplet.
struct tpc_arg_element {
  uint32_t flag;	/* args flag bitmask */
  uint32_t size;        /* Transfer size */
  uint32_t eal;         /* Lower word of ea of argument */
  uint32_t stride;      /* Stride size in case of STRIDE argument. */
			/* Stride is ignored otherwise */
} __attribute__ ((aligned (16)));


struct queue_entry {
  struct tpc_arg_element arguments[MAX_ARGS];
  uint32_t ea_upper;
  uint8_t funcid;
  uint8_t total_arguments;
  //uint8_t flag;
  //uint8_t status;
  enum entry_status active;
} __attribute__ ((aligned (16)));

typedef struct queue_entry queue_entry_t ;

struct completions_status_t {
  enum compl_status status;
  int return_value;
} __attribute__ ((aligned (128)));



// Structs for statistical information

struct tpc_ppe_statistics_t {
  // Time statistics
  uint64_t stalled_ticks;
  uint64_t issue_ticks;
  uint64_t wait_ticks;

  // Count statistics
  uint32_t stat_total_blocks;
  uint64_t stat_tpc_per_spe[MAX_SPES];
  uint64_t bytes_per_spe[MAX_SPES];
} __attribute__ ((aligned (128)));


struct tpc_spe_statistics_t {
  uint64_t idle_ticks;
  uint64_t lib_ticks;
  uint64_t argsin_ticks;
  uint64_t argsout_ticks;
  uint64_t task_ticks;
  uint64_t final_ticks;
  uint64_t prefetch_count;
  uint64_t fetch_count;
  vector unsigned int idle_flag;
} __attribute__ ((aligned (128)));



#endif
