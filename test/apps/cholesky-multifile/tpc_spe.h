#ifndef _TPC_SPE_H_
#define _TPC_SPE_H_

#include "tpc_common.h"

#include <spu_mfcio.h>

#define MAX_DMA_LISTSZ (MAX_ARGS+128U) // 16 entries of 16KB each equals to 256KB.
#define MAX_FETCHING 8U  // One active fetch and the rest pre-fetch.

enum task_state {
  UNFETCHED=0,
  FETCHING=1,
  FETCHED=2,
  EXECUTED=3,
  WRITINGBACK=4,
  WRITENBACK=5
};


struct tpc_spe_task_state {
  void *ls_addr;
  enum task_state state;
  int dmatag;
  mfc_list_element_t *dmalist;
  int isPrepared;
  unsigned int mfclist_offset;
  unsigned int outListElements;
  unsigned int totalListElements;
  unsigned int totalBytes;
} __attribute__ ((aligned (16)));

typedef struct tpc_spe_task_state  tpc_spe_task_state_t;

int execute_task(queue_entry_t *ex_task, tpc_spe_task_state_t *task_info);


#endif

