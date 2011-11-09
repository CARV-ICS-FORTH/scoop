#ifndef __TPC_API_H__
#define __TPC_API_H__

#include <stdint.h>

// Bit-masks for supported argument flags and helping macros
#define TPC_IN_ARG                      0x1
#define TPC_OUT_ARG                     0x2
#define TPC_INOUT_ARG                   0x3
#define TPC_STRIDE_ARG                  0x4
#define TPC_SAFE_ARG                    0x8
#define TPC_START_ARG                   0x10
#define TPC_REDUCTION_ARG               0x20
#define TPC_HIGHPRIORITY_ARG            0x40

#define TPC_IS_INARG(x)                 ((x) & TPC_IN_ARG)
#define TPC_IS_OUTARG(x)                ((x) & TPC_OUT_ARG)
#define TPC_IS_INOUTARG(x)              ((x) & TPC_INOUT_ARG)
#define TPC_IS_HIGHPRIORITYARG(x)       ((x) & TPC_HIGHPRIORITY_ARG)
#define TPC_IS_REDUCTIONARG(x)          ((x) & TPC_REDUCTION_ARG)
#define TPC_IS_STRIDEARG(x)             ((x) & TPC_STRIDE_ARG)
#define TPC_IS_SAFEARG(x)               ((x) & TPC_SAFE_ARG)
#define TPC_IS_STARTARG(x)              ((x) & TPC_START_ARG)

// TPC Arguments
#pragma pack(4)
struct _tpc_task_argument {
  void * addr_in;
  void * addr_out;
  uint32_t type;
  uint32_t size;
  uint32_t stride;
  uint32_t element_num;
}; // 32-bytes on 64-bit arch
typedef struct _tpc_task_argument tpc_task_argument;

// TPC Task Descriptors
#pragma pack(4)
struct _tpc_task_descriptor {
  void (*task)(tpc_task_argument *);    // Wrapper to the original function (no value returned)
  tpc_task_argument * args;
  uint32_t args_no;
  uint32_t rfu;                         // RFU: Reserved for Future Use
  void * extras;
}; // 32-bytes on 64-bit arch
typedef struct _tpc_task_descriptor tpc_task_descriptor;

// Alloc/Free Task Descriptors
tpc_task_descriptor * tpc_task_descriptor_alloc(int); // number of arguments
void tpc_task_descriptor_free(tpc_task_descriptor *);
// + Compiler generated list of all possible task argument numbers
// const int tpc_task_arguments_list[]
// const int tpc_task_arguments_list[] = {2, 3, 5, 9};
// it will help allocate the appropriate pools during initialization
// and avoid malloc/free during run-time

// The typical TPC API
void tpc_init();
void tpc_shutdown();
void tpc_call(tpc_task_descriptor *);
void tpc_wait_all();
void tpc_wait_on(tpc_task_descriptor *);
// #pragma css wait on(x,y,z)
// equivalent to input

void wrapper_SCOOP__(tpc_task_argument *){
	
}

#endif
