#ifndef __TPC_API_H__
#define __TPC_API_H__

#include <stdint.h>
#include <stdio.h>

// Bit-masks for supported argument flags and helping macros
#define SCOOP_IN_ARG                      0x1
#define SCOOP_OUT_ARG                     0x2
#define SCOOP_INOUT_ARG                   0x3
#define SCOOP_STRIDE_ARG                  0x4
#define SCOOP_SAFE_ARG                    0x8
#define SCOOP_BYVALUE_ARG                 0x9
#define SCOOP_REGION_ARG                  0x16
#define SCOOP_REDUCTION_ARG               0x35
#define SCOOP_HIGHPRIORITY_ARG            0x64

#define SCOOP_IS_INARG(x)                 ((x) & SCOOP_IN_ARG)
#define SCOOP_IS_OUTARG(x)                ((x) & SCOOP_OUT_ARG)
#define SCOOP_IS_INOUTARG(x)              ((x) & SCOOP_INOUT_ARG)
#define SCOOP_IS_HIGHPRIORITYARG(x)       ((x) & SCOOP_HIGHPRIORITY_ARG)
#define SCOOP_IS_REDUCTIONARG(x)          ((x) & SCOOP_REDUCTION_ARG)
#define SCOOP_IS_STRIDEARG(x)             ((x) & SCOOP_STRIDE_ARG)
#define SCOOP_IS_SAFEARG(x)               ((x) & SCOOP_SAFE_ARG)
#define SCOOP_IS_BYVALUEARG(x)            ((x) & SCOOP_BYVALUE_ARG)
#define SCOOP_IS_REGIONARG(x)             ((x) & SCOOP_REGION_ARG)

extern int iam;
extern void *shm_start;

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
  uint32_t args_num;
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
void tpc_init(int, char**);
void tpc_shutdown();
void tpc_call(tpc_task_descriptor *);
void tpc_wait_all();
void tpc_wait_on(tpc_task_descriptor *);
// #pragma css wait on(x,y,z)
// equivalent to input

void* tpc_malloc(size_t);
void tpc_free(void *);

void wrapper_SCOOP__(tpc_task_argument *a ){
	
}

#endif
