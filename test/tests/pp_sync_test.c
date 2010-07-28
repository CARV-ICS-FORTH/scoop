#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//#include <altivec.h>
#include <ppu_intrinsics.h>
#include <assert.h>
#include <malloc.h>

//#include <tpc_common.h>
//#include <tpc_ppe.h>

#include "pp_sync_test.h"

void plus1seeds(int *x)
{
  int i;

  for(i=0; i<SEEDS_SIZE; ++i) {
    x[i] += 1;
  }

  printf("aaa");

  return;
}


void set_array(int *array, int *seeds)
{
  int i;

  for(i=0; i<BLOCK_SIZE; ++i) {
    array[i] = seeds[0]+1;
  }

  return;
}


int g_seeds[TOTAL_SEEDS][SEEDS_SIZE] __attribute__((aligned(128)));

int g_array[ARRAY_SIZE] __attribute__((aligned(128)));

int task_ids[TOTAL_SEEDS] __attribute__((aligned(128)));

int main()
{
  int i, j, *arg1, *arg2, arg1_size, arg2_size;
  int errors;
  int *x;
  unsigned long long t1, t2;
  float msec;

  assert(TOTAL_SEEDS == ARRAY_SIZE/BLOCK_SIZE);

  tpc_init(6);

  for(i=0; i<TOTAL_SEEDS; ++i) {
    for(j=0; j<SEEDS_SIZE; ++j) {
      g_seeds[i][j] = i;
    }
  }

  t1 = __mftb();

  for(i=0; i<TOTAL_SEEDS; ++i) {
    arg1 = g_seeds[i];
    arg1_size = SEEDS_SIZE*sizeof(int);
    #pragma tpc(arg1(inout, arg1_size))
    task_ids[i] = plus1seeds(arg1);
      /*tpc_call(0, 1,
	  g_seeds[i], SEEDS_SIZE*sizeof(int), TPC_INOUT_ARG
      )*/;
  }


  tpc_wait_all();
  
  for(i=0, j=0; i<ARRAY_SIZE; i+=BLOCK_SIZE, ++j) {
    arg1 = g_array+i;
    arg2 = g_seeds[j];
    arg1_size = BLOCK_SIZE*sizeof(int);
    arg2_size = SEEDS_SIZE*sizeof(int);
    #pragma tpc(arg1(inout, arg1_size), arg2(in, arg2_size))
    set_array(arg1, arg2);
    //tpc_wait(task_ids[j]);
//     tpc_call(1, 2,
// 	g_array+i , BLOCK_SIZE*sizeof(int), TPC_INOUT_ARG, 
// 	g_seeds[j], SEEDS_SIZE*sizeof(int),TPC_IN_ARG
//     );
  }

  tpc_wait_all();

  t2 = __mftb();

  // Verify
  printf("Verifying\n");
  errors=0;
  for(i=0; i<TOTAL_SEEDS; ++i) {
    x = g_array+(i*BLOCK_SIZE);
    for(j=0; j<BLOCK_SIZE; ++j) {
      if(x[j] != g_seeds[i][0]+1) {
	//printf("%d != %d\n", x[j], g_seeds[i][0]);
	++errors;
      }
    }
  }

  msec = (t2-t1)/79800.0f;
  printf("Errors: %d\n", errors);
  printf("TBR ticks: %llu\n", t2-t1);
  printf("millisecs: %.3f\n", msec);

  /*printf("\n");
  for(i=0; i<TOTAL_SEEDS; ++i){
    printf("task_id[%d] = %x\n", i, task_ids[i]);
  }*/
  //tpc_print_stats(stdout);
  tpc_shutdown();
  return 0;
}

