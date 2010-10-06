#include <stdio.h>
#include <ppu_intrinsics.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>

#define vector __attribute__((altivec(vector__)))

#include "include/tpc_common.h"
#include "include/tpc_ppe.h"

#define TIMEBASE 79800.0
#define CORE_CYCLES 40


#define DEFAULT_SPESNUM     1
#define DEFAULT_ARGSIZE     16
#define DEFAULT_ARGFLAG     TPC_IN_ARG
#define DEFAULT_COMPCLASS   0
#define DEFAULT_TOTALTPCS   100000


int computation_func0(void)
{
/*  void *buf1 = malloc(1);
  void *buf2 = malloc(1);
  void *buf3 = malloc(1);
  void *buf4 = malloc(1);
  printf("No computation %p,%p,%p,%p\n", buf1,buf2,buf3,buf4);
  free(buf1);
  free(buf2);
  free(buf3);
  free(buf4);*/
  return 0;
}

__inline__ int computation_func(int *class)
{
  volatile int *buf = class;
  int i;
  int tmp = *class;
//  unsigned int time;

//  time = spu_read_decrementer();

  for(i=0; i<tmp; ++i) {
    buf[0] = buf[1]+1;
    buf[1] = buf[2]+1;
    buf[2] = buf[3]+1;
    buf[3] += *class;
  }
  
//  time = time - spu_read_decrementer();
  
//  printf("Some computation for %u TBR\n", time);

  return 0;
}

int main(int argc, char **argv)
{
  int c, tmp;
  extern char *optarg;

  int spes_num = DEFAULT_SPESNUM;
  int arg_size = DEFAULT_ARGSIZE;
  int args_num = 1;
  int arg_flag = DEFAULT_ARGFLAG;
  int comp_class = DEFAULT_COMPCLASS;
  int total_tpcs = DEFAULT_TOTALTPCS;
  int funcid=0;


  void *tpc_buf[MAX_SPES];
  int i;

  unsigned long long starttime=0, issuetime=0, finishtime=0;

  memset(tpc_buf, 0, MAX_SPES*sizeof(void*));

  while ((c = getopt(argc, argv, "c:p:s:t:oh")) != -1) {
    switch(c) {
      case 'c': tmp = atoi(optarg); 
		switch(tmp) {
		  case 0: 
		    comp_class=0;    // 0 (almost) core cycles.
		    break;
		  case 1000:
		    comp_class=13;   // 1000 SPE core cycles.
		    break;
		  case 10000:
		    comp_class=144;  // 10000 SPE core cycles.
		    break;
		  default:
		    fprintf(stderr, "Unsupported computation class %d\n", tmp);
		    exit(-1);
		    break;
		}
		break;

      case 'p': spes_num = atoi(optarg); 
                if (spes_num<1 || spes_num>6) {
                  fprintf(stderr, "Wrong number of SPEs %d\n", spes_num);
                  exit(-1);
                }
	        break;  

      case 's': tmp = atoi(optarg);
		arg_size = tmp;
		if(arg_size < 0) {
		  fprintf(stderr, "Argument size must be positive, %d is negative\n", arg_size);
		  exit(-1);
		}
                if (tmp%16 != 0) {
		  arg_size = ceil16(tmp);
                  fprintf(stderr, "Warning: Argument size %d is not multiple of 16, ceiling to %d\n", tmp, arg_size);
                }
		if(arg_size == 0) {
		  args_num = 0;
		}
	        break;

      case 't': total_tpcs = atoi(optarg);
		if(total_tpcs < 1) {
		  fprintf(stderr, "Total TPC calls must be at least one, not %d\n", total_tpcs);
		  exit(-1);
		}
		break;

      case 'o': arg_flag = TPC_INOUT_ARG;
	        break;

      case 'h': printf("Usage: %s <options>\n\n", argv[0]);
                printf("options:\n");
                printf("  -cC : C = computation (0, 1000 and 10000 SPE cycles\n");
                printf("  -pP : P = number of SPEs\n");
                printf("  -sS : S = size of argument in bytes\n");
                printf("  -tT : T = total TPC calls to be measured\n");
                printf("  -o  : Set argument flag to INOUT (default is IN)\n");
                printf("  -h  : Print out command line options.\n\n");
                printf("Default: %s -c%1d -p%1d -s%1d -t%1d\n", argv[0], 
                       DEFAULT_COMPCLASS, DEFAULT_SPESNUM, DEFAULT_ARGSIZE,
		       DEFAULT_TOTALTPCS);
		exit(0);
	        break;
    }
  }

  
  if(arg_size != 0) {
    int tmpsize = arg_size;
    tmpsize = ceil4096(tmpsize);
    for(i=0; i<spes_num; ++i) {
      tpc_buf[i] = tpc_malloc( tmpsize );
      assert(tpc_buf[i]);
      *((int*)tpc_buf[i]) = comp_class;
    }
  } else {
    funcid = 1;
  }


  printf("TPC benchmark setup\n");
  printf("%5d : SPEs\n", spes_num);
  printf("%5d : Computation class\n", comp_class);
  printf("%5d : Argument size (%d arguments)\n", arg_size, args_num);
  printf("%5d : Argument flag\n", arg_flag);
  printf("%5d : Total TPC calls\n", total_tpcs);
  printf("%5d : TPC function id (%s)\n", funcid, funcid==0 ? "Some computation":"No computation");
  printf("\n%p : Buffer address\n", tpc_buf);


  tpc_init(spes_num);

  // Warm up
  for(i=0; i<spes_num; ++i) {
    #pragma css task
    computation_func0();
//     tpc_call(1, args_num, tpc_buf[i%spes_num],arg_size,TPC_INOUT_ARG);
  }

  tpc_wait_all();

  tpc_reset_stats();

  printf("Starting benchmark\n");

  // Start benchmark
  starttime = __mftb();

  for(i=0; i<total_tpcs; ++i) {
    #pragma css task
    computation_func0();
//     tpc_call(funcid, args_num, tpc_buf[i%spes_num],arg_size,arg_flag);
  }

  issuetime = __mftb();

  tpc_wait_all();
  
  finishtime = __mftb();

  printf("Benchmark completed.\n");

  printf("\n");
  printf("Timings\n");
  printf("-------\n");
  printf("starttime           : 0x%.16llx \n", starttime);
  printf("issuetime           : 0x%.16llx \n", issuetime);
  printf("finishtime          : 0x%.16llx \n", finishtime);
  printf("\n");
  printf("Total time   (ms)   : %lf \n", (double)(finishtime-starttime)/TIMEBASE);
  printf("Issue time   (ms)   : %lf \n", (double)(issuetime-starttime)/TIMEBASE);
  printf("Wait time    (ms)   : %lf \n", (double)(finishtime-issuetime)/TIMEBASE);
  printf("Average time (ms)   : %lf \n", ((double)(finishtime-starttime)/total_tpcs)/TIMEBASE);
  /*printf("Total GFLOPS        : %lf \n", ((double)arg_size/
      ((double)(finishtime-starttime)/(TIMEBASE*1000))/1000000000));*/

  printf("\n");
  printf("Total time   (core) : %llu \n", (finishtime-starttime)*CORE_CYCLES);
  printf("Issue time   (core) : %llu \n", (issuetime-starttime)*CORE_CYCLES);
  printf("Wait time    (core) : %llu \n", (finishtime-issuetime)*CORE_CYCLES);
  printf("Average time (core) : %llu \n", ((finishtime-starttime)/total_tpcs)*CORE_CYCLES);
  printf("\n");
  printf("Total time   (TBR)  : %llu \n", (finishtime-starttime) );
  printf("Issue time   (TBR)  : %llu \n", (issuetime-starttime) );
  printf("Wait time    (TBR)  : %llu \n", (finishtime-issuetime) );
  printf("Average time (TBR)  : %llu \n", ( (finishtime-starttime)/total_tpcs) );

  unsigned long long totaltime = finishtime-starttime;
  double tpcs_sec = (double)total_tpcs/((double)totaltime/79800000.0);
  double gb_sec = 0.0;
  if(arg_flag != TPC_INOUT_ARG) {
    gb_sec = (tpcs_sec*arg_size)/1000000000.0;
  } else {
    gb_sec = (tpcs_sec*2*arg_size)/1000000000.0;
  }
  printf("\n");
  printf("Throughput1 TPCs/sec : %.1lf (K) \n", tpcs_sec/1000.0);
  printf("Throughput2 GB/sec   : %.1lf \n", gb_sec);
  
  tpc_shutdown();
  tpc_print_stats(stdout);
  exit(0);
  return 0;
}

