#ifndef _TPC_PPE_H_
#define _TPC_PPE_H_

#include <stdint.h>

void tpc_init(int max_spes);


/* Issues a TPC CALL on any available SPE. If none SPE is available, the
 * function blocks until one SPE becomes available.
 *
 * The first argument is the function id of the user function that is going to
 * be called in SPE.
 * 
 * The second argument is the count of arguments that the corresponding
 * function takes.
 *
 * For each user function argument, a triplet of arguments is expected in the
 * variable list of arguments of the tpc_call() function. Each triplet consists
 * of the following parts:
 * void *arg_address
 * int arg_size
 * int flag
 * 
 * Examples:
 * A tpc_call() to user function with id=0 which accepts one argument
 * would be like this:
 * tpc_call(0, 1, user_buffer, 1024, 0);
 *
 * A tpc_call() to user function with id=1 which accepts two arguments
 * would be like this:
 * tpc_call(1, 2, buf0, 1024, 0, buf1, 1024, 0);
 */
int tpc_call(uint8_t funcid, uint8_t total_arguments, ... );

void tpc_wait(int task_id);



// Returns 1 if there is at least one free slot for task issue.
// A subsequent tpc_call() will be non-blocking
//
// Returns 0 if there are no free slot for task issue.
// A subsequent tpc_call() will be blocking.
int tpc_issue_querry(void);



// Returns 1 if task_id is stiil active.
// Returns 0 if task_id has completed.
int tpc_querry(int task_id);

void tpc_wait_all(void);


void tpc_shutdown(void);


void *tpc_malloc(unsigned int size);
void tpc_free(void *mem, unsigned int size);


void tpc_print_stats(FILE *s);
void tpc_print_stats_normalized(FILE *s);
void tpc_reset_stats(void);


int tpc_call_descriptor(const uint8_t funcid, const uint8_t total_arguments, queue_entry_t *local_entry );

#define TASK_ENTRY queue_entry_t

#define SET_ARGUMENT( task, arg_i, arg_ea, arg_size, arg_type, arg_stride )  {  \
      task.arguments[ arg_i ].eal    = (uint32_t) arg_ea;                            \
      task.arguments[ arg_i ].size   = arg_size;                                        \
      task.arguments[ arg_i ].flag   = arg_type;                                        \
      task.arguments[ arg_i ].stride = arg_stride;                              \
}



#endif

