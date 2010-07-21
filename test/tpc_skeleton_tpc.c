/* This skeleton is for the original tpc runtime */
int tpc_call_tpcAD65(/*uint8_t funcid, uint8_t total_arguments, void* addr,  int size, int flag, int stride*/ )
{
//   int total_bytes, arg_bytes;
  volatile queue_entry_t *avail_task=NULL;
  
#ifdef STATISTICS
  uint64_t tmptime1, tmptime2, tmptime3;
  int arg_bytes;
  unsigned int total_bytes=0;
#endif

  int *task_id_qs, task_id;
  
  // those can be removed when we have no arguments
//   void *arg_addr64=NULL;
//   int arg_size=0;
//   int arg_flag=0;
//   int arg_stride=0;
//   volatile vector unsigned char *tmpvec;
//   struct tpc_arg_element local_arg;
  
  volatile struct completions_status_t *st;

#ifdef TPC_MULTITHREADED
  pthread_mutex_lock( &tpc_callwait_mutex );
#endif

  READ_TIME_REG(tmptime1);

  do {
    st = &compl_queue[s_available_spe][task_queue_tail[s_available_spe]];
    if(st->status == COMPLETED) {
      st->status = WAITING;
      break;
    } else {
      s_available_spe = (s_available_spe+1) % G_max_spes;
    }
  } while(1);

  task_id = g_task_current_id[s_available_spe]++;
  task_id_qs = &g_task_id_queue[s_available_spe][task_queue_tail[s_available_spe]];
  task_id = (task_id & 0x0FFFFFFF) | (s_available_spe << 28);
  *task_id_qs = task_id;
  
  READ_TIME_REG(tmptime2);

  avail_task = &task_queue[s_available_spe][task_queue_tail[s_available_spe]];

//   avail_task->funcid = (uint8_t)funcid;
//   avail_task->total_arguments = (uint8_t)total_arguments;

//   for(i=0; i<total_arguments; ++i) {
//     arg_addr64 = addr;
//     arg_size = size;
//     arg_flag = flag;
//     
// #ifdef STATISTICS
//     if( TPC_IS_STRIDEARG(arg_flag) ) {
//       arg_bytes = TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size)*TPC_EXTRACT_STRIDEARG_ELEMS(arg_size);
//     } else {
//       arg_bytes = arg_size;
//     }
//     total_bytes += ( arg_bytes<< TPC_IS_INOUTARG(arg_flag));
// #endif
// 
// //     arg_stride = stride;
// 
// #ifndef NDEBUG
//     assert( (((unsigned)arg_addr64&0xF) == 0) && ((arg_size&0xF) == 0)  &&
//           ((arg_stride&0xF) == 0));
// #endif
// 
//     tmpvec = (volatile vector unsigned char *)&avail_task->arguments[i];
//     local_arg.eal = (uint32_t)(arg_addr64);
//     local_arg.size = arg_size;
//     local_arg.flag = arg_flag;
//     local_arg.stride = arg_stride;
//     *tmpvec = *((volatile vector unsigned char *)&local_arg);
//   }
  Foo_32412312231();

  avail_task->active = ACTIVE;
  // Increase local tail
  task_queue_tail[s_available_spe] = (task_queue_tail[s_available_spe]+1) % MAX_QUEUE_ENTRIES;
  // Choose next SPE in round-robin.
  s_available_spe = (s_available_spe+1) % G_max_spes;

  READ_TIME_REG(tmptime3);

#ifdef STATISTICS
  G_ppe_stats.stat_tpc_per_spe[s_available_spe] += 1;
  G_ppe_stats.bytes_per_spe[s_available_spe] += total_bytes;

  G_ppe_stats.stalled_ticks += (tmptime2 - tmptime1);
  G_ppe_stats.issue_ticks += (tmptime3 - tmptime2);
#endif

#ifdef TPC_MULTITHREADED
  pthread_mutex_unlock( &tpc_callwait_mutex );
#endif
  return task_id;
}
