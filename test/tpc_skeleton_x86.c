/* This skeleton is for the x86 GOD runtime */
int tpc_call_tpcAD65(/*uint8_t funcid, uint8_t total_arguments, void* addr,  int size, int flag, int stride*/ )
{
  Task_element *this;
 
  short unsigned int i;

#ifdef SCHEDULE_AGGRESIVE //Enabled by default
  TIMER_START(2);
  if(Task_Alloc_table_top==0)
    schedule_to_threshold();
  else
    schedule_one();
  G_ppe_stats.stalled_ticks+=TIMER_END(2);
#endif
  TIMER_START(1);

  this = AddTask();

//   this->closure.funcid = (uint8_t)funcid;
  this->closure.total_arguments = 0;
//   for(i=0; i<total_arguments; i++) {
//     void *arg_addr64;
//     unsigned int arg_size;
//     int arg_flag;
//  
//     arg_addr64 = va_arg(arg_list, void *);
//     arg_size = va_arg(arg_list, int);
//     arg_flag = va_arg(arg_list, int);
// 
//     this->closure.arguments[  this->closure.total_arguments ].stride=TPC_IS_STRIDEARG(arg_flag)? va_arg(arg_list, int):0;
//   
//     if(TPC_IS_SAFEARG(arg_flag)){ //Bypassing dependence Analysis
// 
//       this->closure.arguments[  this->closure.total_arguments ].size = arg_size;
//       this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag|TPC_START_ARG;
// 
//       this->closure.arguments[  this->closure.total_arguments ].eal_in   = (uint32_t) arg_addr64;
//       this->closure.arguments[  this->closure.total_arguments ].eal_out = (uint32_t) arg_addr64;
//       this->closure.total_arguments++;
//       continue;
//     }
// 
//     //overlapping
//     uint32_t block_index_start=this->closure.total_arguments;
//     if(TPC_IS_STRIDEARG(arg_flag)){
//       uint32_t j;
//       uint32_t stride=this->closure.arguments[  this->closure.total_arguments ].stride ;
//       uint32_t e_addr=(uint32_t)arg_addr64;
//       uint32_t numElems=TPC_EXTRACT_STRIDEARG_ELEMS(arg_size);
// #ifdef UNALIGNED_ARGUMENTS_ALLOWED
//       this->closure.arguments[ this->closure.total_arguments ].stride = 0;
// #endif
//       for(j=0;j<numElems;j++,e_addr+=stride){
//       this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag&~TPC_STRIDE_ARG;
//       this->closure.arguments[  this->closure.total_arguments ].size = TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size);
//         AddAttribute_Task(this, (void *)(e_addr), arg_flag&~TPC_STRIDE_ARG,TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size));
//   this->closure.total_arguments++;
//       }
//     }else{
//       const uint32_t limit=(((uint32_t)arg_addr64)+arg_size);
//       uint32_t e_addr;
// #ifdef UNALIGNED_ARGUMENTS_ALLOWED
//       uint32_t tmp_addr=(uint32_t)arg_addr64;
//       arg_addr64=((uint32_t)(tmp_addr/BLOCK_SZ))*BLOCK_SZ;
//       this->closure.arguments[arg_index].stride = tmp_addr-(uint32_t)arg_addr64;
// #endif
//       for(e_addr=(uint32_t)arg_addr64;e_addr + BLOCK_SZ <= limit ;e_addr+=BLOCK_SZ){
//         this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag;
//         this->closure.arguments[  this->closure.total_arguments ].size = BLOCK_SZ;
//         AddAttribute_Task( this, (void *)(e_addr), arg_flag,BLOCK_SZ);
//         this -> closure.total_arguments++;
//       }
//       if(limit-e_addr){
//         this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag;
//         this->closure.arguments[  this->closure.total_arguments ].size = limit-e_addr;
//         AddAttribute_Task( this, (void *)(e_addr), arg_flag,this->closure.arguments[  this->closure.total_arguments ].size);
//         this -> closure.total_arguments++;
//       }
//     }
//    
//     this->closure.arguments[ block_index_start ].flag|=TPC_START_ARG;
//     
//     assert( (((unsigned)arg_addr64&0xF) == 0) && ((arg_size&0xF) == 0));
// 
//   }
  Foo_32412312231();
  
  assert(this->closure.total_arguments<MAX_ARGS);

  Clear_Handler(this ) ;


  G_ppe_stats.issue_ticks +=TIMER_END(1);
  va_end( arg_list );
  /////////////////////////////////////////////////////
  //Unless specifically enabled this is not included //
#ifdef SCHEDULE_NORMAL
  TIMER_START(2);
  if(Task_Alloc_table_top==1)
    schedule_to_threshold();
  else
    schedule_one();
  G_ppe_stats.scheduler+=TIMER_END(2);  
#endif  
  /////////////////////////////////////////////////////
}
