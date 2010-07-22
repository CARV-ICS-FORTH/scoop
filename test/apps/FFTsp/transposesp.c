#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "tpc_common.h"
#include "tpc_ppe.h"
#include "transposesp.h"




void tpc_traspose(float *matrix, int matrix_dim, int block_dim)
{
  int i, j, block_size, total_blocks;
  float *row;
  static struct transargs *args=NULL;

  if(args == NULL) {
    args = (struct transargs *)tpc_malloc(sizeof(struct transargs));
    assert(args!=NULL );
  }

  args->block_dim = block_dim;
  block_size=block_dim*block_dim*2;
  total_blocks = (matrix_dim/block_dim);

  // First traspose the diagonal blocks
  row = matrix;
  for(i=0; i<matrix_dim/block_dim; ++i){
    tpc_call(2, 2,
	&row[block_dim*2*i],
	  TPC_BUILD_STRIDEARG(block_dim,2*block_dim*sizeof(float)),
	  TPC_INOUT_ARG|TPC_STRIDE_ARG, 2*matrix_dim*sizeof(float),
	args, sizeof(struct transargs), TPC_IN_ARG );
    row += 2*matrix_dim*block_dim;
  }
  //tpc_wait_all();

  //printf("Diagonal done\n");

  // Now traspose tha upper/low blocks and swap them.

 /* row = matrix;
  i=1;
  tpc_call(1, 3,
	&row[block_dim*2*i],
	  TPC_BUILD_STRIDEARG(block_dim,2*block_dim*sizeof(float)),
	  TPC_INOUT_ARG|TPC_STRIDE_ARG, 2*matrix_dim*sizeof(float),
	
	&row[32*2*i],
	  TPC_BUILD_STRIDEARG(block_dim,2*block_dim*sizeof(float)),
	  TPC_INOUT_ARG|TPC_STRIDE_ARG, 2*matrix_dim*sizeof(float),
	
	args, sizeof(struct transargs), TPC_IN_ARG );
  
  tpc_wait_all();*/

  row = matrix;
  for(i=0; i<total_blocks; ++i) {
    for(j=i+1; j<total_blocks; ++j) {
      int bid1, bid2;
      bid1 = j+(i*total_blocks);
      bid2 = i+(j*total_blocks);
      //printf("%d, %d, %d\n", i, bid1, bid2);
      //printf("%d, %d", i, (bid1/total_blocks)*block_size*total_blocks + (bid1%total_blocks)*block_dim*2);
      //printf(", %d\n", (bid2/total_blocks)*block_size*total_blocks + (bid2%total_blocks)*block_dim*2);
      tpc_call(3, 3,
	  &matrix[(bid1/total_blocks)*block_size*total_blocks + (bid1%total_blocks)*block_dim*2],
	    TPC_BUILD_STRIDEARG(block_dim,2*block_dim*sizeof(float)),
	    TPC_INOUT_ARG|TPC_STRIDE_ARG, 2*matrix_dim*sizeof(float),
	
	  &matrix[(bid2/total_blocks)*block_size*total_blocks + (bid2%total_blocks)*block_dim*2],
	    TPC_BUILD_STRIDEARG(block_dim,2*block_dim*sizeof(float)),
	    TPC_INOUT_ARG|TPC_STRIDE_ARG, 2*matrix_dim*sizeof(float),

	  args, sizeof(struct transargs), TPC_IN_ARG );
    }
  }

  tpc_wait_all();
  //printf("Upper/Low done\n");


  //free(args);
}
