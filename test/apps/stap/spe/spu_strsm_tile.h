//==============================================================================================
//
//  Innovative Computing Laboratory - Computer Science Department - University of Tennessee
//  Written by Jakub Kurzak
//
//==============================================================================================

#ifndef _SPU_STRSM_TILE_H_
#define _SPU_STRSM_TILE_H_


#pragma css task input(T[64][64]) inout(B[64][64])
void spu_strsm_tile(float *T, float *B);


#endif

//----------------------------------------------------------------------------------------------
