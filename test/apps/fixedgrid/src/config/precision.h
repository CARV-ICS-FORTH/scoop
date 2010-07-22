/*
 *  precision.h
 *  cell_double
 *
 *  Created by John Linford on 4/29/08.
 *  Copyright 2008 Transatlantic Giraffe. All rights reserved.
 *
 */

#ifndef __PRECISION_H__
#define __PRECISION_H__

#include "params.h"

#if DOUBLE_PRECISION == 1

    #define real_t double
    #define VECTOR_LENGTH 2
    #define ROW_LENGTH (NCOLS + (NCOLS % 2))
    #define COL_LENGTH (NROWS + (NROWS % 2))

    // Must never be larger than 2048
    #define MAX_DMA 512

    #define VEC_DIVIDE(x, y) divd2(x, y)

    #define PRINT_VECTOR(name, vec) \
        printf("%s: {%f %f}\n", name, spu_extract(vec, 0), spu_extract(vec, 1))

    #define SPLAT_CONST(x) {x, x}

#else

    #define real_t float
    #define VECTOR_LENGTH 4
    #define ROW_LENGTH (NCOLS + 4 - (NCOLS % 4))
    #define COL_LENGTH (NROWS + 4 - (NROWS % 4))

    // Must never be larger than 4096
    #define MAX_DMA 1024

    #define VEC_DIVIDE(x, y) divf4(x, y)

    #define PRINT_VECTOR(name, vec) \
        printf("%s: {%f %f %f %f}\n", name, spu_extract(vec, 0), spu_extract(vec, 1), spu_extract(vec, 2), spu_extract(vec, 3))

    #define SPLAT_CONST(x) {x, x, x, x}
#endif

#endif
