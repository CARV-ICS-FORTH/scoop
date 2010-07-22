#ifndef __FFT_H__
#define __FFT_H__

#include "transposesp.h"

struct FFTsteps23args {
	int j, m1, n1, N, pad_length, direction;
	int rows;
} __attribute__ ((aligned(128)));



#endif
