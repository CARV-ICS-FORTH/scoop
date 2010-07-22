#ifndef _PBPI_PBPI_H
#define _PBPI_PBPI_H

//include common C libraries
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MPI
#define	DEBUG
//#undef  DEBUG

#define		PROGRAM				"PBPI -- Parallel Bayesian Phylogenetic Inference"
#define		VERSION				"Version 2.0 (2006)"
#define		AUTHOR				"Xizhou Feng"

#include "mpi.h"

#define	MAX_FILE_NAME_LEN	63
#define	PBPI_SUCCESS				0
#define	PBPI_ERROR					-1
#define	min2(a,b)	((a) < (b) ? (a):(b))
#define	max2(a,b)	((a) > (b) ? (a):(b))

typedef enum {FALSE, TRUE} boolean;

typedef	struct TrueTreeComparsionResult
{
	boolean		fEqualMPP,
				fEqualCTS,
				fEqualCON;

	int			nDistanceToMPP,
				nDistanceToCTS,
				nDistanceToCON;
}TrueTreeComparsionResult;

#endif	//_PBPI_PBPI_H

