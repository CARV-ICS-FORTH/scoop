/** This software is part of the dissertation "High Performance, Bayesian-based Phylogenetic Inference Framework" 
 *  by Xizhou Feng and is still under development. 
 *  Copyright: 
 *	Xizhou Feng
 *      Kirk Cameron
 *      Virginia Tech IP
 *  Contact:
 *	Xizhou Feng
 *	Department of Computer Science
 *	Virgnia Tech, Blacksburg, VA 24060 
 *	Email: fengx@cs.vt.edu
 *	Phone: (540)231-9245
 */

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <time.h>
#include "mpi.h"
#include "parallel.h"
#include "runsetting.h"
#include "pbpi.h"
#include "error.h"
#include "rand.h"
#include "rand2.h"


char		g_ProcName[ MPI_MAX_PROCESSOR_NAME + 1 ];
extern		int					g_Commsize,	g_Myrank;
extern		ChainProcessMap		g_map;

boolean	initMPIEnviornment(int *argc, char ***argv)
{
	boolean		fInitMPIOK = FALSE;
	int	numProcessLength;

	//initialize the MPI environemnt
	/*debug("init:0");*/
	if ( MPI_SUCCESS == MPI_Init( argc, argv ) )
	{
		/*debug("init:1");*/
		if( MPI_SUCCESS == MPI_Comm_size( MPI_COMM_WORLD, &g_Commsize) )
		{
			/*debug("init:2");*/
			if( MPI_SUCCESS == MPI_Comm_rank( MPI_COMM_WORLD, &g_Myrank ) )
			{
				/*debug("init:3");*/
				if ( MPI_SUCCESS == MPI_Get_processor_name(g_ProcName, &numProcessLength) )
				{
					/*debug("init:4");*/
					g_ProcName[numProcessLength] = '\0';
					fInitMPIOK	= TRUE;
				}
				else
				{
					error("call MPI_Get_processor_name failed");
				}
			}
			else
			{
				error("call MPI_Comm_rank failed");
			}
		}
		else
		{
			error("call MPI_Comm_Size failed");
		}
	}
	else
	{
		error("call MPI_Init failed");
	}

	/*debug("initMPIEnviornment:%d", fInitMPIOK);*/
	printf("use node[%d]: %s\n", g_Myrank, g_ProcName);
	fflush(stdout);
	return fInitMPIOK;
}

void exitProgram( int nReturnCode )
{
	MPI_Finalize();
	exit( nReturnCode );
}

void message_p( const char *fmt, ...)
{
	static	char msgbuf[1024];
	if (g_Myrank == HEAD_NODE)
	{
		va_list	ap;
		va_start(ap, fmt);
		vsprintf(msgbuf, fmt, ap);
		fprintf(stdout, "%s\n", msgbuf); 
		fflush(stdout);
		va_end(ap);
	}
}
void showProgramInformation( void )
{
	message_p("%s\n%s By %s\n\n", PROGRAM, VERSION, AUTHOR);
}

boolean	readRunsetting_p( char *szRunsettingFile, RunSetting *pRunSetting )
{
	boolean		fOk		=	FALSE;

	if (g_Myrank == HEAD_NODE)
	{
		fOk	= readRunSetting( szRunsettingFile, pRunSetting );
		//debug("readRunsetting_p: 1 (status=%d)", fOk);
		//debug("HOST: %s", g_ProcName);
		//dumpRunSetting( pRunSetting );
	}
	//MPI_Barrier( MPI_COMM_WORLD );
	MPI_Bcast(&fOk, sizeof(boolean), MPI_CHAR, HEAD_NODE, MPI_COMM_WORLD);
	//debug("readRunsetting_p: 2 (status=%d)", fOk);
	MPI_Barrier( MPI_COMM_WORLD );

	if( fOk )
	{
		MPI_Bcast(pRunSetting, sizeof(RunSetting), MPI_CHAR, HEAD_NODE, MPI_COMM_WORLD);
		//debug("HOST: %s", g_ProcName);
		//dumpRunSetting( pRunSetting );
	}
	//MPI_Barrier( MPI_COMM_WORLD );
	//debug("readRunsetting_p: 3 (status=%d)", fOk);

	return fOk;
}

boolean readDataset_p( RunSetting *pRunSetting, Dataset *pDataset)
{
	boolean		fDataOk		= FALSE;
	FILE		*fpDataset;

	initDataset( pDataset);

	if( pRunSetting->fEnableBatchProcessing )
	{
		if( g_map.rankDataset == HEAD_NODE )
		{
			//debug("readDataset_p: 0");
			char  datafilepath[512];
			sprintf(datafilepath, "%s/%s", pRunSetting->sDataSourceDir, pRunSetting->sDatasetFile);
			fpDataset = fopen(datafilepath, "r");
			//fpDataset = fopen(pRunSetting->sDatasetFile, "r");
			if( fpDataset )
			{
				fDataOk = readDataset( pDataset, fpDataset );
				//dumpDataset( pDataset );
			}
			else
			{
				fDataOk	=	FALSE;
				error("can not open dataset file (%s)", pRunSetting->sDatasetFile);
			}
		}
		MPI_Barrier( g_map.commDataset );
		MPI_Bcast(&fDataOk, sizeof(boolean), MPI_CHAR, HEAD_NODE, g_map.commDataset);
		//debug( "[%d] ReadDataSet %s", g_map.rankMPIWorld, fDataOk?"good":"failed");

		if( fDataOk )
		{
			MPI_Bcast( pDataset, sizeof(Dataset), MPI_CHAR, HEAD_NODE, g_map.commDataset);
			//printf( "[%d] nTaxa=%d nChars=%d", g_map.rankMPIWorld, pDataset->nTaxa, pDataset->nChar);
			fflush(stdout);
			if(g_Myrank != HEAD_NODE)
			{
				//debug("readDataset_p: 2(fDataOk=%d)", fDataOk);
				allocCMatrix(&(pDataset->data),  pDataset->nTaxa, pDataset->nChar);
				allocCMatrix(&(pDataset->label), pDataset->nTaxa, pDataset->nlen);
				allocCMatrix(&(pDataset->translate), pDataset->nTaxa, pDataset->nlen);

			}	
			MPI_Bcast(pDataset->data.__data, pDataset->nTaxa *  (pDataset->nChar + 1), MPI_CHAR, HEAD_NODE, g_map.commDataset);
			MPI_Bcast(pDataset->label.__data, pDataset->nTaxa * (pDataset->nlen + 1),  MPI_CHAR, HEAD_NODE, g_map.commDataset);
			MPI_Bcast(pDataset->translate.__data, pDataset->nTaxa * (pDataset->nlen + 1),  MPI_CHAR, HEAD_NODE, g_map.commDataset);
			//dumpDataset( pDataset );
		}
	}
	else
	{
		if (g_Myrank == HEAD_NODE)
		{
			//debug("readDataset_p: 0");
			fpDataset = fopen(pRunSetting->sDatasetFile, "r");
			if( fpDataset )
			{
				fDataOk = readDataset( pDataset, fpDataset );
				//dumpDataset( pDataset );
			}
			else
			{
				fDataOk	=	FALSE;
				error("can not open dataset file (%s)", pRunSetting->sDatasetFile);
			}
		}
		MPI_Barrier( MPI_COMM_WORLD );
		MPI_Bcast(&fDataOk, sizeof(boolean), MPI_CHAR, HEAD_NODE, MPI_COMM_WORLD);
		//debug( "[%d] ReadDataSet %s", g_map.rankMPIWorld, fDataOk?"good":"failed");
	
		if( fDataOk )
		{
			MPI_Bcast( pDataset, sizeof(Dataset), MPI_CHAR, HEAD_NODE, MPI_COMM_WORLD);
			//debug( "[%d] nTaxa=%d nChars=%d", g_map.rankMPIWorld, pDataset->nTaxa, pDataset->nChar);
			if(g_Myrank != HEAD_NODE)
			{
				//debug("readDataset_p: 2(fDataOk=%d)", fDataOk);
				allocCMatrix(&(pDataset->data),  pDataset->nTaxa, pDataset->nChar);
				allocCMatrix(&(pDataset->label), pDataset->nTaxa, pDataset->nlen);
				allocCMatrix(&(pDataset->translate), pDataset->nTaxa, pDataset->nlen);
				
			}	
			MPI_Bcast(pDataset->data.__data, pDataset->nTaxa *  (pDataset->nChar + 1), MPI_CHAR, HEAD_NODE, MPI_COMM_WORLD);
			MPI_Bcast(pDataset->label.__data, pDataset->nTaxa * (pDataset->nlen + 1),  MPI_CHAR, HEAD_NODE, MPI_COMM_WORLD);
			MPI_Bcast(pDataset->translate.__data, pDataset->nTaxa * (pDataset->nlen + 1),  MPI_CHAR, HEAD_NODE, MPI_COMM_WORLD);
			//dumpDataset( pDataset );
		}
		//debug("readDataset_p: 3(fDataOk=%d)", fDataOk);
	}
	return fDataOk;
}

boolean buildChainProcessMap( ChainProcessMap *pMap, RunSetting *pRunSetting)
{
	if ( g_Commsize == 1 )
	{
		//debug("Run in Sequential Mode");
		//pRunSetting->fEnableBatchProcessing		 = FALSE;
		pRunSetting->fEnableDatasetParallelism	 = FALSE;
		pRunSetting->fEnableBootstrapParallelism = FALSE;
		pRunSetting->fEnableRunParallelism		 = FALSE;
		pRunSetting->fEnableChainParallism		 = FALSE;
		pRunSetting->fEnableSequenceParalleism	 = FALSE;
	}

	if( !pRunSetting->fEnableChainParallism )
	{
		//debug("chain level paralleism disabled");
		pRunSetting->nChainsPerGroup  = pRunSetting->nChains;
	}

	if( !pRunSetting->fEnableSequenceParalleism )
	{
		//debug("sequence level paralleism disabled");
		pRunSetting->nProcessPerChain = 1;
	}

	//debug("Before Automatic Adjusting");
	//debug("%-10s:%5d",	"NPROCS",	g_Commsize);
	//debug("%-10s:%5d",	"PPC",		pRunSetting->nProcessPerChain);
	//debug("%-10s:%5d",	"CPG",		pRunSetting->nChainsPerGroup);

	//automatic adjust the grid topology	
	pMap->numCols	=	pRunSetting->nProcessPerChain;
	pMap->numRows	=	pRunSetting->nChains / pRunSetting->nChainsPerGroup;
	pMap->numProcessPerRun = pMap->numCols * pMap->numRows;
	while ( pMap->numProcessPerRun > g_Commsize )
	{
		if(pRunSetting->nProcessPerChain > 1)
			pRunSetting->nProcessPerChain--;
		else if(pRunSetting->nChainsPerGroup<pRunSetting->nChains)
			pRunSetting->nChainsPerGroup++;

		pMap->numCols	=	pRunSetting->nProcessPerChain;
		pMap->numRows	=	pRunSetting->nChains / pRunSetting->nChainsPerGroup;
		pMap->numProcessPerRun = pMap->numCols * pMap->numRows;
	}

	//debug("%-10s:%5d",	"NPROCS",	g_Commsize);
	//debug("%-10s:%5d",	"PPC",		pRunSetting->nProcessPerChain);
	//debug("%-10s:%5d",	"CPG",		pRunSetting->nChainsPerGroup);
	//debug("%-10s:%5d",	"NROW",		pMap->numRows);
	//debug("%-10s:%5d",	"NCOL",		pMap->numCols);

	//now compute the possible run groups
	if( !pRunSetting->fEnableRunParallelism  ||  pRunSetting->nRuns <= 1)
	{
		pMap->numRunGroup	= 1;
	}
	else
	{
		pMap->numRunGroup = min2( pRunSetting->nRuns, 
			(g_Commsize % pMap->numProcessPerRun)? 
				(g_Commsize / pMap->numProcessPerRun + 1)
			:	(g_Commsize / pMap->numProcessPerRun ) );
	}
	
	//debug("%-10s:%5d %5d %5d",	"RUNS",		pRunSetting->nRuns, pMap->numRunGroup, pMap->numProcessPerRun);
	
	//now compute the possible bootstrap
	pMap->numProcessPerBootstrap = pMap->numProcessPerRun * pMap->numRunGroup;
	if( !pRunSetting->fEnableBootstrapParallelism || pRunSetting->nBootstrap <= 1 )
	{
		pMap->numBootstrapGroup = 1;
	}
	else
	{
		pMap->numBootstrapGroup = min2( pRunSetting->nBootstrap, 
			(g_Commsize % pMap->numProcessPerBootstrap)? 
				(g_Commsize / pMap->numProcessPerBootstrap +1)
			: 	(g_Commsize / pMap->numProcessPerBootstrap) );
	}
	//debug("%-10s:%5d %5d %5d",	"BOOTSTRAP",	pRunSetting->nBootstrap, pMap->numBootstrapGroup, pMap->numProcessPerBootstrap);

	//now compute the possible dataset group
	pMap->numProcessPerDataset	=	pMap->numBootstrapGroup * pMap->numProcessPerBootstrap;
	if( !pRunSetting->fEnableDatasetParallelism || pRunSetting->nDataset <= 1 )
	{	
		pMap->numDatasetGroup	=	1;
	}
	else
	{
		pMap->numDatasetGroup = min2( pRunSetting->nDataset, 
			(g_Commsize % pMap->numProcessPerDataset) ?
				(g_Commsize / pMap->numProcessPerDataset + 1)
			:	(g_Commsize / pMap->numProcessPerDataset) );
	}
	//debug("%-10s:%5d %5d %5d",	"DATASET",	pRunSetting->nDataset, pMap->numDatasetGroup, pMap->numProcessPerDataset);

	//check paralleism match
	if( pMap->numProcessPerDataset * pMap->numDatasetGroup != g_Commsize )
	{
		message_p("WARNING: the number of processors does not match current runsetting");
		return FALSE;
	}

	//build the dataset communicator
	{
		MPI_Group		oldGroup,
						newGroup;
		int				myRank,
		/*				index,*/
						indexDatasetGroup,
						nGroupSize,
						nOldCommSize,
					*	paRanksInOldGroup;

		MPI_Comm_group( MPI_COMM_WORLD, &oldGroup );
		MPI_Comm_rank( MPI_COMM_WORLD, &myRank );
		MPI_Comm_size( MPI_COMM_WORLD, &nOldCommSize);

		pMap->rankMPIWorld	=	myRank;
		pMap->numTotalProcess = nOldCommSize;
		indexDatasetGroup = myRank / pMap->numProcessPerDataset;
		nGroupSize  =	min2( pMap->numProcessPerDataset,
			(nOldCommSize % pMap->numProcessPerDataset)?
					(nOldCommSize % pMap->numProcessPerDataset)
				:	pMap->numProcessPerDataset );

		//how many dataset will be processed by my datasetset group
		pMap->indexDatasetGroup = indexDatasetGroup;
		pMap->numLocalDataset = pRunSetting->nDataset / pMap->numDatasetGroup;
		if ( indexDatasetGroup < pRunSetting->nDataset % pMap->numDatasetGroup)
			pMap->numLocalDataset++;
		
		paRanksInOldGroup = (int *)malloc( sizeof(int) * nGroupSize);
		if( paRanksInOldGroup )
		{
			int	index;
			paRanksInOldGroup[0] = indexDatasetGroup * pMap->numProcessPerDataset;

			for( index=1; index<nGroupSize; index++)
				paRanksInOldGroup[index] = paRanksInOldGroup[index-1] + 1;

			MPI_Group_incl( oldGroup,  nGroupSize, paRanksInOldGroup, &newGroup);

			MPI_Comm_create( MPI_COMM_WORLD, newGroup, &pMap->commDataset);
			MPI_Comm_rank( pMap->commDataset, &pMap->rankDataset);

			MPI_Group_free( &newGroup );
			free( paRanksInOldGroup );

			//debug("data group %d	size = %d	rank = %d", indexDatasetGroup,	nGroupSize, pMap->rankDataset);
		}
		else
		{
			error("allocate memory failed");
			return FALSE;
		}
		//debug("[%d] %-10s: %5d %5d %5d %5d",
		//	pMap->rankMPIWorld,
		//	"commData",	
		//	pMap->rankDataset,
		//	indexDatasetGroup,
		//	nGroupSize,
		//	nOldCommSize);

		//build the bootstrap communicator
		{
			int				indexBootstrap;

			MPI_Comm_group( pMap->commDataset, &oldGroup );
			MPI_Comm_rank( pMap->commDataset, &myRank );
			MPI_Comm_size( pMap->commDataset, &nOldCommSize);

			indexBootstrap = myRank /pMap->numProcessPerBootstrap;
			pMap->indexBootstrapGroup = indexBootstrap;
			nGroupSize  =	min2( pMap->numProcessPerBootstrap,
				(nOldCommSize % pMap->numProcessPerBootstrap)?
					(nOldCommSize % pMap->numProcessPerBootstrap)
				:	pMap->numProcessPerBootstrap );

			//how many dataset will be processed by my datasetset group
			pMap->numLocalBootstrap = pRunSetting->nBootstrap / pMap->numBootstrapGroup;
			if ( indexBootstrap < pRunSetting->nBootstrap % pMap->numBootstrapGroup)
				pMap->numLocalBootstrap++;


			paRanksInOldGroup = (int *)malloc( sizeof(int) * nGroupSize);
			if( paRanksInOldGroup )
			{
				int	index;
				paRanksInOldGroup[0] = indexBootstrap * pMap->numProcessPerBootstrap;
				for( index=1; index<nGroupSize; index++)
					paRanksInOldGroup[index] = paRanksInOldGroup[index-1] + 1;

				MPI_Group_incl( oldGroup,  nGroupSize, paRanksInOldGroup, &newGroup);

				MPI_Comm_create(  pMap->commDataset, newGroup, &pMap->commBootstrap);
				MPI_Comm_rank( pMap->commBootstrap, &pMap->rankBootstrap);

				MPI_Group_free( &newGroup );
				free( paRanksInOldGroup );

				//debug("boot group %d	size = %d	rank = %d", indexBootstrap,	nGroupSize, pMap->rankBootstrap);
			}
			else
			{
				error("allocate memory failed");
				return FALSE;
			}

			//debug("[%d] %-10s: %5d %5d %5d %5d",
			//	pMap->rankDataset,
			//	"commBOOT",	
			//	pMap->rankBootstrap,
			//	indexBootstrap,
			//	nGroupSize,
			//	nOldCommSize);

			//build the run communicator
			{
				int			indexRunGroup;

				MPI_Comm_group( pMap->commBootstrap, &oldGroup );
				MPI_Comm_rank ( pMap->commBootstrap, &myRank );
				MPI_Comm_size ( pMap->commBootstrap, &nOldCommSize );

				indexRunGroup = myRank /pMap->numProcessPerRun;
				pMap->indexRunGroup = indexRunGroup;
				nGroupSize  =	min2( pMap->numProcessPerRun,
				(nOldCommSize % pMap->numProcessPerRun)?
					(nOldCommSize % pMap->numProcessPerRun)
				:	pMap->numProcessPerRun );

							//how many dataset will be processed by my datasetset group
				pMap->numLocalRun = pRunSetting->nRuns / pMap->numRunGroup;
				if(indexRunGroup < pRunSetting->nRuns % pMap->numRunGroup)
					pMap->numLocalRun++;

				paRanksInOldGroup = (int *)malloc( sizeof(int) * nGroupSize);
				if( paRanksInOldGroup )
				{
					int	index;
					paRanksInOldGroup[0] = indexRunGroup * pMap->numProcessPerRun;
					for( index=1; index<nGroupSize; index++)
						paRanksInOldGroup[index] = paRanksInOldGroup[index-1] + 1;

					MPI_Group_incl( oldGroup,  nGroupSize, paRanksInOldGroup, &newGroup);
					MPI_Comm_create( pMap->commBootstrap, newGroup, &pMap->commRun );
					MPI_Comm_rank( pMap->commRun, &pMap->rankRun);

					MPI_Group_free( &newGroup );
					free( paRanksInOldGroup );
					//debug("run  group %d	size = %d	rank = %d", indexRunGroup,	nGroupSize, pMap->rankRun );
				}
				else
				{
					error("allocate memory failed");
					return FALSE;
				}

				//debug("[%d] %-10s: %5d %5d %5d %5d		rank	index	size	oldsize",
				//	pMap->rankBootstrap,
				//	"commRun",	
				//	pMap->rankBootstrap,
				//	indexRunGroup,
				//	nGroupSize,
				//	nOldCommSize);


				//build the grid topology
				{
					int			dim_size[2];
					int			wrap_around[2];
					int			reorder;

					dim_size[0]	=	pMap->numRows;
					dim_size[1]	=	pMap->numCols;
					wrap_around[0]	=	0;
					wrap_around[1]	=	0;
					reorder			=	1;

					MPI_Cart_create( pMap->commRun, 2, dim_size, wrap_around, reorder, &(pMap->commGrid) );
					MPI_Comm_rank( pMap->commGrid, &pMap->rankGrid );
					MPI_Comm_size( pMap->commGrid, &pMap->npGrid );
					MPI_Cart_coords( pMap->commGrid, pMap->rankGrid, 2, pMap->gridCoords);

					//debug( "grid coords = (%d %d)", pMap->gridCoords[0], pMap->gridCoords[1] );

					//build lower dimension
					{
						int			free_coords[2];

						free_coords[0]	=	0;
						free_coords[1]	=	1;
						MPI_Cart_sub( pMap->commGrid, free_coords, &(pMap->commRow) );
						MPI_Comm_rank( pMap->commRow, &pMap->rankRow );

						free_coords[0]	=	1;
						free_coords[1]	=	0;
						MPI_Cart_sub( pMap->commGrid, free_coords, &(pMap->commCol) );
						MPI_Comm_rank( pMap->commCol, &pMap->rankCol );
					}
				}
			}
		}
	}


	//map the chains with topology
	{
		int		indexChain,
			numChainsOfLastGroup;
		pMap->numTotalChains	=	pRunSetting->nChains;

		//numChainsOfLastGroup	=	pMap->numTotalChains - pMap->rankCol * pRunSetting->nChainsPerGroup;
		//if( numChainsOfLastGroup < pRunSetting->nChainsPerGroup )
		//	pMap->numLocalChains	=	numChainsOfLastGroup;
		//else
		//	pMap->numLocalChains	=	pRunSetting->nChainsPerGroup;

		pMap->numLocalChains = pMap->numTotalChains / pMap->numRows;
		if( pMap->rankCol <pMap->numTotalChains % pMap->numRows )
			pMap->numLocalChains++;

		//we assume summetric layout here, i.e. every processor has the same number of chains.
		//we should relax this later
		pMap->paChainPartition =	(ChainPartition *)malloc( sizeof(ChainPartition) * pMap->numLocalChains );
		if( pMap->paChainPartition )
		{
			for(indexChain=0; indexChain<pMap->numLocalChains; indexChain++)
			{
				pMap->paChainPartition[ indexChain ].globalChainIndex = pMap->rankCol * pRunSetting->nChainsPerGroup + indexChain;
				pMap->paChainPartition[ indexChain ].partitionIndex	  =	pMap->rankRow;
			}
		}
		else
		{
			error("can not allocate memory for paChainGlobalIndex");
			return FALSE;
		}

		pMap->paChainLocations	=	(ChainLocation *)malloc( sizeof(ChainLocation) * pMap->numTotalChains );
		if( pMap->paChainLocations )
		{
			for(indexChain=0; indexChain<pMap->numTotalChains; indexChain++)
			{
				pMap->paChainLocations[ indexChain ].rowIndex	=	indexChain / pRunSetting->nChainsPerGroup;
				pMap->paChainLocations[ indexChain ].localChainIndex =	indexChain % pRunSetting->nChainsPerGroup;
			}

		}
		else
		{
			error("can not allocate memory for paChainLocations");
			return FALSE;
		}
	}

	//printf("[%d] Dataset(%d, %d, %d) Bootstrap(%d, %d, %d) Run(%d, %d, %d) Grid(%d %d %d %d)\n",
	//	g_map.rankMPIWorld,
	//	g_map.rankDataset, g_map.indexDatasetGroup, g_map.numDatasetGroup,
	//	g_map.rankBootstrap, g_map.indexBootstrapGroup, g_map.numBootstrapGroup,
	//	g_map.rankRun, g_map.indexRunGroup, g_map.numRunGroup,
	//	g_map.rankCol, g_map.rankRow, g_map.numCols, g_map.numRows);

	/*dumpChainProcessMap( pMap );*/
	return TRUE;

}

void dumpChainProcessMap( ChainProcessMap *pMap)
{
	int		indexChain;
	printf("Processor-Chain Map Information\n");
	printf("[%d]Hostname: %s\n", pMap->rankMPIWorld, g_ProcName );
	printf("GlobalRank  : %d\n", pMap->rankMPIWorld);
	printf("DatasetRank : %d\n", pMap->rankDataset);
	printf("BootstrapRk : %d\n", pMap->rankBootstrap);
	printf("RunRank     : %d\n", pMap->rankRun);
	printf("Grid        : (%d %d) at %5d - %-5d grid\n", pMap->gridCoords[0], pMap->gridCoords[1], pMap->numRows, pMap->numCols);
	printf("Row         : %d at row %d\n", pMap->rankRow, pMap->rankCol);
	printf("Col         : %d at col %d\n", pMap->rankCol,  pMap->rankRow );
	printf("LocalChains : %d\n", pMap->numLocalChains);
	for(indexChain=0; indexChain<pMap->numLocalChains; indexChain++)
		printf("local chain #%d : [%d - %d]\n", indexChain, pMap->paChainPartition[ indexChain ].globalChainIndex, pMap->paChainPartition[ indexChain ].partitionIndex);

	printf("GlobalChains : %d\n", pMap->numTotalChains);
	for(indexChain=0; indexChain<pMap->numTotalChains; indexChain++)
		printf("chain #%d = [%d - %d]\n", indexChain, pMap->paChainLocations[indexChain].rowIndex, pMap->paChainLocations[indexChain].localChainIndex);
}

boolean	distributeDataset_p( RunSetting *pRunSetting, CMatrix *cmDatasetList )
{
	FILE *in;
	boolean	fProcessOk = FALSE;	

	//message_p( "distributeDataset_p: get data set list");
	if( g_map.rankMPIWorld == HEAD_NODE ) 
	{
		in = fopen( pRunSetting->sDatasetListFile, "r" );
		if( in )
		{
			int	index;
			fProcessOk = TRUE;
			for( index=0; index<pRunSetting->nDataset; index++)
			{
				if ( fscanf(in, "%s", cmDatasetList->data[index]) < 1 )
				{
					printf("dataset list is invalid\n");
					fProcessOk = FALSE;
					break;
				}
				//printf("dataset #%d: %s\n", index, cmDatasetList->data[index]);
			}
			fclose( in );
		}
		else
		{
			error("can not open datafile list: %s", cmDatasetList);
			fProcessOk = FALSE;	
		}
	}

	MPI_Bcast( &fProcessOk, sizeof(boolean), MPI_CHAR, HEAD_NODE, MPI_COMM_WORLD);
	if( fProcessOk )
	{
		int	indexGlobal, indexLocal;

		MPI_Bcast( cmDatasetList->__data, cmDatasetList->nrows * (cmDatasetList->ncols + 1), MPI_CHAR, HEAD_NODE, MPI_COMM_WORLD);

		for(indexLocal=0, indexGlobal=g_map.indexDatasetGroup; 
			indexGlobal<pRunSetting->nDataset; 
			indexLocal++, indexGlobal+=g_map.numDatasetGroup)
		{
			tokenSafeCopy( cmDatasetList->data[indexLocal], MAX_FILE_NAME_LEN, cmDatasetList->data[indexGlobal]);
			//printf("[%d]:%d %s\n", g_map.rankMPIWorld, indexLocal, cmDatasetList->data[indexLocal]);
		}
	}
	
	return fProcessOk;
}

void showChainProcessMap( )
{
	ChainProcessMap	 *globalMaps;

	globalMaps = (ChainProcessMap *)malloc( sizeof(ChainProcessMap) * g_map.numTotalProcess );
	//printf("Local[%d] Dataset(%d, %d, %d) Bootstrap(%d, %d, %d) Run(%d, %d, %d) Grid(%d %d %d %d)\n",
	//	g_map.rankMPIWorld,
	//	g_map.rankDataset, g_map.indexDatasetGroup, g_map.numDatasetGroup,
	//	g_map.rankBootstrap, g_map.indexBootstrapGroup, g_map.numBootstrapGroup,
	//	g_map.rankRun, g_map.indexRunGroup, g_map.numRunGroup,
	//	g_map.rankCol, g_map.rankRow, g_map.numCols, g_map.numRows);
	//fflush(stdout);

	MPI_Gather( &g_map, sizeof(ChainProcessMap), MPI_CHAR, 
		globalMaps, sizeof(ChainProcessMap), MPI_CHAR, 
		HEAD_NODE, MPI_COMM_WORLD);

	if( g_map.rankMPIWorld == HEAD_NODE )
	{
		int	index;
		printf("\nBuild Chain Process Map\n");
		for(index = 0; index<g_map.numTotalProcess; index++)
		{
			printf("[%d] Dataset(%d, %d, %d) Bootstrap(%d, %d, %d) Run(%d, %d, %d) Grid(%d %d %d %d)\n",
				globalMaps[index].rankMPIWorld,
				globalMaps[index].rankDataset, globalMaps[index].indexDatasetGroup, globalMaps[index].numDatasetGroup,
				globalMaps[index].rankBootstrap, globalMaps[index].indexBootstrapGroup, globalMaps[index].numBootstrapGroup,
				globalMaps[index].rankRun, globalMaps[index].indexRunGroup, globalMaps[index].numRunGroup,
				globalMaps[index].rankCol, globalMaps[index].rankRow, globalMaps[index].numRows, globalMaps[index].numCols);
		
		}
		printf("\n");
		fflush( stdout );
	}
	free( globalMaps );
}

void setRNGSeed_p( RunSetting *pRunSetting )
{
	unsigned long	seed;
	//random number for each row
	switch( pRunSetting->RNG1_seed.type )
	{
	case SEED_DEFAULT:
		seed =	(unsigned long) (g_map.rankMPIWorld+13); 
		break;
	case SEED_CURR_TIME:
		seed	=	(unsigned long) time( NULL );
		break;
	case SEED_USER_DEFINED:
		seed	= pRunSetting->RNG1_seed.seed;
		break;
	default:
		seed =	(unsigned long) (g_map.rankMPIWorld+13);
		break;
	}
	seed = seed * (g_map.rankMPIWorld + 11);
	MPI_Bcast( &(seed), sizeof(unsigned long), MPI_CHAR, HEAD_NODE, g_map.commRow );
	pRunSetting->RNG1_seed.seed	= seed;

	RNG1_seed( pRunSetting->RNG1_seed.seed );

	message_p("RNG1: seed type=%d seed=%d", pRunSetting->RNG1_seed.type, seed);
	//random number for each grid
	switch( pRunSetting->RNG2_seed.type )
	{
	case SEED_DEFAULT:
		seed =	(unsigned long) (g_map.rankMPIWorld+23); 
		break;
	case SEED_CURR_TIME:
		seed	=	(unsigned long) time( NULL );
		break;
	case SEED_USER_DEFINED:
		seed	= pRunSetting->RNG1_seed.seed;
		break;
	default:
		seed =	(unsigned long) (g_map.rankMPIWorld+23);
		break;
	}
	seed = seed * (g_map.rankMPIWorld + 17);
	MPI_Bcast( &(seed), sizeof(unsigned long), MPI_CHAR, HEAD_NODE, g_map.commGrid );
	RNG2_seed( pRunSetting->RNG2_seed.seed );
	message_p("RNG2: seed type=%d seed=%d", pRunSetting->RNG2_seed.type, seed);
}
