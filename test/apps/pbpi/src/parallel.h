#ifndef _PBPI_PARALLEL_H
#define _PBPI_PARALLEL_H

#include "runsetting.h"
#include "dataset.h"
#include "pbpi.h"

typedef int		ProcessRank;
typedef int		ChainID;


#define			HEAD_NODE			0

//typedef struct ChainView
//{
//	ChainID		chainId;
//	int			nProcess;
//	ProcessRank *processList;
//}ChainView;
//
//typedef struct ProcessView
//{
//	ProcessRank	processRank;
//	int			nChains;
//	ChainID		*chainList;
//}ProcessView;

typedef struct ChainLocation
{
	int			rowIndex,
				localChainIndex;
}ChainLocation;

typedef struct ChainPartition
{
	int			partitionIndex,
				globalChainIndex;
}ChainPartition;

typedef struct ChainProcessMap
{
	MPI_Comm		commDataset,
					commBootstrap,
					commRun,
					commGrid,
					commRow,
					commCol;

	int				numDatasetGroup,
					numBootstrapGroup,
					numRunGroup,
					numProcessPerRun,
					numProcessPerBootstrap,
					numProcessPerDataset,
					numRows,
					numCols,
					indexDatasetGroup,
					indexBootstrapGroup,
					indexRunGroup,
					rankRow,
					rankCol,
					rankGrid,
					rankRun,
					rankBootstrap,
					rankDataset,
					rankMPIWorld,
					gridCoords[2],
					npGrid,
					numTotalProcess,
					numTotalChains,
					numLocalChains,
					numLocalDataset,
					numLocalRun,
					numLocalBootstrap;
	ChainPartition*	paChainPartition;
	ChainLocation *	paChainLocations;
}ChainProcessMap;

typedef	struct ChainLogLData
{
	int			globalIndex;
	boolean		isColdChin;
	double		lnL;
}ChainLogLData;

typedef	struct ChainPrintInfo
{
	int			rankMPIWorld,
				globalIndex,
				localIndex,
				isColdChin;
	double		lnL;
}ChainPrintInfo;

typedef struct ChainSwapInfo
{
	boolean		isColdChain;
	double		fTemperature,
				lnL;
} ChainSwapInfo;

typedef struct TreeLogLandPrintBufferLen
{
	int			len;
	double		lnL;
}TreeLogLandPrintBufferLen;

//boolean buildChainProcessMap( ChainProcessMap *pMap, int nChains, int nProcess, int nProcessPerChain);
boolean buildChainProcessMap( ChainProcessMap *pMap, RunSetting *pRuncSetting);
void dumpChainProcessMap( ChainProcessMap *pMap);
void showChainProcessMap( );

boolean	initMPIEnviornment(int *argc, char ***argv);
void message_p( const char *fmt, ...);
void showProgramInformation( void );
void exitProgram( int nReturnCode );
boolean	readRunsetting_p( char *szRunsettingFile, RunSetting *pRuncSetting );
boolean readDataset_p(RunSetting *pRunSetting, Dataset *pDataset);
boolean	distributeDataset_p( RunSetting *pRunSetting, CMatrix *cmDatasetList );


#endif //_PBPI_PARALLEL_H

