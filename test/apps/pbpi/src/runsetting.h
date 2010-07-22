#ifndef _PBPI_RUNSETTING_H
#define	_PBPI_RUNSETTING_H

#include "pbpi.h"
#include "xmltoken.h"
#include "model.h"
#include "util.h"
#include "tree.h"

typedef enum DatasetFormat	{FORMAT_NEXUS, FORMAT_PHYLIP, FORMAT_UNKNOWN} DatasetFormat;

typedef enum RNGSeed_Type
{
	SEED_DEFAULT,
	SEED_CURR_TIME,
	SEED_USER_DEFINED
}RNGSeed_Type;

typedef struct PBPISeed
{
	RNGSeed_Type	type;
	unsigned long	seed;
}PBPISeed;


typedef struct RunSetting
{
	
	char			sDatasetFile[MAX_FILE_NAME_LEN+1],
					sDataSourceDir[MAX_FILE_NAME_LEN+1],
					sWorkingDir[MAX_FILE_NAME_LEN+1],
					sDatasetListFile[MAX_FILE_NAME_LEN+1],
					sTrueTreeFile[MAX_FILE_NAME_LEN+1],
					sLogFile[MAX_FILE_NAME_LEN+1],
					sOutputPrefix[MAX_FILE_NAME_LEN+1];

	int				flag,
					nMaxChainGenerations,	//the maximum generations
					nSampleInterval,		//sample after how many generations
					nPrintInterval,			//print after how many generations
					nExchangeInterval,
					nDataset,
					nBootstrap,
					nRuns,
					nChains,				//number of chains
					nProcessPerChain,		//how many process this chain will span
					nChainsPerGroup,		//how many chains on each process
					nMTMTries,				//number of tries in MTM
					nProcess,				//# of process 
					nTemperatureSchema,		//the cooling schema
					lastProposalType,		//last proposal type
					nSumTreeBurnIn,
					nSampleBurnIn,
					nProposalStep;

	boolean			fEnableMTM,				//enable/disable MTM or not
					fEnablePT,				//enable/diable parallel tempering
					fEnableExchange,		//enable/disable exchange between chains
					fLogEnabled,			//enable/diable loging
					fRunMCMC,				//enable/disable MCMC sampling
					fRunSumt,				//enable/disable MCMC summary
					fSumTreeProb,
					fSumCladeProb,
					fKnowTrueTree,
					fEmbeddedTrueTree,
					fEnableChainParallism,
					fEnableSequenceParalleism,
					fEnableBootstrapParallelism,
					fEnableRunParallelism,
					fEnableDatasetParallelism,
					fEnableBatchProcessing,
					fEnableDirectoryPerDataset,
					fEnableInchainResampling;

	double			dDesiredExchangeAcceptance,	//how much exchange rate are desired
					dDesiredLikelihoodGap,		//the expected maximum likelihood decrease that can be accepted
					dExchangeRate,				//the exchange rate
					dMajorityRatio,
					dCredibleSetPercentage,
					dInchainSamplingRate,
					dInchainSamplingKnife,
					dInchainSamplingRatio,
					dBootstrapSamplingRatio,
					dBootstrapSamplingKnife;				
	
	
	DatasetFormat	datasetFormat;
	ModelType		modelType;				//the modeltype

	PBPISeed		RNG1_seed,				//seed for each row
					RNG2_seed;				//seed for each column
	
	FILE			*fpLog,					//log file
					*fpTFile,				//tree file
					*fpPFile;				//param file

	PhyloTree		TrueTree;				//the true tree
}RunSetting;

int SetRunSetting(RunSetting *pRS, XMLTokenSpace *tokenspace, char *value);


//read RunSetting from file <sRunSettingFile>
//requires: 
//effects: pRS is modified by values provided in <sRunSettingFile>
//return:
//	PBPI_SUCCESS	if no error
//	PBPI_ERROR		if has error
boolean	readRunSetting(char *sRunSettingFile, RunSetting *pRS);
void dumpRunSetting(RunSetting *pRunSetting);

//save RunSetting to file <sRunSettingFile>
//requires: pRS has a set of RunSetting values
//effects:  values provided saved to <sRunSettingFile>
//return:
//	PBPI_SUCCESS	if no error
//	PBPI_ERROR		if has error
int	saveRunSetting(char *sRunSettingFile, RunSetting *pRS);

#endif

