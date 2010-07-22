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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>
#include "runsetting.h"
#include "error.h"
#include "xmltoken.h"
#include "util.h"
#include "pbpi.h"

void initRunSetting( RunSetting *pRunSetting)
{
	pRunSetting->datasetFormat = FORMAT_NEXUS;
	pRunSetting->modelType = JC69;
	pRunSetting->nChains = 1;
	pRunSetting->nDataset = 1;
	pRunSetting->nRuns = 1;
	pRunSetting->nBootstrap = 1;
	pRunSetting->nMaxChainGenerations = 100;
	pRunSetting->nProcess = 1;
	pRunSetting->nMTMTries = 1;
	pRunSetting->nProcessPerChain = 1;
	pRunSetting->fEnableExchange		=	FALSE;
	pRunSetting->fRunMCMC				=	FALSE;
	pRunSetting->fRunSumt				=	FALSE;
	pRunSetting->fEnableMTM				=	FALSE;
	pRunSetting->fEnablePT				=	FALSE;
	pRunSetting->fSumCladeProb			=	TRUE;
	pRunSetting->fSumTreeProb			=	TRUE;
	pRunSetting->fKnowTrueTree			=	FALSE;
	pRunSetting->fEmbeddedTrueTree		=	FALSE;
	pRunSetting->fEnableDirectoryPerDataset = FALSE;
	pRunSetting->fEnableBatchProcessing		= FALSE;
	pRunSetting->fEnableInchainResampling			= FALSE;
	pRunSetting->nProposalStep				= 1;

	//parallelism
	pRunSetting->fEnableBootstrapParallelism	=	TRUE;
	pRunSetting->fEnableChainParallism			=	TRUE;
	pRunSetting->fEnableDatasetParallelism		=	TRUE;
	pRunSetting->fEnableRunParallelism			=	TRUE;
	pRunSetting->fEnableSequenceParalleism		=	TRUE;
	pRunSetting->dInchainSamplingRate			=	0.1;
	pRunSetting->dInchainSamplingRatio			=	1.0;
	pRunSetting->dInchainSamplingKnife			=	1.0;

	pRunSetting->nSumTreeBurnIn			=	0;
	pRunSetting->dMajorityRatio			=	0.50;
	pRunSetting->dCredibleSetPercentage	=	0.95;
	tokenSafeCopy(pRunSetting->sDatasetListFile, MAX_FILE_NAME_LEN, "dataset.lst");
	tokenSafeCopy(pRunSetting->sDataSourceDir, MAX_FILE_NAME_LEN, ".");
	tokenSafeCopy(pRunSetting->sWorkingDir, MAX_FILE_NAME_LEN, ".");
	pRunSetting->sTrueTreeFile[0] = '\0';
	tokenSafeCopy(pRunSetting->sLogFile, MAX_FILE_NAME_LEN, "pbpi.log");

	pRunSetting->dBootstrapSamplingKnife = 1.0;
	pRunSetting->dBootstrapSamplingRatio = 1.0;

}

int SetRunSetting(RunSetting *pRS, XMLTokenSpace *tokenspace, char *value)
{
	char *tokenSpaceString;
	long lTmp;
	unsigned long ulTmp;
	
	pRS->fLogEnabled = FALSE;
	tokenSpaceString = tokenSpace2String(tokenspace);
	/*debug("Token Path = %s", tokenSpaceString);*/
	
	if( tokenMatch(tokenSpaceString, "dataset::file::", FALSE) ){
		 tokenSafeCopy(pRS->sDatasetFile, MAX_FILE_NAME_LEN, value);
	}

	else if( tokenMatch(tokenSpaceString, "dataset::batch::enable::", FALSE) ){
		pRS->fEnableBatchProcessing = tokenMatch( value, "yes", TRUE ) ? TRUE : FALSE;
	}

	else if( tokenMatch(tokenSpaceString, "dataset::batch::num_dataset::", FALSE) ){
		pRS->nDataset = atoi( value );
	}
	
	else if( tokenMatch(tokenSpaceString, "dataset::batch::data_dir::", FALSE) ){
		tokenSafeCopy(pRS->sDataSourceDir, MAX_FILE_NAME_LEN, value);
	}
	else if( tokenMatch(tokenSpaceString, "dataset::batch::work_dir::", FALSE) ){
		tokenSafeCopy(pRS->sWorkingDir, MAX_FILE_NAME_LEN, value);
	}


	else if( tokenMatch(tokenSpaceString, "dataset::batch::filelist::", FALSE) ){
		tokenSafeCopy(pRS->sDatasetListFile, MAX_FILE_NAME_LEN, value);
	}
	else if( tokenMatch(tokenSpaceString, "dataset::batch::directory_per_dataset::", FALSE) ){
		pRS->fEnableDirectoryPerDataset = tokenMatch( value, "yes", TRUE ) ? TRUE : FALSE;	
	}


	else if ( tokenMatch(tokenSpaceString, "dataset::file::", FALSE) ){
		if(tokenMatch(value, "nexus", TRUE)){
			pRS->datasetFormat = FORMAT_NEXUS;
		}else if (tokenMatch(value, "nexus", TRUE)){
			pRS->datasetFormat = FORMAT_PHYLIP;
		}else{
			pRS->datasetFormat = FORMAT_UNKNOWN;
		}
	}

	else if( tokenMatch(tokenSpaceString, "mcmc::nrun::", FALSE) )
	{
		pRS->nRuns = atoi( value );
	}
	else if( tokenMatch(tokenSpaceString, "mcmc::number_of_chains::", FALSE) )
	{
		pRS->nChains = atoi( value );
	}
	else if( tokenMatch(tokenSpaceString, "mcmc::maximum_generation::", FALSE) )
	{
		pRS->nMaxChainGenerations = atoi( value );
	}
	else if( tokenMatch(tokenSpaceString, "mcmc::sample_interval::", FALSE) )
	{
		pRS->nSampleInterval = atoi( value );
	}
	else if( tokenMatch(tokenSpaceString, "mcmc::print_interval::", FALSE) )
	{
		pRS->nPrintInterval = atoi( value );
	}
	else if( tokenMatch(tokenSpaceString, "mcmc::nburnin::", FALSE) )
	{
		pRS->nSampleBurnIn = atoi( value );
	}

	//inchain Resampling
	else if( tokenMatch(tokenSpaceString, "mcmc::bootstrap::enable::", FALSE) )
	{
		if ( tokenMatch( value, "yes", FALSE) )
			pRS->fEnableInchainResampling = TRUE;
		else
			pRS->fEnableInchainResampling = FALSE;
	}
	else if( tokenMatch(tokenSpaceString, "mcmc::bootstrap::samplingRate::", FALSE) )
	{
		pRS->dInchainSamplingRate = atof( value );
	}
	else if( tokenMatch(tokenSpaceString, "mcmc::bootstrap::samplingRatio::", FALSE) )
	{
		pRS->dInchainSamplingRatio = atof( value );
	}
	else if( tokenMatch(tokenSpaceString, "mcmc::bootstrap::samplingKnife::", FALSE) )
	{
		pRS->dInchainSamplingKnife = atof( value );
	}

	//mcmc exchange
	else if( tokenMatch(tokenSpaceString, "mcmc::exchange::enable::", FALSE) )
	{
		if ( tokenMatch( value, "yes", FALSE) )
			pRS->fEnableExchange = TRUE;
		else
			pRS->fEnableExchange = FALSE;
	}
	else if( tokenMatch(tokenSpaceString, "mcmc::exchange::exchangeRate::", FALSE) )
	{
		pRS->dExchangeRate = atof( value );
		if( pRS->dExchangeRate <= 0 )
			pRS->dExchangeRate = 1.0e-10;
		pRS->nExchangeInterval = 1.0 / pRS->dExchangeRate;
	}
	else if( tokenMatch(tokenSpaceString, "mcmc::exchange::exchange_interval::", FALSE) )
	{
		pRS->nExchangeInterval = atoi( value );
		if( pRS->nExchangeInterval )
			pRS->nExchangeInterval = 1;
	}
	else if( tokenMatch(tokenSpaceString, "mcmc::exchange::parallelTempering::enable::", FALSE) )
	{
		/*debug("enable PT ");*/
		if ( tokenMatch( value, "yes", FALSE) )
			pRS->fEnablePT = TRUE;
		else
			pRS->fEnablePT = FALSE;
	}
	else if( tokenMatch(tokenSpaceString, "mcmc::exchange::parallelTempering::temperature_schema::method::", FALSE) )
	{
		pRS->nTemperatureSchema = atoi( value );
	}
	else if( tokenMatch(tokenSpaceString, "mcmc::exchange::parallelTempering::temperature_schema::acceptance_ratio::", FALSE) )
	{
		pRS->dDesiredExchangeAcceptance = atof( value );
	}

	else if( tokenMatch(tokenSpaceString, "mcmc::exchange::parallelTempering::temperature_schema::maximum_gap::", FALSE) )
	{
		pRS->dDesiredLikelihoodGap = atof( value );
	}

	else if( tokenMatch(tokenSpaceString, "bootstrap::nrepeat::", FALSE) )
	{
		pRS->nBootstrap = atoi( value );
	}
	else if( tokenMatch(tokenSpaceString, "bootstrap::samplingRatio::", FALSE) )
	{
		pRS->dBootstrapSamplingRatio = atof( value );
	}
	else if( tokenMatch(tokenSpaceString, "bootstrap::samplingKnife::", FALSE) )
	{
		pRS->dBootstrapSamplingKnife = atof( value );
	}



	else if( tokenMatch(tokenSpaceString, "parallel::paralleism::dataset_level::enable::", FALSE) )
	{
		pRS->fEnableDatasetParallelism	= tokenMatch( value, "yes", FALSE ) ? TRUE : FALSE;
	}

	else if( tokenMatch(tokenSpaceString, "parallel::paralleism::bootstrap_level::enable::", FALSE) )
	{
		pRS->fEnableBootstrapParallelism = tokenMatch( value, "yes", FALSE ) ? TRUE : FALSE;
	}

	else if( tokenMatch(tokenSpaceString, "parallel::paralleism::run_level::enable::", FALSE) )
	{
		pRS->fEnableRunParallelism= tokenMatch( value, "yes", FALSE ) ? TRUE : FALSE;
	}

	else if( tokenMatch(tokenSpaceString, "parallel::paralleism::chain_level::enable::", FALSE) )
	{
		pRS->fEnableChainParallism = tokenMatch( value, "yes", FALSE ) ? TRUE : FALSE;
	}

	else if( tokenMatch(tokenSpaceString, "parallel::paralleism::chain_level::num_chains_per_group::", FALSE) )
	{
		pRS->nChainsPerGroup = atoi( value );
	}

	else if( tokenMatch(tokenSpaceString, "parallel::paralleism::sequence_level::enable::", FALSE) )
	{
		pRS->fEnableSequenceParalleism	= tokenMatch( value, "yes", FALSE ) ? TRUE : FALSE;
	}

	else if( tokenMatch(tokenSpaceString, "parallel::paralleism::sequence_level::num_partitions::", FALSE) )
	{
		pRS->nProcessPerChain = atoi( value );
	}
	
	else if( tokenMatch(tokenSpaceString, "model::type::", FALSE) )
	{
		//debug("pre set model type is OK");
		pRS->modelType = getModelType( value );
		//debug("set model type is OK");
	}
	
	else if( tokenMatch(tokenSpaceString, "mcmc::random_number_generator::rng1::method::", FALSE) )
	{
		switch( atoi( value) )
		{
		case 0:
			pRS->RNG1_seed.type = SEED_DEFAULT;
			break;
		case 1:
			pRS->RNG1_seed.type = SEED_CURR_TIME;
			break;
		case 2:
			pRS->RNG1_seed.type = SEED_USER_DEFINED;
			break;
		default:
			break;
		}
	}
	else if( tokenMatch(tokenSpaceString, "mcmc::random_number_generator::rng1::seed::", FALSE) )
	{
		lTmp = atol( value );
		if ( lTmp < 0 )	
		{
			ulTmp = time( NULL );
		}
		else 
		{
			ulTmp = lTmp;
		}
		pRS->RNG1_seed.seed = ulTmp;
	}
	else if( tokenMatch(tokenSpaceString, "mcmc::random_number_generator::rng2::method::", FALSE) )
	{
		switch( atoi( value) )
		{
		case 0:
			pRS->RNG2_seed.type = SEED_DEFAULT;
			break;
		case 1:
			pRS->RNG2_seed.type = SEED_CURR_TIME;
			break;
		case 2:
			pRS->RNG2_seed.type = SEED_USER_DEFINED;
			break;
		default:
			break;
		}
	}
	else if( tokenMatch(tokenSpaceString, "mcmc::random_number_generator::rng2::seed::", FALSE) )
	{
		lTmp = atol( value );
		if ( lTmp < 0 )	
		{
			ulTmp = time( NULL );
		}
		else 
		{
			ulTmp = lTmp;
		}
		pRS->RNG1_seed.seed = ulTmp;
	}
	else if( tokenMatch(tokenSpaceString, "log::filename::", FALSE) )
	{
		tokenSafeCopy(pRS->sLogFile, MAX_FILE_NAME_LEN, value);
		pRS->fLogEnabled = TRUE;
	}
	else if( tokenMatch(tokenSpaceString, "task::mcmc::", FALSE) )
	{
		pRS->fRunMCMC	=	tokenMatch( value, "yes", FALSE ) ? TRUE : FALSE;
	}

	else if( tokenMatch(tokenSpaceString, "task::sumt::", FALSE) )
	{
		pRS->fRunSumt	=	tokenMatch( value, "yes", FALSE ) ? TRUE : FALSE;
	}

	else if( tokenMatch(tokenSpaceString, "sumt::burn_in::", FALSE) )
	{
		pRS->nSumTreeBurnIn	=	atoi( value );
	}

	else if( tokenMatch(tokenSpaceString, "sumt::tree_prob::", FALSE) )
	{
		pRS->fSumTreeProb	=	tokenMatch( value, "yes", FALSE ) ? TRUE : FALSE;
	}

	else if( tokenMatch(tokenSpaceString, "sumt::clade_prob::", FALSE) )
	{
		pRS->fSumCladeProb	= tokenMatch( value, "yes", FALSE ) ? TRUE : FALSE;
	}
	
	else if( tokenMatch(tokenSpaceString, "sumt::true_tree::known::", FALSE) )
	{
		pRS->fKnowTrueTree	= tokenMatch( value, "yes", TRUE ) ? TRUE : FALSE;
	}

	else if( tokenMatch(tokenSpaceString, "sumt::true_tree::embedded::", FALSE) )
	{
		pRS->fEmbeddedTrueTree	= tokenMatch( value, "yes", TRUE ) ? TRUE : FALSE;
	}

	else if( tokenMatch(tokenSpaceString, "sumt::true_tree::treefile::", FALSE) )
	{
		tokenSafeCopy( pRS->sTrueTreeFile, MAX_FILE_NAME_LEN, value);
	}

	else if( tokenMatch(tokenSpaceString, "sumt::majority::", FALSE) )
	{
		pRS->dMajorityRatio	= atof( value );
	}

	else if( tokenMatch(tokenSpaceString, "sumt::percentage::", FALSE) )
	{
		pRS->dCredibleSetPercentage	= atof( value );
	}
	return PBPI_SUCCESS;
}

//read RunSetting from file <sRunSettingFile>
//requires: 
//effects: pRS is modified by values provided in <sRunSettingFile>
//return:
//	TRUE	if no error
//	FALSE	if has error
boolean	readRunSetting(char *sRunSettingFile, RunSetting *pRS)
{
	XMLToken		xmltoken,
					lasttoken;
	XMLTokenSpace	tokenspace;
	FILE		*	fpIn;
	boolean			fReadOk		=		FALSE;
						
	assert( sRunSettingFile );
	//initialize the runsetting
	initRunSetting( pRS );

	fpIn = fopen(sRunSettingFile, "r");
	if( fpIn )
	{	
		fReadOk	=	TRUE;
		while( fReadOk )
		{
			fReadOk = (PBPI_SUCCESS == nextXMLToken( fpIn, &xmltoken));
			//dumpXMLToken(&xmltoken);
			if( xmltoken.type == XML_KEY_BEGIN ){
				tokenStackPush( &xmltoken );
			}
			else if( xmltoken.type == XML_KEY_END ){
				tokenStackPop( &xmltoken );
			}else if ( xmltoken.type == XML_VALUE ){
				//tokenStackPeek( &lasttoken );
				//debug("name = (%s) value = (%s)\n",	lasttoken.token, xmltoken.token);
				getTokenSpace( &tokenspace );
				SetRunSetting(pRS, &tokenspace, xmltoken.token);
			}
		}
		
		fclose(fpIn);	
		
		pRS->flag = TRUE;
		fReadOk	=	TRUE;
	}
	else
	{
		error("Can not read file (%s)", sRunSettingFile);
	}

	return fReadOk;
}

//save RunSetting to file <sRunSettingFile>
//requires: pRS has a set of RunSetting values
//effects:  values provided saved to <sRunSettingFile>
//return:
//	PBPI_SUCCESS	if no error
//	PBPI_ERROR		if has error
int	saveRunSetting(char *sRunSettingFile, RunSetting *pRS)
{
	FILE *fpOut;

	assert( sRunSettingFile );
	if ( (fpOut = fopen(sRunSettingFile, "w+")) == NULL ){
		warning("Can not write file (%s)", sRunSettingFile);
		goto open_runsetting_file_error;
	}
	
	fclose(fpOut);
	return PBPI_SUCCESS;

open_runsetting_file_error:
	fclose(fpOut);
	return PBPI_ERROR;
}

void dumpRunSetting(RunSetting *pRunSetting)
{
	printf("%-10s:%s\n",	"DatasetFile",	pRunSetting->sDatasetFile);
	printf("%-10s:%5d\n",	"PPC",	pRunSetting->nProcessPerChain);	
	printf("%-10s:%5d\n",	"CPG",	pRunSetting->nChainsPerGroup);	
}

