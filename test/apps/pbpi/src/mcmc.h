#ifndef _PBPI_MCMC_H
#define _PBPI_MCMC_H

#include "runsetting.h"
#include "dataset.h"
#include "model.h"
#include "tree.h"
#include "pbpi.h"
#include "parallel.h"

typedef struct MarkovState
{
	PhyloTree	tree;
	Model		model;
}MarkovState;

//typedef struct MarkovChain
//{
//	int			nGlobalIndex;	     //index of global chains
//	int			nLocalIndex;	     //index of local chains
//	int			nGeneration;	     //current generations
//	int			nTries;			     //number of tries, used by MTM
//	
//	MarkovState	_States[2];		     //we allocate these two states, and therefore we can swap current and proposal states
//								     //without copying
//	MarkovState	*pCurrentState;	     //current state
//	MarkovState	*pCandidateState;    //proposed state
//	int			nAccepts;
//	int			nRejects;
//	int			nTotalExchanges;
//	int			nAcceptedExchanges;
//	double		fTemperature;
//	boolean		fColdChain;
//}MarkovChain;

//MTM Version
typedef struct MarkovChain
{
	int			nGlobalIndex;	     //index of global chains
	int			nLocalIndex;	     //index of local chains
	int			nGeneration;	     //current generations
	int			nTries;			     //number of tries, used by MTM
	int			*pTreeDataMap;		//index map between states with sitelikedata
	
	MarkovState	_States[2];		     //we allocate these two states, and therefore we can swap current and proposal states
								     //without copying
	MarkovState	*pTryStates;		 //State for tries
	MarkovState	*pCurrentState;	     //current state
	MarkovState	*pCandidateState;    //proposed state
	int			nAccepts;
	int			nRejects;
	int			nTotalExchanges;
	int			nAcceptedExchanges;
	double		fTemperature;
	boolean		fColdChain;
}MarkovChain;

boolean  runMarkovChains(int indexDataset, int indexBootstrap, int indexRun);
void	swapCurrentStateWithCandidateState(MarkovChain *pChain);
double	getChainTemperature(int chainIndex, int numChains, int schemaID);

#endif	//_PBPI_MCMC_H

