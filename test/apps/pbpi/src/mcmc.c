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

#include <math.h>
#include <string.h>
#include "mcmc.h"
#include "runsetting.h"
#include "error.h"
#include "dataset.h"
#include "parallel.h"
#include "pbpi.h"
#include "tree.h"
#include "util.h"
#include "model.h"
#include "likelihood.h"
#include "rand.h"
#include "rand2.h"
#include "proposal.h"
#include "align_alloc.h"

#include <tpc_common.h>
#include <tpc_ppe.h>

//all the following global variable are initialized in runMarkovChains
MarkovChain		*g_chains;			//pointer to chains on local process


extern RunSetting		g_rs;		//current runsetting
extern Dataset			g_ds;		//pointer to current dataset
extern ChainProcessMap	g_map;		//pointer to current chain-process map
double *			g_likedata;		//global site likelihood
double **			g_leafdata;		//all leaf nodes site likelihood
//since the sitelikehood values of all leaf nodes 
//are same during the computations, we only need 
//to keep one copy of these data
double **			g_intldata;			//data for internal site likelihood
//this version we share the internal data first
//then we try use individual internal data
long				g_nodeBlockSize;
long				g_treeBlockSize;

size_t				gulBaseMemSize;
size_t				gulChainMemSize;
size_t				gulGlobalMemSize;

ChainPrintInfo	*	g_printSendBuffer,
*	g_printRecvBuffer;

double			*	g_paPartLogL,
				*	g_paWholeLogL;
ChainSwapInfo		chainSwapSendBuf,
chainSwapRecvBuf;

extern			int			g_Myrank;
int							g_coldChainIndex = -1;

char				*		g_treePrintBuffer = 0;
TreeLogLandPrintBufferLen	g_treePrintBufferLen;
int					*		g_nodelist;

unsigned int g_likedata_size = 0;
unsigned int g_leafdata_size = 0;
unsigned int g_intldata_size = 0;

void showDataPartition( void )
{
	int localInfo[7], *globalInfo, index;


	//message_p( "show Data partition");

	globalInfo = (int *)c_malloc( sizeof(int) * 7 * g_map.numCols );

	localInfo[0] = g_ds.nPattern;
	localInfo[1] = g_ds.nTotalPatterns;
	localInfo[2] = g_ds.nPartPos[0];
	localInfo[3] = g_ds.nPartPos[1];
	localInfo[4] = g_map.rankMPIWorld;
	localInfo[5] = g_map.rankCol;
	localInfo[6] = g_map.rankRow;



	MPI_Gather( &(localInfo[0]), sizeof(int)*7, MPI_CHAR, 
		globalInfo, sizeof(int)*7, MPI_CHAR, 
		HEAD_NODE, g_map.commRow);

	if( g_map.rankRow == HEAD_NODE && g_map.rankCol == 0 )
	{
		printf("\nDATA PARTITION\n");
		for(index=0; index<g_map.numCols; index++)
		{
			printf("   [%d] (%d %d): %d-%d (%-5d / %5d)\n",
				globalInfo[index*7 + 4], globalInfo[index*7 + 5], globalInfo[index*7 + 6], 
				globalInfo[index*7 + 2], globalInfo[index*7 + 3], globalInfo[index*7 + 0], globalInfo[index*7 + 1]);
		}
		fflush( stdout );
	}

	free( globalInfo );

}

void partitionDataset( void )
{
	int		i0,	m0, p, n, i, j;

	n = g_ds.nTotalPatterns;
	p = g_map.numCols;

	i0 = n % p;
	m0 = n / p;

	if( g_map.rankRow < i0 )
	{
		g_ds.nPattern = m0 + 1;
		g_ds.nPartPos[0] = g_map.rankRow * (m0 + 1);
		g_ds.nPartPos[1] = g_ds.nPartPos[0] + g_ds.nPattern - 1;
	}
	else
	{
		g_ds.nPattern = m0 ;
		g_ds.nPartPos[0] = g_map.rankRow * m0 + i0;
		g_ds.nPartPos[1] = g_ds.nPartPos[0] + g_ds.nPattern - 1;
	}

	for(i=0, j=g_ds.nPartPos[0]; i<g_ds.nPattern; i++, j++)
	{
		g_ds.compressedPattern.v[i] = g_ds.compressedPattern.v[j];
		g_ds.compressedWeight.v[i]  = g_ds.compressedWeight.v[j];
	}
	for(i = g_ds.nPattern; i<g_ds.nTotalPatterns; i++)
	{
		g_ds.compressedPattern.v[i] = -1;
		g_ds.compressedWeight.v[i]  = 0;
	}

	showDataPartition( );
}

void allocLikeData()
{
	unsigned long 	nTreeBlocks,
					iTaxa,
					iTree,
					iNode;
	double			*sitelike;

	g_nodeBlockSize = 4 * g_ds.nPattern;
	g_treeBlockSize = g_nodeBlockSize * g_ds.nTaxa;

	if( g_rs.fEnableMTM )
	{
		nTreeBlocks = g_map.numLocalChains * (g_rs.nMTMTries + 2);
	}
	else
	{
		nTreeBlocks = g_map.numLocalChains * 2;
	}

	g_likedata = (double * )c_malloc( g_likedata_size = sizeof( double  ) * g_treeBlockSize * (nTreeBlocks + 1) );
	g_leafdata = (double **)c_malloc( g_leafdata_size = sizeof( double *) * g_ds.nTaxa );	
	g_intldata = (double **)c_malloc( g_intldata_size = sizeof( double *) * g_ds.nTaxa * nTreeBlocks );
	if( g_likedata && g_leafdata &&  g_leafdata)
	{
		for( iTaxa = 0; iTaxa < g_treeBlockSize * (nTreeBlocks + 1); iTaxa++ )
			g_likedata[iTaxa] = 0.0;
		/*printf("iTaxa=%d\n", iTaxa);*/

		sitelike = g_likedata;
		for(iTaxa=0; iTaxa<g_ds.nTaxa; iTaxa++)
		{
			g_leafdata[iTaxa] = sitelike;
			sitelike += g_nodeBlockSize;
		}

		iNode = 0;
		for(iTree=0; iTree < nTreeBlocks; iTree++)
		{
			for(iTaxa=0; iTaxa<g_ds.nTaxa; iTaxa++)
			{
				g_intldata[ iNode++ ] =  sitelike;
				sitelike += g_nodeBlockSize;
			}
		}
	}
	else
	{
		fatal("not sufficient memory for allocLikeData");
	}

}

void freeLikeData( )
{
	if( g_intldata )
	{
		c_free( g_intldata);
		g_intldata = 0;
	}

	if( g_leafdata )
	{
		c_free(g_leafdata);
		g_leafdata = 0;
	}

	if( g_likedata )
	{
		c_free( g_likedata);
		g_likedata = 0; 
	}
}

void linkLikeDataWithTreeNode(PhyloTree *pTree, int chainIndex, int treeIndex)
{
	unsigned long   iNode, iPattern, base;
	double			**treeLikeData;

	//link the leaf node with site data; all leaf node will share the same data
	for(iNode=0; iNode<g_ds.nTaxa; iNode++)
	{
		pTree->nodes[iNode].siteLike = g_leafdata[iNode];
	}

	if( g_rs.fEnableMTM )
	{
		treeLikeData = g_intldata + (chainIndex * (g_rs.nMTMTries + 2 ) + treeIndex) * g_ds.nTaxa;
	}
	else
	{
		treeLikeData = g_intldata + (chainIndex * 2 + treeIndex) * g_ds.nTaxa;
	}

	for(iNode = g_ds.nTaxa; iNode < pTree->nnodes; iNode++)
	{
		pTree->nodes[iNode].siteLike = treeLikeData[ iNode - g_ds.nTaxa];
	}
}

void resetLikeSiteLink(PhyloTree *pTree, int chainIndex, int treeIndex)
{
	unsigned long   iNode, iPattern, base;
	double			*sitelike;
	double			**treeLikeData;

	if( g_rs.fEnableMTM )
	{
		treeLikeData = g_intldata + (chainIndex * (g_rs.nMTMTries + 2 ) + treeIndex) * g_ds.nTaxa;
	}
	else
	{
		treeLikeData = g_intldata + (chainIndex * 2 + treeIndex) * g_ds.nTaxa;
	}

	for(iNode = g_ds.nTaxa; iNode < pTree->nnodes; iNode++)
	{
		pTree->nodes[iNode].siteLike = treeLikeData[ iNode - g_ds.nTaxa];
	}
}

//NON-MTM Version
//void  initMarkovChain(MarkovChain *pChain, int index)
//{
//	int i, j;
//	PhyloTree *T0, *T;
//
//	pChain->nLocalIndex = index;
//	pChain->nGlobalIndex = g_map.paChainPartition[ index ].globalChainIndex;
//
//	pChain->nGeneration = 0;
//	pChain->pCurrentState = pChain->_States;
//	pChain->pCandidateState = pChain->_States + 1;
//	pChain->nTries = g_rs.nMTMTries;
//	pChain->nAccepts = 0;
//	pChain->nRejects = 0;
//	pChain->nTotalExchanges = 0;
//	pChain->nAcceptedExchanges = 0;
//
//	if( g_rs.fEnablePT )
//	{
//		pChain->fTemperature = getChainTemperature( index, g_rs.nChains, g_rs.nTemperatureSchema);
//	}
//	else
//	{
//		pChain->fTemperature = 1.0;
//		exit( 0 );
//	}
//
//	if(pChain->nGlobalIndex == 0)
//		pChain->fColdChain = TRUE;
//	else
//		pChain->fColdChain = FALSE;
//
//	allocPhyloTree( &(pChain->pCurrentState->tree), g_ds.nTaxa, FALSE, NULL, &(g_ds.label));
//	initPhyloTreeWithRandomPartition( &(pChain->pCurrentState->tree) );
//	//drawPhyloTreeBr(  &(pChain->pCurrentState->tree), TRUE, TRUE );
//
//	allocPhyloTree( &(pChain->pCandidateState->tree), g_ds.nTaxa, FALSE, NULL, &(g_ds.label));
//	copyPhyloTree( &(pChain->pCandidateState->tree), &(pChain->pCurrentState->tree) );
//	//dumpPhyloTree(  &(pChain->pCandidateState->tree) );
//	//drawPhyloTreeBr(  &(pChain->pCandidateState->tree), TRUE, TRUE );
//
//	initModel( &(pChain->pCurrentState->model), g_rs.modelType);
//	copyModel( &(pChain->pCandidateState->model), &(pChain->pCurrentState->model) );
//
//	//debug("pCurrentState: like tree with site data");
//	linkLikeDataWithTreeNode( &(pChain->pCurrentState->tree), index, 0);
//
//	//debug("pCandidaState: like tree with site data");
//	linkLikeDataWithTreeNode( &(pChain->pCandidateState->tree), index, 1);
//	setUpdateflag( &(pChain->pCurrentState->tree) );
//}

//MTM Version
void  initMarkovChain(MarkovChain *pChain, int index)
{
	int indexTry;

	pChain->nLocalIndex = index;
	pChain->nGlobalIndex = g_map.paChainPartition[ index ].globalChainIndex;
	
	if( g_rs.nMTMTries <= 0)
	{
		g_rs.nMTMTries = 0;
		g_rs.fEnableMTM = FALSE;
	}
	pChain->pTryStates = 0;



	pChain->nGeneration = 0;
	pChain->pCurrentState = pChain->_States;
	pChain->pCandidateState = pChain->_States + 1;
	pChain->nTries = g_rs.nMTMTries;
	pChain->nAccepts = 0;
	pChain->nRejects = 0;
	pChain->nTotalExchanges = 0;
	pChain->nAcceptedExchanges = 0;

	if( g_rs.fEnablePT )
	{
		pChain->fTemperature = getChainTemperature( index, g_rs.nChains, g_rs.nTemperatureSchema);
	}
	else
	{
		pChain->fTemperature = 1.0;
		exit( 0 );
	}

	if(pChain->nGlobalIndex == 0)
		pChain->fColdChain = TRUE;
	else
		pChain->fColdChain = FALSE;

	allocPhyloTree( &(pChain->pCurrentState->tree), g_ds.nTaxa, FALSE, NULL, &(g_ds.label));
	initPhyloTreeWithRandomPartition( &(pChain->pCurrentState->tree) );
	initModel( &(pChain->pCurrentState->model), g_rs.modelType);
	linkLikeDataWithTreeNode( &(pChain->pCurrentState->tree), index, 0);
	setUpdateflag( &(pChain->pCurrentState->tree) );
	copyModel( &(pChain->pCandidateState->model), &(pChain->pCurrentState->model) );
	allocPhyloTree( &(pChain->pCandidateState->tree), g_ds.nTaxa, FALSE, NULL, &(g_ds.label));
	copyPhyloTree( &(pChain->pCandidateState->tree), &(pChain->pCurrentState->tree) );
	linkLikeDataWithTreeNode( &(pChain->pCandidateState->tree), index, 1);
}

void  freeMarkovChain(MarkovChain *pChain)
{
	int indexTry;
	if( pChain->pTryStates ) 
	{
		for ( indexTry=0; indexTry<pChain->nTries; indexTry++)
		{
			freePhyloTree( &(pChain->pTryStates[indexTry].tree) );
		}
		free( pChain->pTryStates );
	}
	freePhyloTree( &(pChain->pCurrentState->tree) );
	freePhyloTree( &(pChain->pCandidateState->tree) );
}

void nextMarkovState( MarkovState *pCurrState,  MarkovState *pNextState)
{
	int i, j;

	PhyloTree *T0, *T;
	double *tmpSiteLike;

	//1.	Locate current tree T0 and candidate tree T
	T0 = &(pCurrState->tree);
	T =  &(pNextState->tree);

	//2.	Clear the update flags 
	for( i=T0->ntaxa; i<T0->nnodes; i++)
	{
		T0->nodes[i].nodeflag = NODE_NOCHANG; 
	}

	//3.	Copy T0 to T
	copyPhyloTree( T, T0 );	

	//4.	Make the proposal
	proposeNextTree( T );

	//5.	Reuse the site conditional probability data
	for( i=T->ntaxa; i<T->nnodes; i++)
	{
		if( T->nodes[i].nodeflag != NODE_CHANGED )
		{
			tmpSiteLike = T->nodes[i].siteLike;
			T->nodes[i].siteLike = T0->nodes[i].siteLike;
			T0->nodes[i].siteLike = tmpSiteLike;
		}
	}
}


void	updateMarkovState(MarkovChain *pChain, boolean fAccepted)
{
	PhyloTree *T0, *T;
	int i;
	double *tmpSiteLike;
	MarkovState	*pTmpState;


	//1.	Locate the trees
	T0 = &(pChain->pCurrentState->tree);
	T =  &(pChain->pCandidateState->tree);

	//2.	Update the state
	if( fAccepted )
	{
		//2.1	swap the candidate tree with current tree

		pTmpState = pChain->pCurrentState;
		pChain->pCurrentState = pChain->pCandidateState;
		pChain->pCandidateState = pTmpState;
	}
	else
	{
		//2.2	restore current state to previous state
		for(i=T->ntaxa; i<T->nnodes; i++)
		{
			if(T->nodes[i].nodeflag != NODE_CHANGED)
			{
				tmpSiteLike = T0->nodes[i].siteLike;
				T0->nodes[i].siteLike = T->nodes[i].siteLike;
				T->nodes[i].siteLike = tmpSiteLike;
				T0->nodes[i].nodeflag = NODE_NOCHANG;
				T->nodes[i].nodeflag = NODE_NOCHANG;
			}
		}
	}
}


void	writeTreeFileHeader(FILE *out)
{
	int	i;

	fprintf(out, "#NEXUS\n");
	fprintf(out, "Begin Trees;\n");
	fprintf(out, "    Translate\n");

	for(i=0; i<g_ds.nTaxa-1; i++)
		fprintf(out, "      %8d %s,\n", i+1, g_ds.label.data[i]);

	fprintf(out, "      %8d %s;\n", g_ds.nTaxa, g_ds.label.data[i]);
}

void	writeTreeFileEnding(FILE *out)
{
	int	i;
	fprintf(out, "End;\n");
}

void	writeParamFileHeader(FILE *out)
{
	int	i;

	fprintf(out, "#NEXUS\n");
	fprintf(out, "%8s %10s %10s\n", "gen", "-lnL", "TrLen");
}

void	writeParamFileEnding(FILE *out)
{
	fprintf(out, "\n");
}

void	saveMarkovState(MarkovState *pState, int igen)
{
	sprintf(pState->tree.label, "rep.%d", igen);
	writePhyloTree(g_rs.fpTFile, &(pState->tree), TRUE, TRUE);
	fprintf(g_rs.fpPFile, "%8d %10.5f\n", igen, pState->tree.lnL);

	//writePhyloTree(stdout, &(pState->tree), TRUE, TRUE);
	//fprintf(stdout, "%8d %10.4f\n", igen, pState->tree.lnL);
}

void	saveMarkovState_p( int igen )
{
	//printf("[%d] npGrid=%d numCols=%d numRows=%d coldChainIndex={%d}\n",
	//	g_map.rankCol, g_map.npGrid, g_map.numCols, g_map.numRows, g_coldChainIndex);
	//fflush(stdout);
	if( g_map.npGrid == 1)	// only have one node
	{
		saveMarkovState( g_chains[ g_coldChainIndex ].pCurrentState, igen );
	}
	else if ( g_map.numRows == 1) // only one row
	{
		if( g_map.rankRow == HEAD_NODE )
		{
			saveMarkovState( g_chains[ g_coldChainIndex ].pCurrentState, igen );
		}
	}
	else //multiple chain groups (rows)
	{
		/*	printf("[%d] coldChainIndex={%d}",	g_map.rankCol, g_coldChainIndex);
		fflush(stdout);*/
		if( g_map.rankGrid == HEAD_NODE )					//head of the grid
		{
			if( g_coldChainIndex >= 0 )							//I have the cold chain
			{
				saveMarkovState( g_chains[ g_coldChainIndex ].pCurrentState, igen );
			}
			else												//I don't have the cold chain
			{
				MPI_Request	recv_request[2];
				MPI_Status	status[2];

				MPI_Irecv( &g_treePrintBufferLen, sizeof(TreeLogLandPrintBufferLen),  MPI_CHAR, MPI_ANY_SOURCE, 300, g_map.commCol, &(recv_request[0]));
				MPI_Wait(&recv_request[0], &status[0]);
				MPI_Irecv( g_treePrintBuffer,     g_treePrintBufferLen.len + 1,       MPI_CHAR, MPI_ANY_SOURCE,	400, g_map.commCol, &(recv_request[1]));
				MPI_Wait(&recv_request[1], &status[1]);

				g_treePrintBuffer[g_treePrintBufferLen.len] = '\0';
				//printf("[%d,%d]recv Tree = %s\n", g_map.rankCol, g_map.rankRow, g_treePrintBuffer);
				fprintf(g_rs.fpTFile, "%s", g_treePrintBuffer);
				fprintf(g_rs.fpPFile, "%8d %10.4f\n", igen, g_treePrintBufferLen.lnL);
			}
		}
		else if ( g_map.rankRow == HEAD_NODE )				//head of the row
		{
			if( g_coldChainIndex >= 0 )							//I have the cold chain
			{
				MPI_Request	send_request[2];
				MPI_Status	status[2];

				sprintf(g_chains[ g_coldChainIndex ].pCurrentState->tree.label, "rep.%d", igen);
				g_treePrintBufferLen.len = writePhyloTreeToBuffer( g_treePrintBuffer, &(g_chains[ g_coldChainIndex ].pCurrentState->tree), TRUE, TRUE );
				 g_treePrintBufferLen.lnL = g_chains[ g_coldChainIndex ].pCurrentState->tree.lnL;

				//printf("[%d,%d]send Tree = %s\n", g_map.rankCol, g_map.rankRow, g_treePrintBuffer);

				MPI_Isend( &g_treePrintBufferLen, sizeof(TreeLogLandPrintBufferLen), MPI_CHAR, HEAD_NODE, 300, g_map.commCol, &(send_request[0]));
				MPI_Isend(  g_treePrintBuffer,  g_treePrintBufferLen.len, MPI_CHAR, HEAD_NODE, 400, g_map.commCol, &(send_request[1]));

				MPI_Wait(&send_request[0], &status[0]);
				MPI_Wait(&send_request[1], &status[1]);
			}
			else												//I don't have the cold chain
			{
				;												//do nothing
			}

		}
		else												//other nodes
		{
			;												//do nothing
		}
	}
}

void  printMarkovState_p( FILE *fpOut, int indexGen)
{
	int indexChain;

	if( g_map.rankRow == HEAD_NODE )
	{
		//printf("(%d %d):%d\t", g_map.rankCol, g_map.rankRow, g_map.numLocalChains);

		for (indexChain=0; indexChain<g_map.numLocalChains; indexChain++)
		{
			g_printSendBuffer[ indexChain ].globalIndex = g_map.paChainPartition[ indexChain ].globalChainIndex;
			g_printSendBuffer[ indexChain ].isColdChin  = g_chains[ indexChain ].fColdChain;
			g_printSendBuffer[ indexChain ].lnL = g_chains[ indexChain ].pCurrentState->tree.lnL;
			//printf("[%d] %10.5f", indexChain, g_printSendBuffer[ indexChain ].lnL );
		}
		/*	printf("\n");
		fflush(stdout);*/

		MPI_Gather(	g_printSendBuffer,  sizeof(ChainPrintInfo)*g_map.numLocalChains,  MPI_CHAR,
			g_printRecvBuffer, sizeof(ChainPrintInfo)*g_map.numLocalChains,  MPI_CHAR,
			HEAD_NODE, 
			g_map.commCol );

		if( g_map.rankCol == HEAD_NODE )
		{
			fprintf( fpOut,  "%6d", indexGen );
			for (indexChain=0; indexChain<g_map.numTotalChains; indexChain++)
			{
				fprintf( fpOut, " %12.5f%c", 
					g_printRecvBuffer[ indexChain ].lnL,
					g_printRecvBuffer[ indexChain ].isColdChin?'*':' ');
			}
			fprintf( fpOut, "\n");
			fflush( fpOut );
		}	
	}
}

double	getChainTemperature(int chainIndex, int numChains, int schemaID)
{
	double T;		//temperature
	static double	maximumGap				= 7.00;	//that should depends on the taget distribution
	static double	desiredAcceptanceRate	= 0.2;		
	static double	c0, k;
	int				i;

	desiredAcceptanceRate = g_rs.dDesiredExchangeAcceptance;
	maximumGap			  = g_rs.dDesiredLikelihoodGap;


	switch(schemaID)
	{
	case 1:
		c0 = -maximumGap /log( desiredAcceptanceRate );
		k = c0 / (double) ( numChains - 1);
		T = k * chainIndex;
		break;

	case 2:
		c0 = -maximumGap /log( desiredAcceptanceRate );
		k = c0 / (double) (( numChains - 1)* (numChains - 1) );
		T = k * chainIndex * chainIndex;
		break;

	case 3:
		c0 = -maximumGap /log( desiredAcceptanceRate );
		k = c0 / (double) (( numChains - 1)* (numChains - 1) *(numChains-1) );
		T = k * chainIndex * chainIndex *chainIndex;
		break;

	case 4:
		desiredAcceptanceRate = g_rs.dDesiredExchangeAcceptance;
		maximumGap			  = g_rs.dDesiredLikelihoodGap;
		c0 = log( desiredAcceptanceRate ) / maximumGap;
		T	=	1.0;
		for(i=1; i<=chainIndex; i++)
		{
			T = 1.0 / ( 1.0 / T + c0 );
		}
		T = T - 1.0;
	}
	return 1.0 / (1.0 + T);
}

boolean	openTreeFile(int indexDataset, int indexBootstrap, int indexRun)
{
	char				sTempFileName[256];
	boolean				fOpenFileOK = FALSE;

	if( g_map.rankGrid == HEAD_NODE )
	{
		if( g_rs.nBootstrap <=1 )
		{
			if( g_rs.nRuns <=1 )
				sprintf(sTempFileName, "%s.t", g_rs.sDatasetFile);
			else
				sprintf(sTempFileName, "%s.run%d.t", g_rs.sDatasetFile, indexRun);
		}
		else
		{
			if( g_rs.nRuns <=1 )
				sprintf(sTempFileName, "%s.boot%d.t", g_rs.sDatasetFile, indexBootstrap);
			else
				sprintf(sTempFileName, "%s.boot%d.run%d.t", g_rs.sDatasetFile, indexBootstrap, indexRun);
		}

		g_rs.fpTFile = fopen(sTempFileName, "w+");
		if( g_rs.fpTFile )
		{
			writeTreeFileHeader(g_rs.fpTFile);
			fOpenFileOK	=	TRUE;
		}
		else
		{
			error("can not create tree file--%s", sTempFileName);
			fOpenFileOK	=	FALSE;
		}
	}
	MPI_Bcast( &fOpenFileOK, sizeof(boolean), MPI_CHAR, HEAD_NODE, g_map.commGrid);
	return fOpenFileOK;
}

void closeTreeFile( )
{
	if( g_map.rankGrid == HEAD_NODE )
	{
		writeTreeFileEnding( g_rs.fpTFile );
		fclose( g_rs.fpTFile );
	}
}



boolean	openParamFile(int indexDataset, int indexBootstrap, int indexRun)
{
	char				sTempFileName[256];
	boolean				fOpenFileOK = FALSE;

	if( g_map.rankGrid == HEAD_NODE )
	{
		if( g_rs.nBootstrap <=1 )
		{
			if( g_rs.nRuns <=1 )
				sprintf(sTempFileName, "%s.p", g_rs.sDatasetFile);
			else
				sprintf(sTempFileName, "%s.run%d.p", g_rs.sDatasetFile, indexRun);
		}
		else
		{
			if( g_rs.nRuns <=1 )
				sprintf(sTempFileName, "%s.boot%d.p", g_rs.sDatasetFile, indexBootstrap);
			else
				sprintf(sTempFileName, "%s.boot%d.run%d.p", g_rs.sDatasetFile, indexBootstrap, indexRun) ;
		}

		g_rs.fpPFile = fopen(sTempFileName, "w+");
		if( g_rs.fpTFile )
		{
			writeParamFileHeader( g_rs.fpPFile );
			fOpenFileOK	=	TRUE;
		}
		else
		{
			error("can not create tree file--%s", sTempFileName);
			fOpenFileOK	=	FALSE;
		}
	}
	MPI_Bcast( &fOpenFileOK, sizeof(boolean), MPI_CHAR, HEAD_NODE, g_map.commGrid);
	return fOpenFileOK;
}

void closeParamFile( )
{
	if( g_map.rankGrid == HEAD_NODE )
	{
		writeParamFileEnding( g_rs.fpPFile );
		fclose( g_rs.fpPFile );
	}
}


void chainExchange( void )
{
	double	mu,
		alpha;

	//performance exchange
	//if( g_rs.fEnableExchange )
	//{
	//alpha = RNG2_unif( ) ;
	/*	if( alpha <= g_rs.dExchangeRate )
	{*/
	int		chainA, 
		chainB;

	//select two chains, chainA and chainB are global index
	chainA = RNG2_int2(0, g_map.numTotalChains-1);
	chainB = RNG2_int2(0, g_map.numTotalChains-1);

	//message_p("[%d] exchange chains: chainA = %d chainB = %d\n", g_map.rankMPIWorld, chainA, chainB);

	if (chainA == chainB)	//no exchange needed
	{
		return	;
	}
	else
	{
		int		rowA,
			rowB,
			localIndexA,
			localIndexB;

		rowA = g_map.paChainLocations[chainA].rowIndex;
		rowB = g_map.paChainLocations[chainB].rowIndex;
		localIndexA = g_map.paChainLocations[chainA].localChainIndex;
		localIndexB = g_map.paChainLocations[chainB].localChainIndex;


		if( rowA == rowB )
		{
			if( g_map.rankCol == rowA )
			{
				alpha =		g_chains[ localIndexA ].fTemperature * ( g_chains[ localIndexA ].pCurrentState->tree.lnL - g_chains[ localIndexB ].pCurrentState->tree.lnL )
					+	g_chains[ localIndexB ].fTemperature * ( g_chains[ localIndexB ].pCurrentState->tree.lnL - g_chains[ localIndexA ].pCurrentState->tree.lnL );

				//if(alpha>0)
				//{
				//	alpha = 1.0;
				//}
				//else if(alpha<-100)
				//{
				//	alpha = 0.0;
				//}
				//else
				//{
				//	alpha = exp( alpha );
				//}

				//alpha = min2(alpha, 1.0);
				alpha = min2( exp( alpha ), 10.0 );
				mu = RNG2_unif( );

				g_chains[ localIndexA ].nTotalExchanges++;
				g_chains[ localIndexB ].nTotalExchanges++;
				if ( mu <= alpha )
				{
					int			tempIndex;
					boolean		tempFlag;
					double		tempTemperature;	

					//accept exchange
					g_chains[ localIndexA ].nAcceptedExchanges++;
					g_chains[ localIndexB ].nAcceptedExchanges++;

					tempTemperature = g_chains[ localIndexA ].fTemperature;
					g_chains[ localIndexA ].fTemperature = g_chains[ localIndexB ].fTemperature;
					g_chains[ localIndexB ].fTemperature = tempTemperature;

					tempFlag = g_chains[ localIndexA ].fColdChain;
					g_chains[ localIndexA ].fColdChain = g_chains[ localIndexB ].fColdChain;
					g_chains[ localIndexB ].fColdChain = tempFlag;

					//update coldChainIndex
					if( g_coldChainIndex == localIndexA )
						g_coldChainIndex = localIndexB;
					else if ( g_coldChainIndex == localIndexB )
						g_coldChainIndex = localIndexA;

				}//if ( mu <= alpha )
			}//only the row need to change
			else
			{
				mu = RNG2_unif( );
			}
		} 
		else //if( rowA != rowB )
		{	
			MPI_Status	status[2];
			MPI_Request	request[2];
			////message_p("[%d] exchange chains: chainA = %d (row=%d, index=%d) chainB = %d (row=%d, index=%d)\n", 
			//	g_map.rankMPIWorld,
			//	chainA, rowA, localIndexA,
			//	chainB, rowB, localIndexB);

			if( g_map.rankCol == rowA )
			{
				chainSwapSendBuf.fTemperature = g_chains[ localIndexA ].fTemperature;
				chainSwapSendBuf.isColdChain  = g_chains[ localIndexA ].fColdChain;
				chainSwapSendBuf.lnL		  = g_chains[ localIndexA ].pCurrentState->tree.lnL;

				//printf("[%d:%d] before swap between (%d) and (%d)\n", g_map.rankGrid, g_map.rankCol, rowA, rowB); fflush( stdout );
				//MPI_Send( &chainSwapSendBuf, sizeof(ChainSwapInfo), MPI_CHAR, rowB, 5, g_map.commCol);
				//MPI_Recv( &chainSwapRecvBuf, sizeof(ChainSwapInfo), MPI_CHAR, rowB, 5, g_map.commCol, &status);

				MPI_Isend( &chainSwapSendBuf, sizeof(ChainSwapInfo), MPI_CHAR, rowB, 5, g_map.commCol, &request[0]);
				MPI_Irecv( &chainSwapRecvBuf, sizeof(ChainSwapInfo), MPI_CHAR, rowB, 5, g_map.commCol, &request[1]);
				MPI_Waitall(2,	request, status );


				alpha =		chainSwapSendBuf.fTemperature * ( chainSwapSendBuf.lnL - chainSwapRecvBuf.lnL )
					+	chainSwapRecvBuf.fTemperature * ( chainSwapRecvBuf.lnL - chainSwapSendBuf.lnL );

				//if(alpha>0)
				//{
				//	alpha = 1.0;
				//}
				//else if(alpha<-100)
				//{
				//	alpha = 0.0;
				//}
				//else
				//{
				//	alpha = exp( alpha );
				//}

				//alpha = min2(alpha, 1.0);
				alpha = min2( exp( alpha ), 10.0 );
				mu = RNG2_unif( );

				g_chains[ localIndexA ].nTotalExchanges++;
				if ( mu <= alpha )
				{
					g_chains[ localIndexA ].fTemperature = chainSwapRecvBuf.fTemperature;
					g_chains[ localIndexA ].fColdChain = chainSwapRecvBuf.isColdChain;
					g_chains[ localIndexA ].nAcceptedExchanges++;

					if ( chainSwapRecvBuf.isColdChain )
						g_coldChainIndex = localIndexA;
					else if ( g_coldChainIndex == localIndexA )
						g_coldChainIndex = -1;

				}

			}
			else if( g_map.rankCol == rowB )
			{
				chainSwapSendBuf.fTemperature = g_chains[ localIndexB ].fTemperature;
				chainSwapSendBuf.isColdChain  = g_chains[ localIndexB ].fColdChain;
				chainSwapSendBuf.lnL		  = g_chains[ localIndexB ].pCurrentState->tree.lnL;

				//printf("[%d:%d] before swap between (%d) and (%d)\n", g_map.rankGrid, g_map.rankCol, rowA, rowB); fflush( stdout );

				//MPI_Recv( &chainSwapRecvBuf, sizeof(ChainSwapInfo), MPI_CHAR, rowA, 5, g_map.commCol, &status);
				//MPI_Send( &chainSwapSendBuf, sizeof(ChainSwapInfo), MPI_CHAR, rowA, 5, g_map.commCol);

				MPI_Isend( &chainSwapSendBuf, sizeof(ChainSwapInfo), MPI_CHAR, rowA, 5, g_map.commCol, &request[0]);
				MPI_Irecv( &chainSwapRecvBuf, sizeof(ChainSwapInfo), MPI_CHAR, rowA, 5, g_map.commCol, &request[1]);
				MPI_Waitall(2,	request, status );

				alpha =		chainSwapSendBuf.fTemperature * ( chainSwapSendBuf.lnL - chainSwapRecvBuf.lnL )
					+	chainSwapRecvBuf.fTemperature * ( chainSwapRecvBuf.lnL - chainSwapSendBuf.lnL );

		/*		if(alpha>0)
				{
					alpha = 1.0;
				}
				else if(alpha<-100)
				{
					alpha = 0.0;
				}
				else
				{
					alpha = exp( alpha );
				}*/
				alpha = min2( exp( alpha ), 10.0 );
				//alpha = min2(alpha, 1.0);
				mu = RNG2_unif( );

				g_chains[ localIndexB ].nTotalExchanges++;
				if ( mu <= alpha )
				{
					g_chains[ localIndexB ].fTemperature = chainSwapRecvBuf.fTemperature;
					g_chains[ localIndexB ].fColdChain = chainSwapRecvBuf.isColdChain;
					g_chains[ localIndexB ].nAcceptedExchanges++;

					if ( chainSwapRecvBuf.isColdChain )
						g_coldChainIndex = localIndexB;
					else if ( g_coldChainIndex == localIndexB )
						g_coldChainIndex = -1;
				}
			}
			else
			{
				mu = RNG2_unif( );
			}
		}
	} //if (chainA == chainB)	//no exchange needed
	//} //if( alpha <= g_rs.dExchangeRate )
	//} //if ( g_rs.fEnableExchange )
}


void MarkovChainCore( )
{
	int					ichain,				//index of current Markov Chain
		igen,				//current generation
		itry,
		istate,
		nPrintCounter	=	0,
		nSampleCounter	=	0,
		nExchangeCounter =	0;
	double				detL,				//the log Likelihood change
		alpha,				//the acceptance-rejectionratio
		mu;					//a random number in (0.0, 1.0)
	MarkovChain			*pMyChain;			//reference of current Markov Chain

	//message_p( "MarkovChainCore" );

	for( igen = 1; igen <= g_rs.nMaxChainGenerations; igen++)
	{
		/*MPI_Barrier( g_map.commGrid );*/

		if( !g_rs.fEnableMTM )
		{
			//message("%d, prepare next move", g_map.rankMPIWorld);
			//propose a new state
			for( ichain = 0; ichain < g_map.numLocalChains; ichain++)
			{
				pMyChain = g_chains + ichain;
				pMyChain->nGeneration++;
				nextMarkovState( pMyChain->pCurrentState, g_chains[ichain].pCandidateState );
				ComputeLikelihood( pMyChain->pCandidateState );
				g_paPartLogL[ ichain ] = pMyChain->pCandidateState->tree.lnL;
				//message("%d, check ichain value: %d",g_map.rankMPIWorld, ichain);
			}

			//message("%d, collect global likelihood", g_map.rankMPIWorld);
			if( g_map.numCols > 1 )
				MPI_Allreduce( g_paPartLogL, g_paWholeLogL, g_map.numLocalChains, MPI_DOUBLE, MPI_SUM, g_map.commRow );
			else
			{
				for( ichain = 0; ichain < g_map.numLocalChains; ichain++)
					g_paWholeLogL[ ichain ] = g_paPartLogL[ ichain ];
			}

			/*		message_p("decide acceptance");*/
			//decide accept or not
			for( ichain = 0; ichain < g_map.numLocalChains; ichain++)
			{
				pMyChain = g_chains + ichain;
				pMyChain->pCandidateState->tree.lnL = g_paWholeLogL[ ichain ];
				detL = pMyChain->pCurrentState->tree.lnL - pMyChain->pCandidateState->tree.lnL;

				if (detL >= 0)
					alpha = 1.0;
				else
					alpha = exp( detL * pMyChain->fTemperature );

				mu = unifdev();

				if( alpha >= mu )
				{
					pMyChain->nAccepts++;
					updateMarkovState( pMyChain, TRUE );
				}
				else
				{
					pMyChain->nRejects++;
					updateMarkovState( pMyChain, FALSE );
				}
			}
		}	

		//exchange information between two chains
		if( g_rs.fEnableExchange && ++nExchangeCounter == g_rs.nExchangeInterval )
		{
			/*message_p("chain swap");*/
			chainExchange( );
			nExchangeCounter = 0;
			/*	message_p("chain swap ok");*/
		}

		//save samples
		if( ++nSampleCounter == g_rs.nSampleInterval )
		{
			//message_p("save state");

			//saveMarkovState(  g_chains[ nCurrentColdChain ].pCurrentState, igen);
			saveMarkovState_p( igen );
			nSampleCounter = 0;
			//message_p("save state ok");
		}

		if( ++nPrintCounter == g_rs.nPrintInterval )
		{
			//message_p("print state");
			printMarkovState_p( stdout, igen );
			nPrintCounter = 0;
			//message_p("print state ok");
		}
	}
}


/*
* runMarkovChains
* 
* Parameters:
*		pChain			:	an array of MarckovChain
*		pRunsetting		:	pointer of Runsetting
*		pDataset		:	pointer	of Dataset
*		pMap			:	pointer of ChainProcessMap
* Returns:
*		an booelan, which is represent the status after the run
*/

boolean  runMarkovChains(int indexDataset, int indexBootstrap, int indexRun)
{
	int					ichain;				//index of current Markov Chain
	int					igen;				//current generation

	boolean				fProcessFlag	=	FALSE;

	message_p( "runMarkovChains" );
	//allocate the chains
	g_chains = (MarkovChain *)c_malloc( sizeof(MarkovChain) * g_map.numLocalChains );
	if( g_chains )
	{
		//message_p( "runMarkovChains: begin");
		//partition the dataset
		partitionDataset( );

		//message_p( "runMarkovChains: partition");
		//allocate memory for likelihood data
		allocLikeData();
		buildLeafSiteLikeData( );

		//message_p( "runMarkovChains: allocLikeData");
		g_printSendBuffer = (ChainPrintInfo *)c_malloc( sizeof(ChainPrintInfo) * g_map.numLocalChains );
		g_printRecvBuffer = (ChainPrintInfo *)c_malloc( sizeof(ChainPrintInfo) * g_map.numTotalChains );
		if( g_rs.fEnableMTM )
		{
			g_paPartLogL =  (double *)c_malloc( sizeof(double) * g_map.numLocalChains * g_rs.nMTMTries );
			g_paWholeLogL = (double *)c_malloc( sizeof(double) * g_map.numLocalChains * g_rs.nMTMTries );
		}
		else
		{
			g_paPartLogL =  (double *)c_malloc( sizeof(double) * g_map.numLocalChains );
			g_paWholeLogL = (double *)c_malloc( sizeof(double) * g_map.numLocalChains );
		}
		g_treePrintBuffer = (char *)c_malloc( sizeof(char) * g_ds.nTaxa * 2 * 10 * g_ds.nlen );
		g_nodelist = (int *)c_malloc( sizeof(int) * g_ds.nTaxa * 2);

		//message_p( "runMarkovChains: allocSendRecvBuffer");
		if( g_printSendBuffer && g_printRecvBuffer && g_paPartLogL && g_paWholeLogL 
			&& g_treePrintBuffer && g_nodelist)
		{
			//init chains
			for( ichain = 0; ichain < g_map.numLocalChains; ichain++)
			{
				initMarkovChain( g_chains + ichain, ichain );

				if( g_map.rankCol == 0)
					g_coldChainIndex = 0;
				else
					g_coldChainIndex = -1;

				ComputeLikelihood( g_chains[ ichain ].pCurrentState );
				g_paPartLogL[ ichain ] = g_chains[ ichain ].pCurrentState->tree.lnL;
			}
			MPI_Allreduce( g_paPartLogL, g_paWholeLogL, g_map.numLocalChains, MPI_DOUBLE, MPI_SUM, g_map.commRow );
			for( ichain = 0; ichain < g_map.numLocalChains; ichain++)
			{
				g_chains[ ichain ].pCurrentState->tree.lnL = g_paWholeLogL[ ichain ];
			}
			//message_p( "runMarkovChains: init");


			//open files
			if( openTreeFile( indexDataset, indexBootstrap, indexRun ) )
			{
				if( openParamFile( indexDataset, indexBootstrap, indexRun ) )
				{	
					saveMarkovState_p( 0 );
					printMarkovState_p( stdout, 0 );

					MarkovChainCore();

					//close files
					closeTreeFile( );
					closeParamFile( );
				}
				else
				{
					error("open param file failed");
					fProcessFlag = FALSE;
				}
			}
			else
			{
				error("open tree file failed");
				fProcessFlag = FALSE;
			}

			free( g_treePrintBuffer );
			free( g_printSendBuffer );
			free( g_printRecvBuffer );
			free( g_paPartLogL );
			free( g_paWholeLogL);
			free( g_nodelist );
		}
		else
		{
			error("alloacte g_globalLogLData and g_localLogLData failed");
			fProcessFlag = FALSE;
		}

		//release resources used by chains
		for( ichain = 0; ichain < g_map.numLocalChains; ichain++)
		{
			freeMarkovChain( g_chains + ichain );
		}

		//free memory for likelihood data
		freeLikeData( );
		fProcessFlag = TRUE;

	}
	else
	{
		error("can not allocate g_chains in runMarkovChains: g_map.numLocalChains=%d", g_map.numLocalChains);
		fProcessFlag = FALSE;
	}
	return fProcessFlag;

}

