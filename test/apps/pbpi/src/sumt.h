#ifndef _PBPI_SUMT_H
#define _PBPI_SUMT_H

#include <stdio.h>
#include <stdlib.h>
#include "dataset.h"
#include "token.h"
#include "tree.h"
#include "util.h"
#include "pbpi.h"
#include "error.h"


typedef struct	TreeListNode
{
	struct TreeListNode		*next;
	PhyloTree				*tree;
}TreeListNode;

typedef struct CountedTreeListNode
{
	struct	CountedTreeListNode	*next;
	struct	CountedTreeListNode	*prev;
	TreeListNode				*treeList;
	int							count;
	int							distance;
}CountedTreeListNode;

typedef	struct CountedTreeList
{
	CountedTreeListNode		*head;
	CountedTreeListNode		*tail;
	int						numTrees;
	int						numNodes;
}CountedTreeList;

//typedef struct CountedCladeListNode
//{
//	struct CountedCladeListNode	*next;
//	struct CountedCladeListNode	*prev;
//	unsigned					*signature;
//	int							count;
//}CountedCladeListNode;


typedef	struct CountedCladeList
{
	//CountedCladeListNode		*head;
	//CountedCladeListNode		*tail;

	unsigned			*		_pClades;
	int							_MaxNumClades;
	int							_numClades;
	int							_numTrees;

	double						percentageThreshold;				

	//int							numClades;
	//int							numNodes;
	int							numSignatureUnit;
	int							numTaxa;
}CountedCladeList;

void	writeSignature(FILE *out, unsigned *uNumbers, int numUnit, int numTaxa);
boolean summaryTreeSamples(Dataset *pDataset, FILE *fpSampleFile);
boolean	extractTrueTreeFromDataset(char *szDatasetFile, Dataset *pDataset, PhyloTree *pTrueTree );
void compareTrueTreeWithSamples( PhyloTree *pTrueTree, CountedTreeList	*pTreeList, CountedCladeList *pCladeList, TrueTreeComparsionResult *pResults);
void outputSummary(CountedTreeList *pTreeList, CountedCladeList *pCladeList, PhyloTree *pTrueTree, TrueTreeComparsionResult *pResults);
boolean summaryTree_p(int indexDataset, int indexBootstrap, int indexRun );

#endif
