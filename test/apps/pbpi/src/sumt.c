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
#include "dataset.h"
#include "token.h"
#include "tree.h"
#include "util.h"
#include "pbpi.h"
#include "parallel.h"
#include "error.h"
#include "sumt.h"
#include "runsetting.h"

CountedTreeList				g_treeSamples;
extern	RunSetting			g_rs;
extern	ChainProcessMap		g_map;	
extern	Dataset				g_ds;

IMatrix						g_bipartionsTrueTree;
IMatrix						g_bipartionsSampTree;

void initBipartitions(PhyloTree *tree, IMatrix *bipratitions)
{
	int i, j, ntaxa, nnode;

	ntaxa = tree->ntaxa;
	nnode = tree->nnodes;  

	allocIMatrix( bipratitions, nnode + 1, ntaxa + 1 );
	for( i=0; i< bipratitions->nrows; i++)
	{
		bipratitions->data[i][0] = 0;
	}
	bipratitions->flag = ntaxa;
}

void freeBipartitions()
{
	freeIMatrix( &g_bipartionsTrueTree );
	freeIMatrix( &g_bipartionsSampTree );
}


int *listNodeBipartions( BinaryTreeNode *node,  IMatrix * bipartions)
{
	int		i, j, ntaxa;
	
	ntaxa  = bipartions->flag;

	if ( node->nodetype == LEAF_NODE )
	{
		int		*p, *pa, *pb, index;
		index  = node->index;
	
		p = bipartions->data[index];
		p[0] = 1;

		pa  = p + 1;
		pa[0] = index;

		pb = pa + 1;
		for(i=0, j=0; i<ntaxa; i++)
		{
			if( i!= index )
				pb[j++] = i;
		}
		return p;
	}
	else
	{
		int	index, *pc, *pa, *pb, ic, ia, ib, na, nb, nc, *p;


		index = node->index;
		
		pa = listNodeBipartions( node->left, bipartions);
		na = pa[0];
		pa++;

		pb = listNodeBipartions( node->right, bipartions);
		nb = pb[0];
		pb++;	
		
		//pc  = bipartions->data[index];
		//nc = na + nb;
		//pc[0] = nc;
		//pc++;

		////merge two partitions
		//for(ia=0, ib=0, ic=0; ia<na & ib<nb; ic++)
		//{
		//	if( pa[ia] < pb[ib] )
		//		pc[ic] = pa[ia++];
		//	else
		//		pc[ic] = pb[ib++];
		//}
		//while( ia < na )
		//	pc[ic++] = pa[ia++];
		//while( ib < nb )
		//	pc[ic++] = pb[ib++];

		////get the complements

		//p = bipartions->data[ bipartions->nrows - 1 ];
		//p++;
		//for(ia=0; ia<ntaxa; ia++) p[ ia ] = 1;
		//for(ic=0; ic<nc; ic++)    p[ pc[ic] ] = 0;
		//for(ic=nc, ia=0; ia<ntaxa; ia++)
		//{
		//	if( p[ ia ] )
		//		pc[ ic++ ] = ia;
		//}
		p = bipartions->data[ bipartions->nrows - 1 ];
		p++;
		for(ic=0; ic<ntaxa; ic++) p[ ic ] = 0;
		for(ia=0; ia<na; ia++)
		{
			p[ pa[ia] ] = 1;
		}
		for(ib=0; ib<nb; ib++)
		{
			p[ pb[ib] ] = 1;
		}

		pc	= bipartions->data[index];
		p[0]= na + nb;
		pa  = pc + 1;
		pb  = pa + pc[0];
		ia = 0;
		ib = 0;

		for(ia=0, ib=0, ic=0; ic<ntaxa; ic++)
		{
			if( p[ ic ] )
				pc[ ia++ ] = ic;
			else
				pc[ ib++ ] = ic;
		}
		return pc;
	}
}

void getBipartions(PhyloTree *tree, IMatrix *bipratitions)
{
	BinaryTreeNode *root = tree->root;
	listNodeBipartions( root, bipratitions );
	if( root->parent )
		listNodeBipartions( root->parent, bipratitions );
}

//return: the number of bipartions in <secondBipratitions> which is not found in
//			<firstBipratitions>
int matchBipartions(IMatrix *firstBipratitions, IMatrix *secondBipratitions)
{
	int  index1, index2, ntaxa, nnode, ndist = 0, match, *p1, *p2, *pa, *pb, ia, ib;

	ntaxa = firstBipratitions->flag;
	nnode = firstBipratitions->nrows - 1;
	for( index2 = ntaxa; index2 < nnode; index2++)
	{
		p2 = secondBipratitions->data[ index2 ];
		if( p2[0] > 0 )
		{
			match = 0;
			for( index1 = ntaxa; index1 < nnode; index1++)
			{
				p1 = firstBipratitions->data[ index1 ];
				if( p1[0] > 0)
				{
					if( p1[0] == p2[0] )
					{
						pa = p1 + 1;
						pb = p2 + 1;
						for( ia = 0; ia < p1[0]; ia++ )
						{
							if( pa[ia] != pb[ia] )
								break;
						}
						if( ia == p1[0] )
						{
							match = 1;
							break;
						}
					}
					if( p1[0] == ntaxa - p2[0] )
					{
						pa = p1 + 1;
						pb = p2 + 1 + p2[0];
						for( ia = 0; ia < p1[0]; ia++ )
						{
							if( pa[ia] != pb[ia] )
								break;
						}
						if( ia == p1[0] )
						{
							match = 1;
							break;
						}
					}
				}
			}
			if( !match )
			{
				ndist++;
			}
		}
	}
	return ndist;
}



//
//
//void listNodeBipartions( IMatrix * bipartions, BinaryTreeNode *node)
//{
//	int	i, 
//		index = node->index,
//		ntaxa = bipartions->flag;
//	if( node->nodetype == LEAF_NODE )
//	{
//		for( i=0; i<index; i++)
//			bipartions->data[ index ][ i ] = i;
//		bipartions->data[ index ][ index ] = bipartions->data[ index ][ 0 ];
//		bipartions->data[ index ][ 0 ];    = index;
//		bipartions->data[ node->index ][ntaxa]  = 1;
//	}
//	else
//	{
//		listNodeBipartions( bipartions, node->left );
//		listNodeBipartions( bipartions, node->right);
//		mergeBipartitions( bipartions, node->left->index, node->right->index, node->index);
//	}
//}
//
//void listBipartions( IMatrix * bipartions, PhyloTree *tree)
//{
//	listNodeBipartions( tree->root );
//	listNodeBipartions( tree->root->parent );
//}


void  initCountedTreeList( CountedTreeList *pList)
{
	pList->head		=	0;
	pList->tail		=	0;
	pList->numNodes	=	0;
	pList->numTrees	=	0;
}

void freeTreeList( TreeListNode *pList)
{
	TreeListNode	*curr, *next;
	curr = pList;
	while( curr )
	{
		next = curr->next;
		if( curr->tree )
			freePhyloTree( curr->tree );
		free( curr );
		curr = next;
	}
}

void  freeCountedTreeListNode( CountedTreeListNode *pNode)
{
	if( pNode )
	{
		if( pNode->treeList )
			freeTreeList( pNode->treeList );
		free(pNode);
	}
}

void  freeCountedTreeList( CountedTreeList *pList)
{
	CountedTreeListNode	*currNode, *nextNode;

	currNode = pList->head;
	while( currNode )
	{
		nextNode = currNode->next;
		freeCountedTreeListNode( currNode );
		currNode = nextNode;
	}
	pList->head		=	0;
	pList->tail		=	0;
	pList->numNodes	=	0;
	pList->numTrees	=	0;

}

CountedTreeListNode	*	newCountedTreeListNode( )
{
	CountedTreeListNode	*pNode;

	pNode = (CountedTreeListNode *)malloc( sizeof(CountedTreeListNode) );
	if( pNode )
	{
		pNode->count	=	0;
		pNode->next		=	0;
		pNode->prev		=	0;
		pNode->treeList	=	0;
	}
	else
	{
		error("run out of memory in newCountedTreeListNode");
	}

	return pNode;
}

CountedTreeListNode	*	findPhyloTree( CountedTreeListNode *pHeadNode, PhyloTree *pTree )
{
	TreeListNode			*pTreeList;
	CountedTreeListNode		*pCurrNode;

	if ( pHeadNode )
	{
		pCurrNode	=	pHeadNode;
		while ( pCurrNode )
		{
			pTreeList	=	pCurrNode->treeList;
			if( pTreeList )
			{
				if ( calcPhyloTreeDistance( pTreeList->tree, pTree ) == 0 )
					return pCurrNode;
			}
			pCurrNode = pCurrNode->next;
		}
	}
	return 0;
}

void addTeeeToTreeListAllTrees( TreeListNode ** pList, PhyloTree *pTree )
{
	TreeListNode	*	pNode;

	pNode = (TreeListNode *) malloc( sizeof(TreeListNode) );
	if( pNode )
	{
		pNode->tree = pTree;
		pNode->next = *pList;
		*pList		= pNode;
	}
	else
	{
		error("can not allocate memory");
	}
}

void addTeeeToTreeList( TreeListNode ** pList, PhyloTree *pTree )
{
	TreeListNode	*	pNode;

	if( *pList ) //tree already there
	{
		free( pTree );
	}
	else
	{
		pNode = (TreeListNode *) malloc( sizeof(TreeListNode) );
		if( pNode )
		{
			pNode->tree = pTree;
			pNode->next = *pList;
			*pList		= pNode;
		}
		else
		{
			error("can not allocate memory");
		}
	}
}


void dumpCountedTreeList( CountedTreeList *pList )
{
	int					numCredibleSample;
	int					numCredibieTrees;
	CountedTreeListNode *pNode = pList->head;
	

	numCredibleSample	=	(int)(g_rs.dCredibleSetPercentage * pList->numTrees);
	numCredibieTrees	=	0;
	printf("ntrees=%d nsamples=%d\n", pList->numNodes, pList->numTrees);
	while ( pNode  && (numCredibleSample=numCredibleSample-pNode->count) > 0 )
	{
		numCredibieTrees++;
		printf(" %d\n", pNode->count);
		pNode = pNode->next;
	}
	printf("\n");

}

void sortCountedTreeList( CountedTreeList *pList, CountedTreeListNode *pTail)
{
	CountedTreeListNode	*curr, *prev;
	
	curr = pTail;

	if( curr )
	{
		prev = curr->prev;
		while( prev )
		{
			if( curr->count > prev->count )
			{
				int	tempCount;
				TreeListNode	*tempTreeList;

				tempCount = prev->count;
				prev->count = curr->count;
				curr->count = tempCount;

				tempTreeList   = prev->treeList;
				prev->treeList = curr->treeList;
				curr->treeList = tempTreeList;
			}

			curr = prev;
			prev = curr->prev;
		}
	}
}

void addTreeToCountedTreeList( CountedTreeList *pList, PhyloTree *pTree )
{
	CountedTreeListNode		*pMatchNode, *pPrevNode;

	if ( pList->head )
	{
		pMatchNode = findPhyloTree( pList->head, pTree );
		if ( pMatchNode )
		{
			addTeeeToTreeList( &(pMatchNode->treeList), pTree );
			pMatchNode->count++;
		}
		else
		{
			pMatchNode = newCountedTreeListNode();

			pList->tail->next = pMatchNode;
			pMatchNode->prev = pList->tail;
			pList->tail	= pMatchNode;

			addTeeeToTreeList( &(pMatchNode->treeList), pTree );

			pMatchNode->count++;
			pList->numNodes++;
		}
	}
	else
	{
		pMatchNode = newCountedTreeListNode();
		pList->head = pMatchNode;
		pList->tail = pMatchNode;

		addTeeeToTreeList( &(pMatchNode->treeList), pTree );

		pMatchNode->count++;
		pList->numNodes++;
	}
	pList->numTrees++;
	
	sortCountedTreeList( pList, pMatchNode );
}

void  initCountedCladeList( CountedCladeList *pList, int numSignatureUnit, int numTaxa,	int numUniqueTrees, int numTotalTrees )
{
	//pList->numNodes	=	0;
	//pList->numClades=	0;
	pList->numSignatureUnit = numSignatureUnit;
	pList->numTaxa	=	numTaxa;

	pList->percentageThreshold	= g_rs.dMajorityRatio;
	pList->_MaxNumClades	=	numUniqueTrees * numTaxa;	//could be smaller since two tree will have same clades
	pList->_numClades		=	0;
	pList->_numTrees		=	numTotalTrees;
	pList->_pClades		= (unsigned *)malloc( sizeof(unsigned) * pList->_MaxNumClades	* (numSignatureUnit + 1) );
	if( pList->_pClades )
	{
		int			index;
		unsigned	*pClade;
		pClade = pList->_pClades;
		for(index=0; index<pList->_MaxNumClades	* (numSignatureUnit + 1); index++)
			*pClade++	=	0;
	}
	else
	{
		error("run out of memory");
	}
}

void freeCountedCladeList( CountedCladeList *pList)
{
	pList->_MaxNumClades = 0;
	pList->_numClades	 = 0;
	pList->_numTrees	 = 0;
	if ( pList->_pClades )
		free ( pList->_pClades );
	pList->numSignatureUnit = 0;
	pList->numTaxa = 0;
	pList->percentageThreshold = 0;
}


//CountedCladeListNode	*	newCountedCladeListNode( int numSignatureUnit )
//{
//	CountedCladeListNode	*pNode;
//
//	pNode = (CountedCladeListNode *)malloc( sizeof(CountedCladeListNode) );
//	if( pNode )
//	{
//		pNode->count	=	0;
//		pNode->next		=	0;
//		pNode->prev		=	0;
//		pNode->signature=	(unsigned *)malloc( sizeof(unsigned) );
//		if( !(pNode->signature) )
//		{
//			error("run out of memory in newCountedCladeListNode--signature");
//		}
//	}
//	else
//	{
//		error("run out of memory in newCountedCladeListNode");
//	}
//
//	return pNode;
//}
//
//CountedCladeListNode	*	findCladeSignature( CountedCladeList *pList, unsigned *signature )
//{
//	CountedCladeListNode	* currNode	=	pList->head;
//	int						numUnit		=	pList->numSignatureUnit;
//	int						numTaxa		=	pList->numTaxa;
//
//	while( currNode )
//	{
//		if( matchSignature( currNode->signature, signature, numUnit, numTaxa) )
//		{
//			return currNode;
//		}
//
//		currNode = currNode->next;
//	}
//	return 0;
//}

//void copySignature( unsigned *destSignature, unsigned *srcSignature, int numUnit)
//{
//	int		indexUnit;
//	for(indexUnit=0; indexUnit<numUnit; numUnit++)
//	{
//		destSignature[indexUnit] = srcSignature[indexUnit];
//	}
//
//}

//void sortCountedCladeList( CountedCladeList *pList, CountedCladeListNode *pTail)
//{
//	CountedCladeListNode	*curr, *prev;
//	
//	curr = pTail;
//
//	if( curr )
//	{
//		prev = curr->prev;
//		while( prev )
//		{
//			if( curr->count > prev->count )
//			{
//				int	tempCount;
//				int	indexUnit;
//				unsigned uNumber;
//		
//				tempCount = prev->count;
//				prev->count = curr->count;
//				curr->count = tempCount;
//
//				for(indexUnit = 0; indexUnit<pList->numSignatureUnit; indexUnit++)
//				{
//					uNumber = prev->signature[indexUnit];
//					prev->signature[indexUnit] = curr->signature[indexUnit];
//					curr->signature[indexUnit] = uNumber;
//				}
//			}
//			curr = prev;
//			prev = curr->prev;
//		}
//	}
//}

//void addCladeToCountedCladeListOld( CountedCladeList *pList, unsigned *signature, int count )
//{
//	CountedCladeListNode	* pMatchNode;
//
//	if( pList )
//	{
//		if( pList->head )
//		{
//			pMatchNode	=	findCladeSignature( pList, signature );
//			if( pMatchNode )
//			{
//				//debug("add an existing clade");
//				pMatchNode->count = pMatchNode->count + count;
//			}
//			else
//			{
//				//debug("add a new clade");
//				pMatchNode = newCountedCladeListNode( pList->numSignatureUnit );
//				pList->tail->next = pMatchNode;
//				pMatchNode->prev  = pList->tail;
//				pList->tail		  = pMatchNode;
//
//				copySignature( pMatchNode->signature, signature, pList->numSignatureUnit );
//
//				pMatchNode->count = pMatchNode->count + count;
//				pList->numNodes++;
//			}
//		}
//		else
//		{
//			//debug("add the first clade");
//			pMatchNode = newCountedCladeListNode( pList->numSignatureUnit );
//			pList->head = pMatchNode;
//			pList->tail = pMatchNode;;
//
//			copySignature( pMatchNode->signature, signature, pList->numSignatureUnit );
//
//			pMatchNode->count = pMatchNode->count + count;
//			pList->numNodes++;
//		}
//		pList->numClades = pList->numClades + count;
//
//		//sortCountedCladeList( pList, pMatchNode );
//	}
//	else
//	{
//		bug("unexpected NULL pointer in addCladeToCountedCladeList");
//	}
//}


extern void reverseSignature(unsigned *signature,	int numUnit, int numUsefulBit);

boolean	matchSignature(unsigned *signatureA, unsigned *signatureB, int numUnit, int numUsefulBit)
{
	int indexUnit;
	boolean				fMatch;
	static	unsigned	uSign[ MAX_NUM_SIGNANATUR_UNIT ];

	//save  signatureB
	for(indexUnit=0; indexUnit<numUnit; indexUnit++)
	{
		uSign[ indexUnit ] = signatureB[ indexUnit ];
	}

	fMatch	=	TRUE;
	for(indexUnit = 0; indexUnit<numUnit; indexUnit++)
	{		
		if( signatureA[ indexUnit] != uSign[ indexUnit ] )
		{
			fMatch = FALSE;
			break;
		}
	}

	if( !fMatch )
	{
		reverseSignature(uSign, numUnit, numUsefulBit );
		fMatch = TRUE;
		for(indexUnit = 0; indexUnit<numUnit; indexUnit++)
		{		
			if( signatureA[ indexUnit] != uSign[ indexUnit ] )
			{
				fMatch = FALSE;
				break;
			}
		}
	}

	return fMatch;
}

void addCladeToCountedCladeList( CountedCladeList *pList, unsigned *signature, int count )
{
	int				indexClade,
					numUnit,
					numUsefulBit;

	unsigned	*	pClade;	
	boolean			fMatch = FALSE;

	pClade	=	pList->_pClades;
	numUnit	=	pList->numSignatureUnit;
	numUsefulBit = pList->numTaxa;

	for(indexClade=0; indexClade<pList->_numClades; indexClade++)
	{
	
		int		indexUnit;

		fMatch	=	matchSignature( signature, pClade, numUnit, numUsefulBit);
		
		if( fMatch )
		{
			//add an existing clade
			pClade[numUnit] += count;
			break;
		}

		pClade	=	pClade + (numUnit + 1);
	}

	if( !fMatch )
	{
		//add a new clade
		int		indexUnit;
		for(indexUnit=0; indexUnit<numUnit; indexUnit++)
		{
			pClade[indexUnit] = signature[indexUnit];
			pClade[numUnit]	  = count;
		}
		pList->_numClades++;
	}
}

void	writeSignature(FILE *out, unsigned *uNumbers, int numUnit, int numTaxa)
{
	int indexUnit, indexBit;

	
	for(indexUnit=0; indexUnit<numUnit; indexUnit++)
	{
		unsigned u = uNumbers[indexUnit];
		for(indexBit=0; indexBit< sizeof(unsigned) * 8; indexBit++)
		{
			if( u & 0x1 )
				fprintf(out, "*" );
			else
				fprintf(out, "-" );
			u = u >> 1;
			numTaxa--;
			if( !numTaxa )
				return;
		}
	}
}

void buildCountedCladeList( CountedCladeList *pCladeList, CountedTreeList *pTreeList)
{
	CountedTreeListNode		*currTreeListNode = pTreeList->head;
	PhyloTree				*pTree;
	
	while( currTreeListNode )
	{
		pTree = currTreeListNode->treeList->tree;
		if( pTree )
		{
			int	indexClade;
			//for( indexClade=pTree->ntaxa; indexClade<pTree->nnodes; indexClade++)
			for( indexClade=0; indexClade<pTree->nnodes; indexClade++)
			{
				addCladeToCountedCladeList( pCladeList, pTree->nodes[indexClade].signature, currTreeListNode->count );
			}
		}
		currTreeListNode = currTreeListNode->next;
	}
}

//void	dumpCountedCladeListOld( CountedCladeList *pList )
//{
//	CountedCladeListNode	* currCladeListhNode = pList->head;
//	printf("nnode = %d	nclade = %d  nsigunit=%d\n", pList->numNodes, pList->numClades, pList->numSignatureUnit );
//	while( currCladeListhNode )
//	{
//		writeSignature( stdout, currCladeListhNode->signature, pList->numSignatureUnit, pList->numTaxa) ;
//		printf("    %d\n",	currCladeListhNode->count);
//		currCladeListhNode = currCladeListhNode->next;
//	}
//}

void	dumpCountedCladeList( CountedCladeList *pList )
{
	unsigned	*	pClade;
	int				indexClade,
					numUnit,	
					numTaxa,
					numThreshold;

	printf("nclade = %d	ntrees = %d  nsigunit=%d\n", pList->_numClades, pList->_numTrees, pList->numSignatureUnit );
	pClade	=	pList->_pClades;
	numUnit	=	pList->numSignatureUnit;
	numTaxa	=	pList->numTaxa;
	numThreshold	=	(int)(pList->percentageThreshold * pList->_numTrees);
	for(indexClade=0; indexClade<pList->_numClades; indexClade++)
	{
		if( pClade[numUnit] > numThreshold )
		{
			writeSignature( stdout, pClade, numUnit, numTaxa);
			printf("    %d\n", pClade[numUnit] );
		}
		pClade = pClade + ( numUnit + 1 );
	}
}

boolean readTranslate(FILE *in, Dataset *pDataset)
{
	int				index;
	char			szLabel[MAX_LABEL_LENGTH + 1],			//taxa label
					szTranslate[MAX_LABEL_LENGTH + 1];		//translate of the taxa label
	NEXUSToken	*	pNEXUSToken;							//pointer to next nexus token
	boolean			fTranslateOK	=		TRUE;

	for(index=0; index<pDataset->nTaxa; index++)
	{
		pNEXUSToken = nextNEXUSToken( in );
		if( pNEXUSToken->type == TOK_STRING )
		{
			tokenSafeCopy( szTranslate, MAX_LABEL_LENGTH, pNEXUSToken->token );

			pNEXUSToken = nextNEXUSToken( in );
			if( pNEXUSToken->type == TOK_STRING )
			{
				tokenSafeCopy( szLabel, MAX_LABEL_LENGTH, pNEXUSToken->token );

				fTranslateOK = addTranslate(pDataset, szTranslate, szLabel);

				pNEXUSToken = nextNEXUSToken( in );
				if( pNEXUSToken->type == TOK_COMMA )
				{
					continue;
				}
				else if( pNEXUSToken->type == TOK_SEMICOLON )
				{
					break;
				}
				else
				{
					fTranslateOK = FALSE;
					error("unexpected seperator (%s)", pNEXUSToken->token);
				}
			}
			else
			{
				fTranslateOK = FALSE;
				error("taxa label string is expected");
			}
		}
		else
		{
			fTranslateOK = FALSE;
			error("translate string is expected");
		}
	}

	if( fTranslateOK && pNEXUSToken->type != TOK_SEMICOLON )
	{
		fTranslateOK = FALSE;
		error("semicolon is expected to end the translate command");
	}

	return fTranslateOK;
}

boolean processTreeSample(CountedTreeList *pList, PhyloTree	*pTree)
{
	normalizePhyloTree( pTree );
	updatePhyloTreeSignature( pTree );
	addTreeToCountedTreeList( pList, pTree );
	return TRUE;
}

int	calcNumSignatureUnit( int nTaxa )
{
	int nUnit;
	nUnit = nTaxa / ( sizeof(unsigned) * 8);
	if( nTaxa % ( sizeof(unsigned) * 8) )
		nUnit++;

	return nUnit;
}

boolean summaryTreeSamples(Dataset *pDataset, FILE *in)
{
	boolean				fDataOK			= TRUE,	
						fUseTranslate	= FALSE;
	NEXUSToken		*	pNEXUSToken;

	CountedTreeList		TreeList;
	CountedCladeList	CladeList;

	int					nTreeCount;

	initCountedTreeList(  &TreeList );

	pNEXUSToken = nextNEXUSToken( in );
	if( tokenMatch( pNEXUSToken->token, "#nexus", TRUE ) )
	{
		if ( searchNEXUSToken( in, "begin" ) )
		{
			pNEXUSToken = nextNEXUSToken( in );
			if( tokenMatch( pNEXUSToken->token, "trees", TRUE ) )
			{
				pNEXUSToken = nextNEXUSToken( in );
				if ( pNEXUSToken->type == TOK_SEMICOLON)
				{
					//read the translate
					pNEXUSToken = nextNEXUSToken( in );
					if( tokenMatch( pNEXUSToken->token, "translate", TRUE ) )
					{
						fUseTranslate = TRUE;
						fDataOK = readTranslate(in, pDataset);
						pNEXUSToken = nextNEXUSToken( in );
					}

					nTreeCount = 0;
					//read each tree and process
					while( fDataOK && tokenMatch( pNEXUSToken->token, "tree", TRUE ) )
					{
						pNEXUSToken = nextNEXUSToken( in );
						if( pNEXUSToken->type == TOK_STRING )
						{
							char			szTreeLabel[MAX_TREE_LABEL_LEN + 1];

							tokenSafeCopy( szTreeLabel, MAX_TREE_LABEL_LEN, pNEXUSToken->token );
							if( searchNEXUSToken(in, "=") )
							{
								PhyloTree	*	pTree;
							
								pTree = (PhyloTree *)malloc( sizeof( PhyloTree )  );
								if ( pTree )
								{
									allocPhyloTree( pTree, pDataset->nTaxa, TRUE,  szTreeLabel, &(pDataset->label) );

									if( readPhyloTree(in, pTree, fUseTranslate ) )
									{
										nTreeCount = nTreeCount + 1;
										//debug( "nTreeCount=%d g_rs.nSumTreeBurnIn=%d", nTreeCount, g_rs.nSumTreeBurnIn);
										if( nTreeCount <= g_rs.nSumTreeBurnIn )
											free( pTree );
										else
											processTreeSample( &TreeList, pTree );
										
										//debug( "Get next Tree", nTreeCount, g_rs.nSumTreeBurnIn);
										pNEXUSToken = nextNEXUSToken( in );
										//printf("next token = %s\n", pNEXUSToken->token);
									}
									else
									{
										error("read tree error");
										fDataOK = FALSE;
									}
								}
								else
								{
									error("allocate tree error");
									fDataOK = FALSE;
								}
							}
							else
							{
								error("= is expected");
								fDataOK = FALSE;
							}
						}
						else
						{
							error("tree label is expected");
							fDataOK = FALSE;
						}
					}

					if( tokenMatch( pNEXUSToken->token, "end", TRUE ) )
					{
						searchNEXUSToken( in, ";" );
					}
					else
					{
						error("keyword End is expected");
						fDataOK = FALSE;
					}
				}
				else
				{
					error("\';\' is expected");
					fDataOK = FALSE;
				}
			}
			else
			{
				error("Tree blocks expected");
				fDataOK = FALSE;
			}
		}
		else
		{
			error("Can not find tree block");
			fDataOK = FALSE;
		}
	}
	else
	{
		error("Tree file is not NEXUS format");
		fDataOK = FALSE;
	}

	//debug("read in all tree");

	if( fDataOK )
	{
		TrueTreeComparsionResult	results;

		//dumpCountedTreeList(  &TreeList );
		initCountedCladeList( &CladeList, calcNumSignatureUnit( pDataset->nTaxa ),  pDataset->nTaxa, TreeList.numNodes, TreeList.numTrees );
		buildCountedCladeList( &CladeList, &TreeList );
		//dumpCountedCladeList( &CladeList );
		if( g_rs.fKnowTrueTree )
		{
			compareTrueTreeWithSamples( &(g_rs.TrueTree), &TreeList, &CladeList, &results);
		//printf("MPP-TrueTree:	%d	%d\n", results.nDistanceToMPP, results.fEqualMPP);
		//printf("CTS-TrueTree:	%d	%d\n", results.nDistanceToCTS, results.fEqualCTS);
		//printf("CTS-TrueTree:	%d	%d\n", results.nDistanceToCON, results.fEqualCON);
		}
		outputSummary(&TreeList, &CladeList, &(g_rs.TrueTree), &results);
		
	}
	freeCountedTreeList( &TreeList );
	freeCountedCladeList( &CladeList );

	return fDataOK;
}

boolean	extractTrueTreeFromDataset(char *szDatasetFile, Dataset *pDataset, PhyloTree *pTrueTree )
{
	FILE				*fpData;
	boolean				fDataOk			=	TRUE;
	NEXUSToken		*	pNEXUSToken;

	fpData	=	fopen( szDatasetFile, "r" );
	if( fpData )
	{
		pNEXUSToken = nextNEXUSToken( fpData );
		if( tokenMatch( pNEXUSToken->token, "#nexus", TRUE ) )
		{
			pNEXUSToken = _nextToken( fpData );
			pNEXUSToken = _nextToken( fpData );
			if ( TOK_COMMENT_START == pNEXUSToken->type)
			{
				if ( searchNEXUSToken( fpData, "tree" ) )
				{
					pNEXUSToken = nextNEXUSToken( fpData );
					if( pNEXUSToken->type == TOK_STRING )
					{
						char			szTreeLabel[MAX_TREE_LABEL_LEN + 1];
						tokenSafeCopy( szTreeLabel, MAX_TREE_LABEL_LEN, pNEXUSToken->token );

						if( searchNEXUSToken(fpData, "=") )
						{
							allocPhyloTree( pTrueTree, pDataset->nTaxa, TRUE,  szTreeLabel, &(pDataset->label) );
							if( readPhyloTree(fpData, pTrueTree, FALSE ) )
							{
								fDataOk = TRUE;
								//debug("true tree read success");
							}
							else
							{
								fDataOk	=	FALSE;
								error("read true tree failed");
							}
						}
						else
						{
							fDataOk	=	FALSE;
							error("token = is expected");
						}
					}
					else
					{
						fDataOk	=	FALSE;
						error("tree lable is expected");
					}
				}
				else
				{
					fDataOk	=	FALSE;
					error("keyword tree is expected");
				}
			}
			else
			{
				fDataOk	=	FALSE;
				error(" token [ is expected");
				
			}
		}
		else
		{
			fDataOk	=	FALSE;
			error("keyword tree is expected");
		}
		fclose(fpData);
	}
	else
	{
		fDataOk	=	FALSE;
		error("open dataset file error");
	}

	return fDataOk;
}

boolean	readTrueTreeFromTreeFile(char *szTreeFile, Dataset *pDataset, PhyloTree *pTrueTree )
{
	FILE				*fpData;
	boolean				fDataOk			=	TRUE;
	NEXUSToken		*	pNEXUSToken;

	fpData	=	fopen( szTreeFile, "r" );
	if( fpData )
	{	
		char			szTreeLabel[MAX_TREE_LABEL_LEN + 1];
		tokenSafeCopy( szTreeLabel, MAX_TREE_LABEL_LEN, szTreeFile);

		//debug("open tree file succeed");
		if ( allocPhyloTree( pTrueTree, pDataset->nTaxa, TRUE,  szTreeLabel, &(pDataset->label) ) )
		{
			//debug("allocate tree succeed");
			if( readPhyloTree(fpData, pTrueTree, FALSE ) )
			{
	/*			debug("True Tree was read in");
				dumpPhyloTree( pTrueTree );*/
				fDataOk = TRUE;
			}
			else
			{
				debug("failed to read in True Tree");
				fDataOk = FALSE;
			}
		}else
		{
			error("allocate tree failed in readTrueTreeFromTreeFile" );
			fDataOk = FALSE;
		}
	}
	else
	{
		fDataOk	=	FALSE;
		error("open tree file error: %s", szTreeFile);
	}

	return fDataOk;
}

boolean	findSignatureInPhyloTree( PhyloTree *pTree, unsigned *signature)
{
	int	indexTreeNode,
		indexUnit;
	boolean fMatch;
	

	for( indexTreeNode = 0; indexTreeNode < pTree->nnodes; indexTreeNode++)
	{
		unsigned * sign0;
		sign0 = pTree->nodes[ indexTreeNode].signature;
		fMatch	=	TRUE;
		for(indexUnit = 0; indexUnit<pTree->nSignatureUnit; indexUnit++)
		{		
			if( sign0[ indexUnit] != signature[ indexUnit ] )
				fMatch = FALSE;
		}
		if( fMatch )
			break;
	}

	return fMatch;

}

boolean	findSinguatureInCladeList( CountedCladeList *pCladeList, unsigned *signature)
{
		boolean	fMatch	=	FALSE;
		int		indexUnit,
				numUnit,
				numUsefulBit,
				numThreshold,
				indexClade;
		unsigned	*pClade;

		numUnit			=	pCladeList->numSignatureUnit;
		numUsefulBit	=	pCladeList->numTaxa;
		numThreshold	=	(int)(pCladeList->percentageThreshold * pCladeList->_numTrees);

		pClade = pCladeList->_pClades;
		for(indexClade=0; indexClade<pCladeList->_numClades; indexClade++)
		{
			if( pClade[numUnit] >= numThreshold )
			{
				fMatch = matchSignature( signature, pClade, numUnit, numUsefulBit);

				//writeSignature( stdout, signature, numUnit, numUsefulBit);
				//printf(" compare with clade #%d ", indexClade );
				//writeSignature( stdout, pClade, numUnit, numUsefulBit);	
				//printf(" %s\n", fMatch?"match":"not match" );

				if( fMatch )
					break;	
			}

			pClade = pClade + (numUnit + 1);
		}
		return fMatch;
}

void compareTrueTreeWithSamples( PhyloTree *pTrueTree, CountedTreeList	*pTreeList, CountedCladeList *pCladeList, TrueTreeComparsionResult *pResults)
{
	PhyloTree	*pMPPTree,	
				*pCTSTree;
	int			indexTreeNode,
				indexUnit,
				indexClade,
				nDistance,
				numUnit,
				numThreshold;
				
	unsigned	*pClade;
	double		percentage;
	
	CountedTreeListNode	*currNode;

	if( pTreeList->numNodes <= 0 )
	{
		pResults->nDistanceToMPP = -1;
		pResults->nDistanceToCTS = -1;
		pResults->nDistanceToCON = -1;
		pResults->fEqualMPP		 = FALSE;
		pResults->fEqualCTS		 = FALSE;
		pResults->fEqualCON		 = FALSE;
		return;
	}
	
	normalizePhyloTree( pTrueTree );
	updatePhyloTreeSignature( pTrueTree );

	//MPP Tree
	pMPPTree	=	pTreeList->head->treeList->tree;
	//pTreeList->head->distance = calcPhyloTreeDistance( pTrueTree, pMPPTree );
	pTreeList->head->distance = calcPhyloTreeDistanceFN(pTrueTree, pMPPTree, FALSE );

	pResults->nDistanceToMPP = pTreeList->head->distance;
	pResults->fEqualMPP = (!pResults->nDistanceToMPP) ? TRUE : FALSE;

	//CTS Trees
	pResults->nDistanceToCTS = pResults->nDistanceToMPP;
	currNode = pTreeList->head;
	percentage = 0.0;
	while( currNode && percentage <= g_rs.dCredibleSetPercentage )
	{
		pCTSTree	=	currNode->treeList->tree;
		//nDistance	=	calcPhyloTreeDistance( pTrueTree, pCTSTree );
		nDistance	= calcPhyloTreeDistanceFN(pTrueTree, pCTSTree, FALSE );
		currNode->distance = nDistance;
		if( nDistance < pResults->nDistanceToCTS  )
			pResults->nDistanceToCTS  = nDistance;
		percentage += (double)(currNode->count) / (double)(pTreeList->numTrees);
		currNode	=	currNode->next;
	}
	pResults->fEqualCTS = (!pResults->nDistanceToCTS) ? TRUE : FALSE;
	//debug("compare true tree: 300");

	//CON Tree
	pClade			=	pCladeList->_pClades;
	numUnit			=	pCladeList->numSignatureUnit;
	numThreshold	=	pCladeList->_numTrees * pCladeList->percentageThreshold;

	nDistance = 0;
	for( indexTreeNode=0; indexTreeNode<pTrueTree->nnodes; indexTreeNode++)
	{
		if ( ! (findSinguatureInCladeList( pCladeList, pTrueTree->nodes[ indexTreeNode ].signature )) )
			nDistance++;
	}
	pResults->nDistanceToCON = nDistance;
	pResults->fEqualCON = (!pResults->nDistanceToCON) ? TRUE : FALSE;
}

void outputSummary(CountedTreeList *pTreeList, CountedCladeList *pCladeList, PhyloTree *pTrueTree, TrueTreeComparsionResult *pResults)
{
	char	szFileName[ MAX_FILE_NAME_LEN + 1];
	char	*szLogFile = "sumt.log";

	FILE *fpSummary, *fplog;
	sprintf(szFileName, "%s.tsum", g_rs.sOutputPrefix);
	fpSummary = fopen( szFileName, "w+");

	if ( fpSummary )
	{
		double	individualPercentage;
		double	totalPercentage;
		CountedTreeListNode	*currNode;
		int			numTrees;
		int			numClade;
		int			indexClade;
		unsigned	*pClade;


		if( g_rs.fKnowTrueTree )
		{
			//debug("output true tree");
			fprintf(fpSummary, "[MPP-TT: %d %d\tCTS-TT: %d %d\tCON-TT: %d %d]\n", 
				pResults->fEqualMPP,  pResults->nDistanceToMPP, 
				pResults->fEqualCTS,  pResults->nDistanceToCTS, 
				pResults->fEqualCON,  pResults->nDistanceToCON
				);

			fplog = fopen( szLogFile, "a" );
			fprintf(fplog, "%s\t%d\t%d\t%d\t%d\t%d\t%d\n",
				g_rs.sDatasetFile,
				pResults->fEqualMPP,  
				pResults->fEqualCTS, 
				pResults->fEqualCON, 
				pResults->nDistanceToMPP, 
				pResults->nDistanceToCTS, 
				pResults->nDistanceToCON);
			fflush(fplog);
			fclose(fplog);

			fprintf(fpSummary, "[True Tree\n");
			//debug("write true tree to file");
			writePhyloTree(fpSummary, pTrueTree, TRUE, FALSE);
			//debug("draw true tree to file");
			drawPhyloTreeBr( pTrueTree, FALSE, FALSE, fpSummary);
			fprintf(fpSummary, "]\n\n\n");
		}

		//debug("output mpp trees");
		totalPercentage	=	0.0;
		currNode = pTreeList->head;
		numTrees = 0;
		while( currNode && totalPercentage < g_rs.dCredibleSetPercentage )
		{
			numTrees++;
			//debug("write tree %d of %d tree (%d)", numTrees,  pTreeList->numNodes, pTreeList->numTrees);
			individualPercentage =	(double)(currNode->count) / (double)(pTreeList->numTrees);
			totalPercentage += individualPercentage;
			if( g_rs.fKnowTrueTree )
			{
				fprintf(fpSummary, "[Tree #%d%c p=%.3f P=%.3f distance=%d]", 
					numTrees, currNode->distance?' ':'*', individualPercentage, totalPercentage, 
					currNode->distance);
				printf("%5d %10.6f %10.6f %5d\n", numTrees, individualPercentage, totalPercentage, currNode->distance);

			}
			else
			{
				fprintf(fpSummary, "[Tree #%d  p=%.3f P=%.3f]", 
					numTrees,individualPercentage, totalPercentage);
			}
			//debug("write tree %d of %d tree (%d):1002", numTrees,  pTreeList->numNodes, pTreeList->numTrees);
			//normalizePhyloTree( currNode->treeList->tree );
			writePhyloTree(fpSummary, currNode->treeList->tree, TRUE, FALSE);
			fprintf(fpSummary, "[\n");
			//debug("write tree %d of %d tree (%d):1003", numTrees,  pTreeList->numNodes, pTreeList->numTrees);
			drawPhyloTreeBr( currNode->treeList->tree, FALSE, FALSE, fpSummary);
			//drawPhyloTree( currNode->treeList->tree, FALSE);
			//debug("write tree %d of %d tree (%d):1004", numTrees,  pTreeList->numNodes, pTreeList->numTrees);
			fprintf(fpSummary, "]\n\n");
			currNode=currNode->next;
		}

		//debug("output clades");
		fprintf(fpSummary, "\n[Partition List\n");
		pClade = pCladeList->_pClades;
		numClade = 0;
		for(indexClade=0; indexClade<pCladeList->_numClades; indexClade++)
		{
			numClade++;
			individualPercentage = (double)pClade[pCladeList->numSignatureUnit]/(double)(pCladeList->_numTrees);
			fprintf(fpSummary, "    %5d %10.3f\t", numClade, individualPercentage);
			writeSignature( fpSummary, pClade, pCladeList->numSignatureUnit, pCladeList->numTaxa);
			fprintf(fpSummary, "\n");
			pClade  = pClade + (pCladeList->numSignatureUnit + 1);
		}
		fprintf(fpSummary, "]\n");

		fclose( fpSummary );
	}
	else
	{
		error("can not create file (%s)", szFileName );
	}
}


void setupOutputPrefix(int indexDataset, int indexBootstrap, int indexRun)
{
	if( g_rs.nBootstrap <=1 )
	{
		if( g_rs.nRuns <=1 )
			sprintf(g_rs.sOutputPrefix, "%s", g_rs.sDatasetFile);
		else
			sprintf(g_rs.sOutputPrefix, "%s.run%d", g_rs.sDatasetFile, indexRun);
	}
	else
	{
		if( g_rs.nRuns <=1 )
			sprintf(g_rs.sOutputPrefix, "%s.boot%d", g_rs.sDatasetFile, indexBootstrap);
		else
			sprintf(g_rs.sOutputPrefix, "%s.boot%d.run%d", g_rs.sDatasetFile, indexBootstrap, indexRun);
	}
	//printf("Output Prefix = %s\n", g_rs.sOutputPrefix);
}

boolean	openTreeSamplesFile( )
{
	char				sTempFileName[256];
	boolean				fOpenFileOK = FALSE;

	if( g_map.rankGrid == HEAD_NODE )
	{
		sprintf(sTempFileName, "%s.t", g_rs.sOutputPrefix );

		g_rs.fpTFile = fopen(sTempFileName, "r");
		if( g_rs.fpTFile )
		{
			writeTreeFileHeader(g_rs.fpTFile);
			fOpenFileOK	=	TRUE;
		}
		else
		{
			error("can not open tree file--%s", sTempFileName);
			fOpenFileOK	=	FALSE;
		}
	}
	MPI_Bcast( &fOpenFileOK, sizeof(boolean), MPI_CHAR, HEAD_NODE, g_map.commGrid);
	return fOpenFileOK;
}


boolean summaryTree_p(int indexDataset, int indexBootstrap, int indexRun )
{
	boolean	fProcessOk	= FALSE;
	
	if( g_map.rankGrid == HEAD_NODE )
	{
		setupOutputPrefix( indexDataset, indexBootstrap, indexRun );
		if ( g_rs.fKnowTrueTree )
		{
			if( g_rs.fEmbeddedTrueTree )
			{
				extractTrueTreeFromDataset( g_rs.sDatasetFile, &g_ds, &(g_rs.TrueTree) );
			}
			else
			{
				readTrueTreeFromTreeFile( g_rs.sTrueTreeFile, &g_ds, &(g_rs.TrueTree) );
			}
		}

		if ( openTreeSamplesFile( indexDataset, indexBootstrap, indexRun ) )
		{
			fProcessOk = summaryTreeSamples( &g_ds, g_rs.fpTFile );
			fclose( g_rs.fpTFile );
		}
		else
		{
			message("can not open file");
			fProcessOk = FALSE;
		}
	}

	MPI_Bcast(&fProcessOk, sizeof(boolean), MPI_CHAR, HEAD_NODE, g_map.commGrid );

	return fProcessOk;
}

