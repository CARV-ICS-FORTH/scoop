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
#include "pbpi.h"
#include "token.h"
#include "error.h"
#include "util.h"
#include "tree.h"
#include "dataset.h"

boolean	allocPhyloTree(PhyloTree *pTree, int nTaxa, boolean	fRooted, char *szLabel, CMatrix *pTaxaLabels)
{
	int i;
	int	j, k, nUnit;
	BinaryTreeNode	*pNode;

	pTree->flag = TREE_UNALLOCATED;
	pTree->ntaxa = nTaxa;
	pTree->lnL = 0.0;
	pTree->rooted = fRooted;
	pTree->nnodes = nTaxa + nTaxa - 1;	//We first treat the tree as rooted

	nUnit = pTree->ntaxa / ( sizeof(unsigned) * 8);
	if( pTree->ntaxa % ( sizeof(unsigned) * 8) )
		nUnit++;
	pTree->nSignatureUnit = nUnit;

	pTree->nodes = (BinaryTreeNode *)malloc( sizeof(BinaryTreeNode) * pTree->nnodes );
	pTree->nodes[0].signature = (unsigned *)malloc( sizeof( unsigned ) * nUnit * pTree->nnodes );
	if( pTree->nodes && pTree->nodes[0].signature )
	{
		for(i=0; i<pTree->nnodes; i++)
		{
			pNode = pTree->nodes + i;	//get a pointer to the i-th node
			pNode->nodeflag = NODE_UNINITAILZED;
			if( i < pTree->ntaxa )	//default, only handle binary tree
				pNode->nodetype = LEAF_NODE;
			else
				pNode->nodetype = INTERNAL_NODE;

			pNode->index  = i;
			pNode->brlen = 0.0;
			pNode->depth = 0;
			pNode->height = 0;
			pNode->left = 0;
			pNode->right = 0;
			pNode->parent = 0;
			pNode->sibling = 0;
			pNode->signature = pTree->nodes[0].signature + nUnit * i;
			for(j=0; j<nUnit; j++)
				pNode->signature[j] = 0;
		}

		for(i=0; i<pTree->ntaxa; i++)
		{
			pNode = pTree->nodes + i;
			j = i / (sizeof(unsigned) * 8);
			k = i % (sizeof(unsigned) * 8);
			pNode->signature[j] = 0x1 << k;
		}

		//update # of TreeNode
		pTree->internodes = pTree->nodes + pTree->ntaxa;
		if ( !pTree->rooted )
			pTree->nnodes = pTree->nnodes -1;

		//set a root node
		pTree->root = pTree->nodes + pTree->nnodes - 1;
		pTree->root->nodetype = ROOT_NODE;

		//save the current Tree Label
		if( szLabel )
			tokenSafeCopy( pTree->label, MAX_TREE_LABEL_LEN, szLabel );
		else
			tokenSafeCopy( pTree->label, MAX_TREE_LABEL_LEN, "mytree" );

		pTree->flag = TREE_UNINITAILZED;
		pTree->pTaxaLabels = pTaxaLabels;

		//debug("allocated %s Tree: nTaxa=%d\t%s", 
		//	pTree->rooted?"Rooted":"Unrooted",
		//nTaxa, szLabel?szLabel:"NULL");
		return TRUE;
	}
	else
	{
		error("can not allocate mmeory in allocPhyloTree");
		return FALSE;
	}

	
}

void	freePhyloTree(PhyloTree *pTree)
{
	int i;
	assert( pTree );
	if( pTree->nodes )
	{
		if( pTree->nodes[0].signature )
			free ( pTree->nodes[0].signature );
		free( pTree->nodes );
	}
	
	pTree->flag = TREE_UNALLOCATED;
}

extern double *			g_likedata;			//global site likelihood
extern Dataset			g_ds;
void	dumpBinaryTreeNode( BinaryTreeNode *pNode )
{
	int i;
	printf("node[%2d]%c%c", pNode->index, 
		(pNode->nodeflag == NODE_CHANGED)?'*':' ',
		(pNode->nodetype == LEAF_NODE)?'o':
		(pNode->nodetype == ROOT_NODE?'+':'x') 
		);

	if(pNode->left) 	printf(" %4d", pNode->left->index);
	else				printf(" %4s", " ");

	if(pNode->right) 	printf(" %4d", pNode->right->index);
	else				printf(" %4s", " ");

	if(pNode->parent) 	printf(" %4d", pNode->parent->index);
	else				printf(" %4s", " ");

	//printf("  %.4f	%d  (%d, %-d)",		pNode->brlen, (pNode->siteLike - g_likedata)/(g_ds.nPattern * 4), pNode->width, pNode->height);
	printf("  %.4f	(%d, %-d)",		pNode->brlen, pNode->width, pNode->height);
	printf("\n");
	fflush( stdout );
}

void	dumpPhyloTree(PhyloTree *pTree)
{
	int i;
	
	printf("Tree Label = %s nTaxa = %d Root = %d -lnL = %.4f\n", 
		pTree->label, pTree->ntaxa, pTree->root->index, pTree->lnL);
	
	for (i=0; i<pTree->nnodes; i++)
		dumpBinaryTreeNode( pTree->nodes + i);

	printf("\n");
}

void	printBits(unsigned uNumber)
{
	int i;
	unsigned u = uNumber;
	for(i=0; i< sizeof(uNumber) * 8; i++)
	{
		if( u & 0x1 )
			fputc( '1', stdout );
		else
			fputc( '0', stdout );
		u = u >> 1;
	}
}

void	updateTreeNodeSignature( BinaryTreeNode *pNode, int nSigUnit )
{
	int i;
	/*debug("update signature at node: %d type=%d left=%d right = %d parent=%d",
		pNode->index, 
		pNode->nodetype, 
		(pNode->left)?pNode->left->index:-1,
		(pNode->right)?pNode->right->index:-1, 
		(pNode->parent)?pNode->parent->index:-1);*/

	if( pNode->nodetype == LEAF_NODE )
	{
		int	indexUnit,
			indexBit;

		for(i=0; i<nSigUnit; i++)
		{
			pNode->signature[i] = 0;
		}
	
		indexUnit	= pNode->index / (sizeof(unsigned) * 8);
		indexBit	= pNode->index % (sizeof(unsigned) * 8);

		//debug("index=%d nSigUnit=%d indexUNit=%d indexBit=%d", pNode->index, nSigUnit, indexUnit, indexBit);
		pNode->signature[indexUnit] = 0x1 << indexBit;
		//debug("node %d is updated\n", pNode->index);
	}
	else if( pNode->nodetype == INTERNAL_NODE )
	{
		for(i=0; i<nSigUnit; i++)
		{
			pNode->signature[i] = 0;
		}
		if( pNode->left )
		{
			updateTreeNodeSignature( pNode->left, nSigUnit );
			for(i=0; i<nSigUnit; i++)
			{
				pNode->signature[i] |= pNode->left->signature[i]; 
			}
		}
		if( pNode->right )
		{
			updateTreeNodeSignature( pNode->right, nSigUnit );
			for(i=0; i<nSigUnit; i++)
			{
				pNode->signature[i] |= pNode->right->signature[i]; 
			}
		}
	}
	else if (pNode->nodetype == ROOT_NODE )
	{
		for(i=0; i<nSigUnit; i++)
		{
			pNode->signature[i] = 0;
		}
		if( pNode->left )
		{
			updateTreeNodeSignature( pNode->left, nSigUnit );

		}
		if( pNode->right )
		{
			updateTreeNodeSignature( pNode->right, nSigUnit );
			for(i=0; i<nSigUnit; i++)
			{
				pNode->signature[i] |= pNode->right->signature[i]; 
			}
		}
		////debug("update signature at node: %d type=%d left=%d right = %d parent=%d",
		//pNode->index, 
		//pNode->nodetype, 
		//(pNode->left)?pNode->left->index:-1,
		//(pNode->right)?pNode->right->index:-1, 
		//(pNode->parent)?pNode->parent->index:-1);

		if( pNode->parent )
		{
			updateTreeNodeSignature( pNode->parent, nSigUnit );
			for(i=0; i<nSigUnit; i++)
			{
				pNode->signature[i] |= pNode->parent->signature[i]; 
			}
		}
	}
	//printf("\n");

}
void	updatePhyloTreeSignature( PhyloTree *pTree )
{
	updateTreeNodeSignature(pTree->root, pTree->nSignatureUnit);
	/*updateTreeNodeSignature(pTree->root->parent, pTree->nSignatureUnit);*/
}

void	dumpPhyloTreeSignature(FILE *out, PhyloTree *pTree)
{
	int i, j, k, counter;
	unsigned u;

	for(i=0; i<pTree->nnodes; i++)
	{
		counter = 0;
		for(j=0; j<pTree->nSignatureUnit; j++)
		{
			u = pTree->nodes[i].signature[j];
			for(k=0; k<sizeof(unsigned)*8;  k++)
			{
				if( u & 0x1 )
					fputc('*', out);
				else
					fputc('-', out);
				u = u >> 1;
				counter++;
				if(counter>=pTree->ntaxa)
					break;
			}
		}
		fprintf(out, "\n");
	}
}


void reverseSignature(unsigned *signature,	int numUnit, int numUsefulBit)
{
	int	indexUnit,
		numFullUnit,
		numBitInHighUnit;
	unsigned mask;

	numFullUnit = numUsefulBit / (sizeof( unsigned ) * 8 );
	for(indexUnit=0; indexUnit<numFullUnit; indexUnit++)
	{
		signature[indexUnit] = ~signature[indexUnit];
	}
	numBitInHighUnit = numUsefulBit - numFullUnit * sizeof( unsigned ) * 8;
	
	mask = 0x01 << numBitInHighUnit;
	mask = mask - 1;
	//printf(" useful:%d max=%0o", numUsefulBit, mask);
	signature[ numFullUnit ] = ~signature[ numFullUnit ] & mask;
}

int	findNodeSignature( PhyloTree *pTree, unsigned *signature)
{
	int					indexTreeNode,
						indexUnit,
						numUnit;
	boolean				fMatch;
	static	unsigned	uSign[ MAX_NUM_SIGNANATUR_UNIT ];
	static	unsigned	mask;
	
	numUnit = pTree->nSignatureUnit;
	for(indexUnit=0; indexUnit<numUnit; indexUnit++)
	{
		uSign[ indexUnit ] = signature[ indexUnit ];
	}

	for( indexTreeNode = pTree->ntaxa; indexTreeNode < pTree->nnodes; indexTreeNode++)
	{
		unsigned * sign0;
		sign0 = pTree->nodes[ indexTreeNode].signature;

		fMatch	=	TRUE;
		for(indexUnit = 0; indexUnit<numUnit; indexUnit++)
		{		
			if( sign0[ indexUnit] != uSign[ indexUnit ] )
			{
				fMatch = FALSE;
				break;
			}
		}

		if( !fMatch )
		{
			//writeSignature(stdout, uSign, numUnit, pTree->ntaxa);
			//printf("\t");
			reverseSignature(uSign, numUnit, pTree->ntaxa );
			//writeSignature(stdout, uSign, numUnit, pTree->ntaxa);
			//printf("\n");

			fMatch = TRUE;
			for(indexUnit = 0; indexUnit<numUnit; indexUnit++)
			{		
				if( sign0[ indexUnit] != uSign[ indexUnit ] )
				{
					fMatch = FALSE;
					break;
				}
			}
		}
		//writeSignature(stdout, sign0, numUnit, pTree->ntaxa);
		//printf("  %s   ", fMatch?"==":"!=");
		//writeSignature(stdout, sign0, numUnit, pTree->ntaxa);
		//printf("\n");

		if ( fMatch )
			break;
	}
	return fMatch;
}

int	calcPhyloTreeDistance(PhyloTree *pTree0, PhyloTree *pTree1)
{
	int indexTreeNode,
		treeDistance;
	
	updatePhyloTreeSignature(pTree0);
	updatePhyloTreeSignature(pTree1);

	treeDistance = 0;
	for( indexTreeNode=pTree0->ntaxa; indexTreeNode<pTree0->nnodes; indexTreeNode++)
	{
		if ( ! (findNodeSignature( pTree0, pTree1->nodes[ indexTreeNode ].signature )) )
			treeDistance++;
	}

	for( indexTreeNode=pTree1->ntaxa; indexTreeNode<pTree1->nnodes; indexTreeNode++)
	{
		if ( ! (findNodeSignature( pTree1, pTree0->nodes[ indexTreeNode ].signature )) )
			treeDistance++;
	}

	return treeDistance;
}

//false negative
int	calcPhyloTreeDistanceFN(PhyloTree *trueTree, PhyloTree *compTree, boolean fUpdateSignature)
{
	int indexTreeNode,
		treeDistance;
	
	if( fUpdateSignature )
	{
		updatePhyloTreeSignature(compTree);
		updatePhyloTreeSignature(trueTree);
	}

	treeDistance = 0;
	for( indexTreeNode=trueTree->ntaxa; indexTreeNode<trueTree->nnodes; indexTreeNode++)
	{
		if ( ! (findNodeSignature( compTree, trueTree->nodes[ indexTreeNode ].signature )) )
			treeDistance++;
	}
	return treeDistance;
}


void setLeftChild( BinaryTreeNode *pNode, BinaryTreeNode *pLeft)
{
	assert( pNode );
	pNode->left = pLeft;
	if( pLeft )
	{
		pLeft->parent = pNode;
	}
}

void setRightChild( BinaryTreeNode *pNode, BinaryTreeNode *pRight)
{
	assert( pNode );
	pNode->right = pRight;
	if( pRight)
	{
		pRight->parent = pNode;
	}
}

//This is for the root node in an unrooted tree which
void setParentChild( BinaryTreeNode *pNode, BinaryTreeNode *pParent)
{
	assert( pNode );
	pNode->parent = pParent;
	if( pParent)
	{
		pParent->parent = pNode;
	}
}

BinaryTreeNode *readBinaryTreeNode(FILE *in, PhyloTree *pTree, int *nCurrentLeaf, int *nCurrentInternal, boolean fUseTranslate)
{
	NEXUSToken			*	pToken;
	BinaryTreeNode		*	pLeft				=	NULL,
						*	pRight				=	NULL, 
						*	pNode				=	NULL;
	boolean					fReadNodeOk			=	TRUE;
	int						taxaIndex;

	pToken = nextNEXUSToken( in );
	if ( TOK_LEFT_PARENTHESIS == pToken->type )
	{
		pLeft =	readBinaryTreeNode( in, pTree, nCurrentLeaf, nCurrentInternal, fUseTranslate);
		if( pLeft )
		{
			pToken = nextNEXUSToken( in );
			if ( TOK_COMMA == pToken->type )
			{
				pRight =	readBinaryTreeNode( in, pTree, nCurrentLeaf, nCurrentInternal, fUseTranslate);
				if( pRight )
				{
					
					pToken = nextNEXUSToken( in );
					if ( TOK_RIGHT_PARENTHESIS == pToken->type )
					{
						pNode = pTree->internodes + *nCurrentInternal;
						
						pNode->index	= *nCurrentInternal + g_ds.nTaxa;
						pNode->nodetype = INTERNAL_NODE;
						pNode->nodeflag	= NODE_CHANGED;
						pNode->siteLike = 0;
						pNode->width    = 0;
						pNode->height	= 0;

						setLeftChild( pNode, pLeft );
						setRightChild( pNode, pRight );

						*nCurrentInternal = (*nCurrentInternal) + 1;
					}
					else
					{
						fReadNodeOk		=	FALSE;
						error( "token \')\' is expected");
					}
				}
				else
				{
					fReadNodeOk		=	FALSE;
					error("read right child failed");
				}
			}
			else
			{
				fReadNodeOk		=	FALSE;
				error("comma is expected");
			}
		}
		else
		{
			fReadNodeOk		=	FALSE;
			error("read left Child failed");
		}
	}
	else if ( TOK_STRING == pToken->type )
	{
		if ( fUseTranslate )
		{
			taxaIndex = searchTranslate( &g_ds, pToken->token );
		}
		else
		{
			taxaIndex = searchTaxaLabel( &g_ds, pToken->token );
		}

		if( taxaIndex >=0 && taxaIndex < g_ds.nTaxa )
		{	
			pNode = pTree->nodes + taxaIndex;

			pNode->index	= taxaIndex;
			pNode->nodetype = LEAF_NODE;
			pNode->nodeflag = NODE_CHANGED;
			pNode->siteLike = 0;
			pNode->width    = 0;
			pNode->height	= 0;

			*nCurrentLeaf	= *nCurrentLeaf + 1;
		}
		else
		{
			fReadNodeOk		=	FALSE;
			error("unexpected node (%s)", pToken->token );
		}
	}
	else
	{
		fReadNodeOk		=	FALSE;
		error("unexpected token (%s)", pToken->token );
	}

	if ( fReadNodeOk )
	{
		pToken = peekNEXUSToken( in );
		if ( TOK_COLON == pToken->type )
		{
			pToken = nextNEXUSToken( in );
			pToken = nextNEXUSToken( in );
			pNode->brlen = atof( pToken->token );
		}
	}

	if ( fReadNodeOk )
		return pNode;
	else
		return NULL;
}

boolean	readPhyloTree(FILE *in, PhyloTree *pTree, boolean fUseTranslate)
{
	BinaryTreeNode	*	pLeft		=	0, 
					*	pRight		=	0, 
					*	pParent		=	0, 
					*	pRoot		=	0;

	NEXUSToken		*	pToken		=	0;

	int					nLeaf			=	0, 
						nInternal		=	0;

	boolean				fReadTreeOK		=	TRUE;

	//set the root node
	pTree->rooted = TRUE;

	pToken = nextNEXUSToken( in );
	if ( TOK_LEFT_PARENTHESIS == pToken->type )
	{
		//debug("read left node of the root");
		pLeft = readBinaryTreeNode( in, pTree, &nLeaf, &nInternal, fUseTranslate );
		if( pLeft )
		{
			pToken = nextNEXUSToken( in );
			if( TOK_COMMA == pToken->type )
			{
				//debug("read right node of the root");
				pRight = readBinaryTreeNode( in, pTree, &nLeaf, &nInternal, fUseTranslate );
				if( pRight )
				{
					pToken = nextNEXUSToken( in );
					if ( TOK_COMMA == pToken->type )
					{
						//debug("read parent node of the root");
						pParent = readBinaryTreeNode( in, pTree, &nLeaf, &nInternal, fUseTranslate );
						if ( pParent )
						{
							pTree->rooted = FALSE;
							pToken = nextNEXUSToken( in );
						}
						else
						{
							fReadTreeOK		=	FALSE;
							error("read parent child of the root failed");
						}
					}
					if ( TOK_RIGHT_PARENTHESIS == pToken->type )
					{
						pToken = nextNEXUSToken( in );
						if ( TOK_SEMICOLON == pToken->type || TOK_COMMENT_STOP == pToken->type )
						{ 	
							pRoot = pTree->internodes + nInternal;
							pTree->nnodes = nLeaf + nInternal + 1;
							pTree->root = pRoot;
	
							pRoot->nodetype = ROOT_NODE;
							pRoot->nodeflag = NODE_CHANGED;
							pRoot->siteLike = 0;
							pRoot->width    = 0;
							pRoot->height	= 0;

							pTree->flag = TREE_INITAILZED;
							if( pLeft )
								setLeftChild( pRoot, pLeft );
							if( pRight )
								setRightChild( pRoot, pRight );
							if( pParent )
							{
								setParentChild( pRoot, pParent);
								pTree->rooted = FALSE;
							}
							else
							{
								pTree->rooted = TRUE;
								derootPhyloTree( pTree );
							}

							fReadTreeOK	= TRUE;
						}
					}
					else
					{
						fReadTreeOK		=	FALSE;
						error("\')\' is expected");
					}
				}
				else
				{
					fReadTreeOK		=	FALSE;
					error("read right child of the root failed");
				}
			}
			else
			{
				fReadTreeOK		=	FALSE;
				error("comma is expected");
			}
		}
		else
		{
			fReadTreeOK		=	FALSE;
			error("read left child of the roor failed");
		}
	}
	else
	{
		fReadTreeOK		=	FALSE;
		error(" \'(\' is expected");
	}
	return fReadTreeOK;
}

void writeBinaryTreeNode( FILE *out, PhyloTree *pTree, BinaryTreeNode *pNode, boolean fShowBranchLength, boolean fUseTranslate )
{
	if ( pNode == NULL )
		//nothing to write for null node
		return;

	if ( pNode->nodetype == LEAF_NODE ){
		if(fUseTranslate)
		{
			fprintf(out, "%d", pNode->index + 1);
		}
		else
		{
			fprintf(out, "%s", pTree->pTaxaLabels->data[pNode->index]);
		}
	}
	else if ( pNode->nodetype == INTERNAL_NODE ){
		fprintf(out, "(");
		writeBinaryTreeNode(out, pTree, pNode->left, fShowBranchLength, fUseTranslate);
		fprintf(out, ",");
		writeBinaryTreeNode(out, pTree, pNode->right, fShowBranchLength, fUseTranslate);
		fprintf(out, ")");
	}
	else if ( pNode->nodetype == ROOT_NODE ){
		fprintf(out, "(");
		writeBinaryTreeNode(out, pTree, pNode->left, fShowBranchLength, fUseTranslate);
		fprintf(out, ",");
		writeBinaryTreeNode(out, pTree, pNode->right, fShowBranchLength, fUseTranslate);
		if(pNode->parent){
			fprintf(out, ",");
			writeBinaryTreeNode(out, pTree, pNode->parent, fShowBranchLength, fUseTranslate);
		}
		fprintf(out, ")");
		return;
	}
	if( fShowBranchLength )
	{
		fprintf(out, ":%.6f", pNode->brlen);
	}
}

boolean	writePhyloTree(FILE *out, PhyloTree *pTree, boolean fShowBranchLength, boolean fUseTranslate)
{
	fprintf(out, "   tree %s = ", pTree->label);
	writeBinaryTreeNode(out, pTree, pTree->root, fShowBranchLength, fUseTranslate);
	fprintf(out, ";\n");
	return TRUE;
}

int	 npos, indx;
char sbuf[32];
void writeBinaryTreeNodeToBuffer( char *buf, PhyloTree *pTree, BinaryTreeNode *pNode, boolean fShowBranchLength, boolean fUseTranslate )
{
	if ( pNode == NULL )
		//nothing to write for null node
		return;

	if ( pNode->nodetype == LEAF_NODE ){
		if(fUseTranslate)
		{
			sprintf(sbuf, "%d", pNode->index + 1);
			indx = 0; while( sbuf[indx] ) buf[npos++] = sbuf[indx++]; buf[npos]='\0';
		}
		else
		{
			sprintf(sbuf, "%s", pTree->pTaxaLabels->data[pNode->index]);
			indx = 0; while( sbuf[indx] ) buf[npos++] = sbuf[indx++]; buf[npos]='\0';
		}
	}
	else if ( pNode->nodetype == INTERNAL_NODE ){
		sprintf(sbuf, "(");
		indx = 0; while( sbuf[indx] ) buf[npos++] = sbuf[indx++]; buf[npos]='\0';
		writeBinaryTreeNodeToBuffer(buf, pTree, pNode->left, fShowBranchLength, fUseTranslate);
		sprintf(sbuf, ",");
		indx = 0; while( sbuf[indx] ) buf[npos++] = sbuf[indx++]; buf[npos]='\0';
		writeBinaryTreeNodeToBuffer(buf, pTree, pNode->right, fShowBranchLength, fUseTranslate);
		sprintf(sbuf, ")");
		indx = 0; while( sbuf[indx] ) buf[npos++] = sbuf[indx++]; buf[npos]='\0';
	}
	else if ( pNode->nodetype == ROOT_NODE ){
		sprintf(sbuf, "(");
		indx = 0; while( sbuf[indx] ) buf[npos++] = sbuf[indx++]; buf[npos]='\0';
		writeBinaryTreeNodeToBuffer(buf, pTree, pNode->left, fShowBranchLength, fUseTranslate);
		sprintf(sbuf, ",");
		indx = 0; while( sbuf[indx] ) buf[npos++] = sbuf[indx++]; buf[npos]='\0';
		writeBinaryTreeNodeToBuffer(buf, pTree, pNode->right, fShowBranchLength, fUseTranslate);
		if(pNode->parent){
			sprintf(sbuf, ",");
			indx = 0; while( sbuf[indx] ) buf[npos++] = sbuf[indx++]; buf[npos]='\0';
			writeBinaryTreeNodeToBuffer(buf, pTree, pNode->parent, fShowBranchLength, fUseTranslate);
		}
		sprintf(sbuf, ")");
		indx = 0; while( sbuf[indx] ) buf[npos++] = sbuf[indx++]; buf[npos]='\0';
		return;
	}
	if( fShowBranchLength )
	{
		sprintf(sbuf, ":%.6f", pNode->brlen);
		indx = 0; while( sbuf[indx] ) buf[npos++] = sbuf[indx++]; buf[npos]='\0';
	}

}
int	writePhyloTreeToBuffer(char *buf, PhyloTree *pTree, boolean fShowBranchLength, boolean fUseTranslate)
{
	npos = 0;
	sprintf(sbuf, "   tree %s = ", pTree->label);
	indx = 0; while( sbuf[indx] ) buf[npos++] = sbuf[indx++]; buf[npos]='\0';
	writeBinaryTreeNodeToBuffer(buf, pTree, pTree->root, fShowBranchLength, fUseTranslate);
	sprintf(sbuf, ";\n");
	indx = 0; while( sbuf[indx] ) buf[npos++] = sbuf[indx++]; buf[npos]='\0';
	
	return npos;
}

//function: void setRandomBranchLength((PhyloTree *pTree, int minchanges, int maxchanges, double scale)
//description: generate random branch length for tree <t>
//parameters: 
//    t            the phylogenetic tree whose branch lengths are to be set
//    minchanges   the minimum changes for each branch
//    maxchanges   the maximum changes for each branch
//    scale        the scaling factors for branch length: 
//requires:  
//    (1)     <t> is initialized & 
//    (2)     minchange >= 1
//    (3)     maxchange >= 1
//    (4)     scale     >  0.0
//effects:    the branch lengths of <t> are modified after the calling
void setRandomBranchLength(PhyloTree *pTree, int minchanges, int maxchanges, double scale)
{
	int i;
	double s;
	int   cmin, cmax;

	cmin  = minchanges * 10000;
	cmax  = maxchanges * 10000;
	s = scale / cmax;
        
	for (i=0; i<pTree->nnodes; i++){
		pTree->nodes[i].brlen = irand(cmin, cmax) * s;
	}
	pTree->root->brlen = 0.0;
}

BinaryTreeNode *random_partition(int *permutation, int lower, int upper, PhyloTree *pTree, int *nInternal)
{
	int posPartition;	//partion point
	BinaryTreeNode *pLeft, *pRight, *pMe;

	if(lower>=upper)
		//only one leaf node
	{
		pMe = pTree->nodes + permutation[lower];
		pMe->nodetype = LEAF_NODE;
	}else
		//there are more leave node
	{
		posPartition = irand(lower, upper-1);

		pLeft = random_partition(permutation, lower, posPartition, pTree, nInternal);
		pRight = random_partition(permutation, posPartition+1, upper, pTree, nInternal);

		pMe = pTree->internodes + *nInternal;
		*nInternal = *nInternal + 1;
		pMe->nodetype = INTERNAL_NODE;

		setLeftChild(pMe, pLeft);
		setRightChild(pMe, pRight);
	}
	return pMe;
}

boolean derootPhyloTree(PhyloTree *pTree)
{
	BinaryTreeNode *pRoot=0, 
		*pLeft=0, 
		*pRight=0;

	assert( pTree );
	assert( pTree->root );
	assert( pTree->flag == TREE_INITAILZED);
	//dumpPhyloTree( pTree );

	//debug("deroot - step 0");
	pRoot = pTree->root;
	if( pRoot->parent != NULL ){
		//tree is already rooted
		return TRUE;
	}

	//debug("deroot - step 1");
	pLeft = pRoot->left;
	pRight = pRoot->right; 
	if( !pLeft || !pRight ){
		error("tree has an internal node with only one leaf node");
		return FALSE;
	}

	if( pLeft->nodetype == LEAF_NODE && pRight->nodetype == LEAF_NODE){
		error("tree has only two leave nodes, can not be derooted");
		return FALSE;
	}

	//debug("deroot - step 2");

	if(pLeft->nodetype == INTERNAL_NODE)
	{
		pRoot = pLeft;
		pLeft->parent = pRight;
		pRight->parent = pLeft;
	}
	else 
	{
		pRoot = pRight;
		pRight->parent = pLeft;
		pLeft->parent = pRight;
	}
	//debug("deroot - step 3");
	pTree->root = pRoot;
	pTree->rooted = FALSE;
	pTree->root->nodetype = ROOT_NODE;
	pTree->nnodes = pTree->ntaxa + pTree->ntaxa - 2;

	//dumpPhyloTree( pTree );

	return TRUE;
}

//requires: t is allocated
boolean initPhyloTreeWithRandomPartition( PhyloTree *pTree )
{
	int	index=0;
	int nInternal=0;
	IVector	permutation;

	assert( pTree );
	assert( pTree->flag != TREE_UNALLOCATED);

	allocIVector( &permutation, pTree->ntaxa );

	creatPermutation( permutation.v, permutation.size );
	/*dumpIVector( permutation );*/

	//pTree->root = &pTree->nnodes[pTree->nnodes-1];
	//pTree->root->parent = NULL;
	pTree->root = random_partition(permutation.v, 0, pTree->ntaxa-1, pTree, &nInternal);
	pTree->root->nodetype = ROOT_NODE;
	//printf("Root Index:%d\n", pTree->root->index);
	pTree->flag = TREE_INITAILZED;

	//make the tree unrooted
	derootPhyloTree( pTree );
	//printf("Root Index:%d\n", pTree->root->index);
	
	//init BranchLength
	freeIVector( permutation );

	setRandomBranchLength( pTree, 10, 20, 0.1);

	return TRUE;
}


//void	drawPhyloTreeNode(BinaryTreeNode *pNode)
//{
//	int i, j;
//
//	if(!pNode)
//	{
//		return;
//	}
//	else if(pNode->nodetype == LEAF_NODE)
//	{
//		printf("%5c", ' ');
//		for(i=5; i < nDrawWidth - pNode->parent->height * nDrawScale; i++)
//		{
//			printf(" ");
//		}
//
//		for( ; i < nDrawWidth - pNode->height * nDrawScale-2; i++)
//		{
//			printf("-");
//		}
//		printf("%d\n", pNode->index);
//	}
//	else if(pNode->nodetype == INTERNAL_NODE)
//	{
//		drawPhyloTreeNode(pNode->left);
//		printf("%5c", ' ');
//		for(i=5; i < nDrawWidth - pNode->parent->height * nDrawScale; i++)
//		{
//			printf(" ");
//		}
//		for( ; i < nDrawWidth - pNode->height * nDrawScale-2; i++)
//		{
//			printf("-");
//		}
//		printf("%d\n", pNode->index);
//		drawPhyloTreeNode(pNode->right);
//	}
//	else if(pNode->nodetype == ROOT_NODE)
//	{
//		drawPhyloTreeNode(pNode->left);
//		printf("%5c%d\n", ' ',pNode->index);
//		drawPhyloTreeNode(pNode->right);
//		printf("\n");
//		drawPhyloTreeNode(pNode->parent);
//	}
//}

int	calcTreeNodeHeight( BinaryTreeNode *pNode)
{
	int	hleft, hright, hparent;
	if( !pNode )
	{
		bug("unexpected null node in calcTreeNodeHeight");
		return -1;
	}
	else if (pNode->nodetype == LEAF_NODE )
	{
		pNode->height = 0;
	}
	else if (pNode->nodetype == INTERNAL_NODE )
	{
		hleft  = calcTreeNodeHeight(pNode->left);
		hright = calcTreeNodeHeight(pNode->right);
		pNode->height = max2(hleft, hright) + 1;
	}
	else if(pNode->nodetype == ROOT_NODE)
	{
		hleft  = calcTreeNodeHeight(pNode->left);
		hright = calcTreeNodeHeight(pNode->right);
		if( pNode->parent )
		{
			hparent = calcTreeNodeHeight( pNode->parent );
			hright = max2( hright, hparent );
		}
		pNode->height = max2(hleft, hright) + 1;
	}
	else
	{
		bug("unexpected nodetype in calcTreeNodeHeight");
		return -2;
	}
	return pNode->height;
}

float	calcTreeNodeDistance( BinaryTreeNode *pNode)
{
	if( pNode == NULL )
	{
		bug("unexpected null node in calcTreeNodeDistance");
		return 0.0;
	}
	else if (pNode->nodetype == LEAF_NODE )
	{
		pNode->fDistToRoot = pNode->parent->fDistToRoot + pNode->brlen;
	}
	else if (pNode->nodetype == INTERNAL_NODE )
	{
		pNode->fDistToRoot = pNode->parent->fDistToRoot + pNode->brlen;
		calcTreeNodeDistance( pNode->left );
		calcTreeNodeDistance( pNode->right );
	}
	else if (pNode->nodetype == ROOT_NODE )
	{
		pNode->fDistToRoot = 0.0;
		calcTreeNodeDistance( pNode->left );
		calcTreeNodeDistance( pNode->right );
		if( pNode->parent )
		{
			calcTreeNodeDistance( pNode->parent );
		}
	}
	return pNode->fDistToRoot;
}

float getMaxTreeNodeDistance( BinaryTreeNode *pNode )
{
	float fLeft, fRight, fParent;
	if( pNode == NULL )
	{
		bug("unexpected null node in getMaxTreeNodeDistance");
		return 0.0;
	}
	else if (pNode->nodetype == LEAF_NODE )
	{
		return pNode->fDistToRoot;
	}
	else if (pNode->nodetype == INTERNAL_NODE )
	{
		fLeft = getMaxTreeNodeDistance( pNode->left );
		fRight = getMaxTreeNodeDistance( pNode->right );
		return max2( fLeft, fRight );
	}else if (pNode->nodetype == ROOT_NODE )
	{
		fLeft = getMaxTreeNodeDistance( pNode->left );
		fRight = getMaxTreeNodeDistance( pNode->right );
		if( pNode->parent )
		{
			fParent = getMaxTreeNodeDistance( pNode->parent );
			fRight = max2( fParent, fRight );
		}
		return max2( fLeft, fRight );
	}
	else
	{
		bug("unexpected Node Type in getMaxTreeNodeDistance");
		return 0.0;
	}
}

int		calcTreeNodeWidth( BinaryTreeNode *pNode, int *nCounter )
{
	if( !pNode )
	{
		bug("unexpected null node in calcTreeNodeWidth");
		return -1;
	}
	else if (pNode->nodetype == LEAF_NODE )
	{
		pNode->width = *nCounter;
		*nCounter = *nCounter + 1;
	}
	else if (pNode->nodetype == INTERNAL_NODE )
	{
		calcTreeNodeWidth( pNode->left, nCounter);
		pNode->width = *nCounter;
		*nCounter = *nCounter + 1;
		calcTreeNodeWidth( pNode->right, nCounter);
	}
	else if(pNode->nodetype == ROOT_NODE)
	{
		calcTreeNodeWidth( pNode->left, nCounter);
		*nCounter = *nCounter + 1;

		calcTreeNodeWidth( pNode->right, nCounter);
		pNode->width = *nCounter;

		*nCounter = *nCounter + 1;
		if( pNode->parent )
			calcTreeNodeWidth( pNode->parent, nCounter);
	}
	return *nCounter;
}


extern	IMatrix			g_layout;
extern	double			g_layout_width;
extern	double			g_layout_scale;

#define		LAYOUT_WHITESPACE	-1
#define		LAYOUT_VBAR			-2
#define		LAYOUT_HBAR			-3
#define		LAYOUT_UPPERCORNER	-4
#define		LAYOUT_LOWERCORNER	-5
#define		LAYOUT_INTERNALNODE	-6
#define		LAYOUT_ROOTNODE		-7

void display_Layout(FILE *out, PhyloTree *tree)
{
	int	y, x, index;
	for( y = 0; y<g_layout.nrows; y++)
	{
		fputc(' ', out); fputc(' ', out);
		for( x = 0; x <=g_layout_width; x++)
		{
			switch( g_layout.data[y][x] )
			{
			case LAYOUT_WHITESPACE:
				fputc(' ', out);
				break;
			case LAYOUT_VBAR:
				fputc('|', out);
				break;
			case LAYOUT_HBAR:
				fputc('-', out);
				break;
			case LAYOUT_UPPERCORNER:
				fputc('/', out);
				break;
			case LAYOUT_LOWERCORNER:
				fputc('\\', out);
				break;
			case LAYOUT_INTERNALNODE:
				fputc('+', out);
				break;
			case LAYOUT_ROOTNODE:
				fputc('*', out);
				break;
			default:
				/*fprintf(out, "%d", g_layout.data[y][x] );*/
				index = g_layout.data[y][x]; 
				if( tree->nodes[ index ].nodetype == LEAF_NODE )
					fprintf(out, "%s (%d)", 	tree->pTaxaLabels->data[ tree->nodes[index].index ], index );
				else
					fprintf(out, "(%d)", tree->nodes[index].index + 1 );
				break;
			}
		}
		fputc('\n', out);
	}
}

void  draw_layout(FILE *out, PhyloTree *pTree, boolean fShowInternalLabel)
{
	int	i, j, x, y, X, Y, X0, Y0;
	BinaryTreeNode *pNode;
	
	//debug("layout: %d %d", g_layout.nrows, g_layout.ncols );
	for(i=0; i<g_layout.nrows; i++)
	{
		for(j=0; j<g_layout.ncols; j++)
			g_layout.data[i][j] = -1 ;
	}

	//put nodes in position
	for(i=0; i<pTree->nnodes; i++)
	{
		pNode = pTree->nodes + i;
		X = pNode->height;
		Y = pNode->width;

		if( pNode->parent )
		{
			X0 = pNode->parent->height;
			Y0 = pNode->parent->width;
		}
		else
		{
			X0 = 0;
			Y0 = Y;
		}

		/*debug("[%d] X=%d Y=%d X0=%d Y0=%d", i, X, Y, X0, Y0);*/
		
		for( x=X0+1; x <= X; x++)
		{
			g_layout.data[Y][x] = LAYOUT_HBAR;
		}
		//draw th evertical bar
		if( pNode->nodetype == INTERNAL_NODE )
		{
			for( y = pNode->left->width+1; y<=pNode->right->width-1; y++)
				g_layout.data[y][X] = LAYOUT_VBAR;
			g_layout.data[pNode->left->width][X]  = LAYOUT_UPPERCORNER;
			g_layout.data[pNode->right->width][X] = LAYOUT_LOWERCORNER;
			g_layout.data[Y][X] = LAYOUT_INTERNALNODE;
		}
		else if( pNode->nodetype == ROOT_NODE )
		{
			for( y = pNode->left->width+1; y<=pNode->parent->width-1; y++)
				g_layout.data[y][X] = LAYOUT_VBAR;
			g_layout.data[pNode->left->width][X]  = LAYOUT_UPPERCORNER;
			g_layout.data[pNode->parent->width][X] = LAYOUT_LOWERCORNER;
			g_layout.data[Y][X] = LAYOUT_ROOTNODE;
		}
		//draw the horizontal bar

	}

	//draw the label
	for(i=0; i<pTree->nnodes; i++)
	{
		pNode = pTree->nodes + i;
		X = pNode->height;
		Y = pNode->width;
		if( pNode->nodetype == LEAF_NODE || fShowInternalLabel )
			g_layout.data[Y][X+1] = pNode->index;
	}
}


void	drawPhyloTree(PhyloTree *pTree, boolean fShowInternalLabel)
{
	int i, maxX, nCount = 0;
	BinaryTreeNode *pNode;

	calcTreeNodeHeight( pTree->root );
	calcTreeNodeWidth( pTree->root, &nCount );

	maxX = pTree->root->height;
	g_layout_scale = g_layout_width / (maxX + 1);
	
	for(i=0; i<pTree->nnodes; i++)
	{
		pNode = pTree->nodes + i;
		pNode->height = maxX - pNode->height;
		pNode->height = min2( pNode->height * g_layout_scale, g_layout_width-1 );
	}
	/*dumpPhyloTree( pTree );*/
	draw_layout( stdout, pTree, fShowInternalLabel);
	display_Layout(stdout, pTree );
}

void	drawPhyloTreeBr(PhyloTree *pTree, boolean fShowInternalLabel, boolean fShowBranchLength, FILE *out)
{
	int i, maxX, nCount = 0;
	double	maxDistance;
	BinaryTreeNode *pNode;
	
	//compute the distance to the root for all nodes, which is unscaled X-Axle coordinate
	calcTreeNodeDistance( pTree->root );
	//get the maximum distance
	maxDistance = getMaxTreeNodeDistance( pTree->root );
	//compute the Y-Axis coordinate for each node
	calcTreeNodeWidth( pTree->root, &nCount );

	g_layout_scale = g_layout_width / maxDistance;

	for(i=0; i<pTree->nnodes; i++)
	{
		pNode = pTree->nodes + i;
		pNode->height = pNode->fDistToRoot * g_layout_scale;
		pNode->height = min2( pNode->height, g_layout_width-1 );
	}

	draw_layout( out, pTree, fShowInternalLabel);
	display_Layout(out, pTree );	
}


void	copyPhyloTree(PhyloTree *pDestTree, PhyloTree *pSrcTree)
{
	int		i;
	BinaryTreeNode		*pSrcNode, *pDestNode;
	double				*pdTmp;

	pDestTree->ntaxa			=			pSrcTree->ntaxa;
	pDestTree->nnodes			=			pSrcTree->nnodes;
	pDestTree->rooted			=			pSrcTree->rooted;	
	pDestTree->flag				=			pSrcTree->flag;	
	pDestTree->lnL				=			pSrcTree->lnL;
	pDestTree->fDistance		=			pSrcTree->fDistance;
	pDestTree->root				=			pDestTree->nodes + pSrcTree->root->index;
	pDestTree->root->nodetype	=			ROOT_NODE;
	

	for(i=0; i<pDestTree->nnodes; i++)
	{	
		pSrcNode = pSrcTree->nodes + i;				//get locationof the source node
		pDestNode = pDestTree->nodes + i;			//get locationof the dest   node

		
		pDestNode->brlen = pSrcNode->brlen;
		pDestNode->depth = pSrcNode->depth;
		pDestNode->nodeflag = pSrcNode->nodeflag;
		pDestNode->nodetype = pSrcNode->nodetype;

		if( pSrcNode->left )
			pDestNode->left = pSrcNode->left - pSrcTree->nodes + pDestTree->nodes;
		else
			pDestNode->left = NULL;

		if( pSrcNode->right )
			pDestNode->right = pSrcNode->right - pSrcTree->nodes + pDestTree->nodes;
		else
			pDestNode->right = NULL;

		if( pSrcNode->parent )
			pDestNode->parent = pSrcNode->parent - pSrcTree->nodes + pDestTree->nodes;
		else
			pDestNode->parent = NULL;

		//pdTmp = pDestNode->siteLike;
		//pDestNode->siteLike = pSrcNode->siteLike;
		//pSrcNode->siteLike = pdTmp;
	}
}

void setUpdateflag( PhyloTree *pTree)
{
	int i;
	assert(pTree);
	for(i=0; i<pTree->ntaxa; i++)
		pTree->nodes[i].nodeflag = NODE_NOCHANG;
	for(i=pTree->ntaxa; i<pTree->nnodes; i++)
		pTree->nodes[i].nodeflag = NODE_CHANGED;
}

void clearUpdateflag( PhyloTree *pTree)
{
	int i;
	for(i=pTree->ntaxa; i<pTree->nnodes; i++)
		pTree->nodes[i].nodeflag = NODE_NOCHANG;

}


boolean verifyTreeNode( BinaryTreeNode *pNode )
{
	boolean		fTreeNodeOK = TRUE;

	if( pNode )
	{
		debug(" node %d is good", pNode->index );

		if( ROOT_NODE == pNode->nodetype )
		{
			fTreeNodeOK = fTreeNodeOK && verifyTreeNode( pNode->left   );
			fTreeNodeOK = fTreeNodeOK && verifyTreeNode( pNode->right  );
			fTreeNodeOK = fTreeNodeOK && verifyTreeNode( pNode->parent );
		}
		else if ( INTERNAL_NODE == pNode->nodetype )
		{
			fTreeNodeOK = fTreeNodeOK && verifyTreeNode( pNode->left   );
			fTreeNodeOK = fTreeNodeOK && verifyTreeNode( pNode->right  );
		}
	}
	else
	{
		debug(" node is bad" );
		fTreeNodeOK = FALSE;
	}

	return	fTreeNodeOK;
}

//Move the root of a PhyloTree to newroot
boolean rerootPhyloTree(PhyloTree *tree, BinaryTreeNode *newroot)
{
	BinaryTreeNode *curr, *parent, *child, *Q, *oldroot;
	double	savedbr, savedbr2;
	boolean fSucceed = FALSE;

	if( !tree->rooted )
	{
		if( newroot->nodetype != LEAF_NODE )
		{
			oldroot = tree->root;
			if( newroot != oldroot )
			{
				curr   = newroot;
				parent = curr->parent;
				savedbr =  curr->brlen;
		
				while( curr != oldroot )
				{		
					//detach curr from previous parent
					if( curr == parent->left)
						parent->left = 0;
					else if ( curr == parent->right)
						parent->right = 0;
					else
						;

					//atach parent as child of curr
					Q = parent->parent;
					parent->parent = curr;
					if( !curr->left )
						curr->left = parent;
					else if( !curr->right )
						curr->right = parent;
					else
						;

					savedbr2		= curr->brlen;
					curr->brlen     = savedbr;
					savedbr			= savedbr2;
					curr->nodeflag  = NODE_CHANGED;

					curr = parent;
					parent = Q;
				}

				if ( !curr->left )
					curr->left = parent;
				else if( !curr->right )
					curr->right = parent;
				else
					curr->parent = parent;
				curr->brlen = savedbr;
				curr->nodeflag  = NODE_CHANGED;

				//set the nodetype
				oldroot->nodetype = INTERNAL_NODE;
				newroot->nodetype = ROOT_NODE;
				/*newroot->nodeflag = NODE_CHANGED;*/
				tree->root = newroot;
				fSucceed = TRUE;
			}
			else  //tree is already rooted as newroot
			{
				//message("The newroot is same as the oldroot");
				fSucceed = TRUE;
			}
		}
		else //leaf node
		{
			message("The root node has to be an internal node.");
		}
	}
	else	//rooted tree
	{
		message("Only unrooted tree can be reroot.");
	}
	return fSucceed;
}


BinaryTreeNode *rerootSubTree(BinaryTreeNode *oldroot, BinaryTreeNode *newroot)
{
	BinaryTreeNode	*L, *R, *P, *Q, *curr, *child, *parent;
	double			savedbrlen, savedbrlen2;

	if( oldroot == newroot || oldroot == newroot->parent )
	{
		//printf("no need to reroot");
		return oldroot;
	}
	else
	{
		//printf("oldroot=%d newroot=%d\n", oldroot->index, newroot->index);
		
		//detach the children of oldroot
		L = oldroot->left;
		R = oldroot->right;
		L->parent = 0;
		R->parent = 0;
		oldroot->left  = 0;
		oldroot->right = 0;
		//printf("L=%d R=%d\n", L->index, R->index);

	
		//detach newroot from its parent
		P = newroot->parent;
		if( newroot == P->left)
			P->left = 0;
		else if ( newroot == P->right )
			P->right = 0;
		else
			error("Subtree is ill-defined at %d (1001)", P->index);	
		//printf("P=%d\n", P->index);

		//attach newroot and P to oldroot as children
		newroot->parent = oldroot;
		oldroot->left  = newroot;
		oldroot->right = P;
		savedbrlen = newroot->brlen;
		savedbrlen /= 2;
		newroot->brlen = savedbrlen;
		//savedbrlen2    = P->brlen;
		//P->brlen       = savedbrlen;
		//savedbrlen     = savedbrlen2;

		curr = P;
		parent = P->parent;
		P->parent       = oldroot;

		while ( curr != L && curr != R )
		{
			//printf("curr=%d parent=%d\n", curr->index, parent->index);

			//detach curr from its parent
			if( curr == parent->left )
				parent->left = 0;
			else if ( curr == parent->right )
				parent->right = 0;
			else
			{
				error("Subtree is ill-defined at curr(%d) (1002)", curr->index);	
			}
			
			//attach parent as curr's child
			Q = parent->parent;
			parent->parent = curr;
			if( !curr->left )
				curr->left = parent;
			else if ( !curr->right )
				curr->right = parent;
			else 
			{
				error("Subtree is ill-defined at curr(%d) (1002)", curr->index);	
			}

			savedbrlen2    = curr->brlen;
			curr->brlen    = savedbrlen;
			savedbrlen     = savedbrlen2;
			curr->nodeflag  = NODE_CHANGED;

			curr   = parent;
			parent = Q;
		}

		curr->brlen    = savedbrlen;
		curr->nodeflag  = NODE_CHANGED;
		oldroot->nodeflag = NODE_CHANGED;

		if( curr == L )
		{
			R->parent = L;
			R->brlen += L->brlen;
			if( !L->left )
				L->left = R;
			else if ( !L->right )
				L->right = R;
			else
				error("Subtree is ill-defined at L(%d) (1003)",L->index);	
		}
		else if ( curr == R )
		{
			L->parent = R;
			L->brlen += R->brlen;
			if( !R->left )
				R->left = L;
			else if ( !R->right )
				R->right = L;
			else
				error("Subtree is ill-defined at L(%d) (1003)", R->index);	
		}
		else
		{
			error("Subtree is ill-defined at curr(%d) (1004)", curr->index);	
		}
		return oldroot;
	}
	
}

void _getNodelist(BinaryTreeNode *node, int *nodelist, int *size)
{
	if( node )
	{
		nodelist[*size] = node->index;
		*size = *size + 1;

		if( node->left )
			_getNodelist( node->left, nodelist, size);
		if( node->right )
			_getNodelist( node->right, nodelist, size);
	}
}

int getNodeListInSubTree(BinaryTreeNode *node, int *nodelist, int *size)
{
	*size = 0;
	_getNodelist( node, nodelist, size );
	return *size;
}


//
//void ProposalTree_TBR(PhylogeneticTree *T)
//{
//    int index, i;
//	int *Bu, Nu, Iu;
//	int *Bv, Nv, Iv;
//	TreeNode *u, *v, *Ru, *Rv, *p, *q;
//	index = irand(0, T->nnodes-1);
//	u = T->nodes + index;
//	v = u->adjNodes.b.parent;
//	debug("Tree TBR");
//	PhylogeneticTree_dump(T, stdout);
//
//	Bu = (int *)malloc(sizeof(int)*T->nnodes);
//	Bv = (int *)malloc(sizeof(int)*T->nnodes);
//	if( !Bu || !Bv )
//	{
//		err_system("can not allocate memeory in ProposalTree_SPR");
//	}
//
//	debug("selected branch %d-%d", u->index, v->index);
//
//	if(v->adjNodes.b.parent == u)
//		//case 1: u and v are the roots of each subtree
//	{
//		debug("u.parent(%d)==v.parent(%d)", u->index, v->index);
//		if(T->root==u)
//			u->brlen = v->brlen;
//		else if(T->root==v)
//			v->brlen = u->brlen;
//		else
//		{
//			err_bug("either u or v should be the root in ProposalTree_SPR");
//		}
//		u->adjNodes.b.parent = NULL;
//		v->adjNodes.b.parent = NULL;
//	}
//	else
//		//case 2: u are not the root
//	{
//		u->adjNodes.b.parent = NULL;
//		if(v->adjNodes.b.left == u)
//		{
//            v->adjNodes.b.left = NULL;
//			q = v->adjNodes.b.right;
//		}
//		else if (v->adjNodes.b.right == u)
//		{
//			v->adjNodes.b.right = NULL;
//			q = v->adjNodes.b.left;
//		}
//		else
//		{
//			err_bug("u can be v's parent in ProposalTree_SPR");
//		}
//
//		
//		//prune v
//		p = v->adjNodes.b.parent;
//		v->adjNodes.b.parent = NULL;
//		debug("v=%d, q=%d p=%d", v->index, q->index, p->index);
//
//		if(p->adjNodes.b.parent == v)
//		{
//			if(T->root==p)
//			{
//				p->brlen = v->brlen;
//			}
//			else
//			{
//				v->brlen = p->brlen;
//			}
//			p->adjNodes.b.parent = v;
//			q->adjNodes.b.parent = v;
//			v->adjNodes.b.left = p;
//			v->adjNodes.b.right = q;
//		}
//		else
//		{
//			if(p->adjNodes.b.left == v)
//			{
//				p->adjNodes.b.left = q;
//				q->adjNodes.b.parent = p;
//			}
//			else if(p->adjNodes.b.right == v)
//			{
//				p->adjNodes.b.right = q;
//				q->adjNodes.b.parent = p;
//			}
//			debug("locate the root of v");
//			Rv = p;
//			while(Rv!=T->root && Rv!=NULL)
//			{
//				debug("visit %d", Rv->index);
//				Rv = Rv->adjNodes.b.parent;
//			}
//			//make v the new root of the subtree
//			p = Rv->adjNodes.b.parent;
//			Rv->brlen = p->brlen;
//
//			v->adjNodes.b.left = Rv;
//			v->adjNodes.b.right = p;
//
//			Rv->adjNodes.b.parent = v;
//			p->adjNodes.b.parent = v;
//			v->adjNodes.b.parent = NULL;
//		}
//	}
//
//	Ru = u;
//	Rv = v;
//	debug("root of u is %d", Ru->index);
//	debug("root of v is %d", Rv->index);
//
//	Nu = 0;
//	GetSubtreeNodeList(Ru, Bu, &Nu);
//	Nv = 0;
//	GetSubtreeNodeList(Rv, Bv, &Nv);
//
//	Iu = irand(0, Nu-1);
//	Iu = Bu[Iu];
//	Iv = irand(0, Nv-1);
//	Iv = Bv[Iv];
//
//	printf("Bu=["); for(i=0; i<Nu; i++)	printf("%d ", Bu[i]); printf("] (%d)\n", Iu);
//	printf("Bv=["); for(i=0; i<Nv; i++)	printf("%d ", Bv[i]); printf("] (%d)\n", Iv);
//
//	p = T->nodes + Iu;
//	if(p!=u)
//	{
//        RerootSubtree(u, p);        
//	}
//	
//	p = T->nodes + Iv;
//	if(p!=v)
//	{
//        RerootSubtree(v, p);        
//	}
//	u->adjNodes.b.parent = v;
//	v->adjNodes.b.parent = u;
//	if(u->leafNode)
//		T->root = v;
//	else
//		T->root = u;
//   
//    free(Bu);
//	free(Bv);
//	PhylogeneticTree_dump(T, stdout);
//}
//
//void ProposalTree_NNI(PhylogeneticTree *T)
//{
//	int index;
//	TreeNode *u, *v, *A, *B, *C, *D;
//	
//	index = irand(T->ntaxa, T->nnodes-1);
//	u = T->nodes + index;
//
//	v = u->adjNodes.b.parent;
//	if(v->leafNode)
//	{
//		if(!u->adjNodes.b.left->leafNode)
//			v = u->adjNodes.b.left;
//		else if(!u->adjNodes.b.right->leafNode)
//			v = v = u->adjNodes.b.right;
//		else
//			return;
//		A = u;
//		u = v;
//		v = A;
//	}
//
//	index = irand(0, 2);
//	A = u->adjNodes.b.left;
//	B = u->adjNodes.b.right;
//
//	debug("Tree NNI");
//	if(index==0)
//		return;
//	PhylogeneticTree_dump(T, stdout);
//	//printf("(u,v)=(%d,%d)\n", u->index, v->index);
//	//printf("(A,B|C,D)=(%d,%d)\n", A->index, B->index);
//
//	if(v->adjNodes.b.left == u)
//	{
//		C = v->adjNodes.b.right;
//		D = v->adjNodes.b.parent;
//
//		if(index==1)
//		{
//			v->adjNodes.b.right = A;
//			A->adjNodes.b.parent = v;
//			u->adjNodes.b.left = C;
//			C->adjNodes.b.parent = u;
//		}
//		else
//		{
//			v->adjNodes.b.right = B;
//			B->adjNodes.b.parent = v;
//			u->adjNodes.b.right = C;
//			C->adjNodes.b.parent = u;
//		}
//	}
//	else if (v->adjNodes.b.right == u)
//	{
//		C = v->adjNodes.b.parent;
//		D = v->adjNodes.b.left;
//
//		if(index==1)
//		{
//			v->adjNodes.b.left = A;
//			A->adjNodes.b.parent = v;
//			u->adjNodes.b.left = D;
//			D->adjNodes.b.parent = u;
//		}
//		else
//		{
//			v->adjNodes.b.left = B;
//			B->adjNodes.b.parent = v;
//			u->adjNodes.b.right = D;
//			D->adjNodes.b.parent = u;
//		}
//	}
//	else if (v->adjNodes.b.parent == u)
//	{
//		C = v->adjNodes.b.left;
//		D = v->adjNodes.b.right;
//		if(index==1)
//		{
//			v->adjNodes.b.left = A;
//			A->adjNodes.b.parent = v;
//			u->adjNodes.b.left = C;
//			C->adjNodes.b.parent = u;
//		}
//		else
//		{
//			v->adjNodes.b.left = B;
//			B->adjNodes.b.parent = v;
//			u->adjNodes.b.right = C;
//			C->adjNodes.b.parent = u;
//		}
//	}
//	else
//	{
//		err_bug("Tree is not consistent");
//		exit(0);
//	}
//}
//
//void FindBset(TreeNode *node, int *Bset)
//{
//	if(!node) return;
//
//	Bset[node->index] = -1;
//	if(node->adjNodes.b.left)
//		FindBset(node->adjNodes.b.left, Bset);
//	if(node->adjNodes.b.right)
//		FindBset(node->adjNodes.b.right, Bset);
//}
//
//void ProposalTree_SPR(PhylogeneticTree *T)
//{
//	int index, n, i, j, m;
//	int	*Bset;
//	TreeNode *S, *P, *u, *v, *A, *B;
//
//	n = T->nnodes;
//	Bset = (int *)malloc(sizeof(int)*n);
//	if(!Bset)
//	{
//		err_system("allocate memory for BS in ProposalTree_SPR failed");
//		exit(0);
//	}
//
//	for(i=0; i<n; i++)
//	{
//		Bset[i] = i;
//	}
//
//	//choose a subtree
//	index = irand(0, T->nnodes-1);
//	S = T->nodes + index;
//	u = S->adjNodes.b.parent;
//	if(u->leafNode)
//	{
//		S = u;
//		u = S->adjNodes.b.parent;
//		index = S->index;
//	}
//
//	FindBset(S, Bset);
//	Bset[u->index] = -1;
//
//	//debug("\nTree SPR");
//	//PhylogeneticTree_dump(T, stdout);
//	//printf("Selected Subtree %d\n", index);
//	//printf("Bset=[");
//	//for(i=0; i<n; i++)
//	//{
//	//	printf("%d ", Bset[i]);
//	//}
//	//printf("]\n");
//
//	i=0; j=n-1;
//	while(1)
//	{
//		while(Bset[i]!=-1 && i<j) i++;
//		while(Bset[j]==-1 && i<j) j--;
//		//printf("i=%d j=%d\n", i, j);
//		if(i>=j)
//			break;
//		Bset[i] = Bset[j];
//		Bset[j] = -1;
//	}
//	m = i;
//	//printf("Bset=[");
//	//for(i=0; i<n; i++)
//	//{
//	//	printf("%d ", Bset[i]);
//	//}
//	//printf("](%d)\n", m);
//
//	
//	j = irand(0, m-1);    
//	index = Bset[j];
//	free(Bset);
//
//	//printf("Atach point: %d (Bset) %d(index)\n", j, index);
//
//	P = T->nodes + index;
//	v = P->adjNodes.b.parent;
//
//	
//	if(v==u)return;
//	//prune
//	if(u->adjNodes.b.left == S)
//	{
//		A = u->adjNodes.b.right;
//		B = u->adjNodes.b.parent;	
//		
//		if(B->adjNodes.b.left == u)
//		{
//			B->adjNodes.b.left = A;
//			A->adjNodes.b.parent = B;
//		}
//		else if(B->adjNodes.b.right == u)
//		{
//			B->adjNodes.b.right = A;
//			A->adjNodes.b.parent = B;
//		}
//		else //B->adjNodes.b.parent == u
//		{
//			A->adjNodes.b.parent = B;
//			B->adjNodes.b.parent = A;
//			if(T->root == u)
//			{
//				if (A->leafNode) 
//					T->root = B;
//				else if(B->leafNode) 
//					T->root = A;
//				else
//					T->root = A;
//			}
//		}
//	}
//	else if(u->adjNodes.b.right == S)
//	{
//		A = u->adjNodes.b.left;
//		B = u->adjNodes.b.parent;
//		
//		if(B->adjNodes.b.left == u)
//		{
//			B->adjNodes.b.left = A;
//			A->adjNodes.b.parent = B;
//		}
//		else if(B->adjNodes.b.right == u)
//		{
//			B->adjNodes.b.right = A;
//			A->adjNodes.b.parent = B;
//		}
//		else //B->adjNodes.b.parent == u
//		{
//			A->adjNodes.b.parent = B;
//			B->adjNodes.b.parent = A;
//			if(T->root == u)
//			{
//				if (A->leafNode) 
//					T->root = B;
//				else if(B->leafNode) 
//					T->root = A;
//				else
//					T->root = A;
//			}
//		}
//
//	}
//	else if(u->adjNodes.b.parent == S)
//	{
//		A = u->adjNodes.b.left;
//		B = u->adjNodes.b.right;
//		A->adjNodes.b.parent = B;
//		B->adjNodes.b.parent = A;
//		if (A->leafNode) 
//			T->root = B;
//		else if(B->leafNode) 
//			T->root = A;
//		else
//			T->root = A;
//	}
//
//	//graft
//	if(v->adjNodes.b.left==P)
//	{
//		v->adjNodes.b.left = u;
//		u->adjNodes.b.parent = v;
//		u->adjNodes.b.left = P;
//		u->adjNodes.b.right = S;
//		P->adjNodes.b.parent = u;
//		S->adjNodes.b.parent = u;
//	}
//	else if(v->adjNodes.b.right==P)
//	{
//		v->adjNodes.b.right = u;
//		u->adjNodes.b.parent = v;
//		u->adjNodes.b.left = P;
//		u->adjNodes.b.right = S;
//		P->adjNodes.b.parent = u;
//		S->adjNodes.b.parent = u;
//	}else //v->adjNodes.b.parent==P
//	{
//		if(T->root==v)
//		{
//			u->adjNodes.b.parent = v;
//			v->adjNodes.b.parent = u;
//			u->adjNodes.b.left = P;
//			u->adjNodes.b.right = S;
//			P->adjNodes.b.parent = u;
//			S->adjNodes.b.parent = u;
//		}
//		else if(T->root==P)
//		{
//			u->adjNodes.b.parent = P;
//			P->adjNodes.b.parent = u;
//			u->adjNodes.b.left = v;
//			u->adjNodes.b.right = S;
//			v->adjNodes.b.parent = u;
//			S->adjNodes.b.parent = u;
//		}
//		else
//		{
//			T->root = u;
//			u->adjNodes.b.parent = P;
//			P->adjNodes.b.parent = u;
//			u->adjNodes.b.left = v;
//			u->adjNodes.b.right = S;
//			v->adjNodes.b.parent = u;
//			S->adjNodes.b.parent = u;
//		}
//	}
//
//	if(T->root->leafNode)
//		T->root = T->root->adjNodes.b.parent;
//}
//
//void GetSubtreeNodeList(TreeNode *node, int *B, int *size)
//{
//	if(!node)
//		return;
//	B[*size] = node->index;
//	*size = *size + 1;
//	if(!node->leafNode)
//	{
//		GetSubtreeNodeList(node->adjNodes.b.left, B, size);
//		GetSubtreeNodeList(node->adjNodes.b.right, B, size);
//	}
//}
//


int normalizeTreeNode(BinaryTreeNode *node)
{
	int idx1, idx2;
	BinaryTreeNode *temp;

	if( node->nodetype == LEAF_NODE )
	{
		node->min_index = node->index;
	}
	else
	{
		idx1 = normalizeTreeNode(node->left);
		idx2 = normalizeTreeNode(node->right);
		if( idx1 > idx2 )
		{
			temp = node->left;
			node->left = node->right;
			node->right = temp;
			node->min_index = idx2;
		}
		else
		{
			node->min_index = idx1;
		}
    }		
	return node->min_index;
}

void normalizePhyloTree(PhyloTree *tree)
{
	BinaryTreeNode *temp;
	if(!tree->rooted)
	{
		rerootPhyloTree(tree, tree->nodes[0].parent);
		normalizeTreeNode( tree->root );
		normalizeTreeNode( tree->root->parent);
		
		if( tree->root->right->min_index > tree->root->parent->min_index )
		{
			temp = tree->root->parent;
			tree->root->parent= tree->root->right;
			tree->root->right = temp;
		}
		if( tree->root->left->min_index > tree->root->right->min_index )
		{
			temp = tree->root->right;
			tree->root->right= tree->root->left;
			tree->root->left = temp;
		}
	}
	else
	{
		normalizeTreeNode( tree->root );
	}
}

