#ifndef _PBPI_TREE_H
#define _PBPI_TREE_H

#include "util.h"
#include "model.h"
#include "pbpi.h"

#define	MAX_NUM_SIGNANATUR_UNIT		100

typedef enum TreeNodeType 
{
	LEAF_NODE, 
	INTERNAL_NODE, 
	ROOT_NODE
} TreeNodeType;

typedef enum TreeNodeFlag 
{
	NODE_UNINITAILZED, 
	NODE_CHANGED,	//NODE_CHANGE and NODE_NOCHANG was two flags 
	NODE_NOCHANG,	//used for local update in likelihood calculation
	NODE_VISITING, 
	NODE_VISITED 
} TreeNodeFlag;

typedef enum TreeFlag 
{
	TREE_UNALLOCATED, 
	TREE_UNINITAILZED, 
	TREE_INITAILZED,
	TREE_ERROR 
} TreeFlag;

typedef struct BinaryTreeNode BinaryTreeNode;

struct BinaryTreeNode				//a node with at most two children
{
	struct BinaryTreeNode	*parent;		//to parent node
	struct BinaryTreeNode	*left;			//to left child node
	struct BinaryTreeNode	*right;			//to right sibling node
	struct BinaryTreeNode	*sibling;		//to sibling node

	double	trprob[16];
	double	brlen;		//branch length to its parent node
	TreeNodeType			nodetype;
	TreeNodeFlag			nodeflag;

	int		index;		//the index of current node, map to dataset
	int		depth;		//distance to the root
	int		height;		//maxim distance to its children
	float	fDistToRoot; //total branch length from this node to root 
	int		width;
	int		min_index;	//minmum index of its children, use to normalize the tree
	double	*siteLike;
	unsigned	*signature;			//the signature of the node
}__attribute__ ((aligned (16)));

#define MAX_TREE_LABEL_LEN			31

typedef struct PhyloTree
{
	int	ntaxa;				//how many taxa, which equals # of internal node
	int	nnodes;				//# of total nodes
	boolean		rooted;		//TRUE for rooted true, FALSE for unrooted(free) tree
	//	TreeType	treetype;	//binary tree or multibranched
	//WE ONLY USE BINARY TREE HERER
	BinaryTreeNode	*nodes;		//array of tree node
	BinaryTreeNode	*internodes;//array of internal node, which is a subarray of the nodes array
	BinaryTreeNode	*root;		//pointer to the root node
	char		label[MAX_TREE_LABEL_LEN + 1];	//we assign a label to the tree
	TreeFlag	flag;			//tree flag
	double		lnL;			//the tree log likelihood
	float		fDistance;		//the maximum distance from a leaf node to the root	
	CMatrix		*pTaxaLabels;	//Taxa Labels
	int			nSignatureUnit;
}PhyloTree;

boolean	allocPhyloTree(PhyloTree *pTree, int nTaxa, boolean	rooted, char *szLabel, CMatrix *pTaxaLabels);
void	freePhyloTree(PhyloTree *pTree);
void	copyPhyloTree(PhyloTree *pDestTree, PhyloTree *pSrcTree);
void	dumpPhyloTree(PhyloTree *pTree);
void	drawPhyloTree(PhyloTree *pTree, boolean fShowInternalLabel);
void	drawPhyloTreeBr(PhyloTree *pTree, boolean fShowInternalLabel, boolean fShowBranchLength, FILE *out);
boolean	readPhyloTree(FILE *in, PhyloTree *pTree, boolean fUseTranslate);
void writeBinaryTreeNode( FILE *out, PhyloTree *pTree, BinaryTreeNode *pNode, boolean fShowBranchLength, boolean fUseTranslate );
boolean	writePhyloTree(FILE *out, PhyloTree *pTree, boolean fShowBranchLength, boolean fUseTranslate);
int	writePhyloTreeToBuffer(char *buf, PhyloTree *pTree, boolean fShowBranchLength, boolean fUseTranslate);
boolean initPhyloTreeWithRandomPartition( PhyloTree *pTree );
void setRandomBranchLength(PhyloTree *pTree, int minchanges, int maxchanges, double scale);
boolean derootPhyloTree(PhyloTree *pTree);
boolean rerootPhyloTree(PhyloTree *tree, BinaryTreeNode *newroot);
BinaryTreeNode *rerootSubTree(BinaryTreeNode *oldroot, BinaryTreeNode *newroot);
int getNodeListInSubTree(BinaryTreeNode *node, int *nodelist, int *size);

void setUpdateflag( PhyloTree *pTree);
void clearUpdateflag( PhyloTree *pTree);

void	updatePhyloTreeSignature( PhyloTree *pTree );
void	dumpPhyloTreeSignature(FILE *out, PhyloTree *pTree);
int		calcPhyloTreeDistance(PhyloTree *pTree0, PhyloTree *pTree1);
boolean verifyTreeNode( BinaryTreeNode *pNode );
void	writeSignature(FILE *out, unsigned *uNumbers, int numUnit, int numTaxa);
void reverseSignature(unsigned *signature,	int numUnit, int numUsefulBit);
boolean	matchSignature(unsigned *signatureA, unsigned *signatureB, int numUnit, int numUsefulBit);
void normalizePhyloTree(PhyloTree *tree);
int	calcPhyloTreeDistanceFN(PhyloTree *trueTree, PhyloTree *compTree, boolean fUpdateSignature);

#endif //_PBPI_TREE_H
