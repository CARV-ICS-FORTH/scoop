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
#include "proposal.h"
#include "tree.h"
#include "rand.h"
#include "runsetting.h"
#include "error.h"

extern RunSetting	g_rs;
static double lamda_brlen = 1.0;
static double delta=1.0e-6;

double proposeTreeBrlen(PhyloTree *T)
{
	int index;
	BinaryTreeNode *node;
	double scale, len0;

	//step 1: chose which branch to change
	index = irand(0, T->nnodes-1);		//the root can be any node
	if(index == T->root->index)
		index = T->root->parent->index;
	
	//step 2: make the change
	node = 	T->nodes + index;
	scale = exp( lamda_brlen * (unifdev()-0.5) );
	len0 = node->brlen;
	node->brlen = node->brlen * scale;
	if(node->brlen < delta) 
	{
		node->brlen = fabs( delta * 2.0 );
	}

	if(node->nodetype == ROOT_NODE)
	{
		debug("bug: change root's brlen");
	}

	//step 3: set the update flag
	do{
		node = node->parent;	//step 1 keep node != T->root at the beginning
		node->nodeflag = NODE_CHANGED;
		//printf("%d ", node->index);
	}while( node!=T->root);

	//step 4: return the ratio change
	return scale;
}

extern	int		*g_nodelist;
void proposeRerootSubTree(PhyloTree *T)
{
	int		index, numNodes;
	BinaryTreeNode *subtree, *newroot, *node;

	index = irand( T->ntaxa, T->nnodes-1 );
	//debug("choose subtree %d", index);
	
	subtree = T->nodes + index;
	getNodeListInSubTree( subtree, g_nodelist, &numNodes);

	index = g_nodelist[irand(0, numNodes-1 )];
	newroot = T->nodes + index;
	subtree = rerootSubTree(subtree, newroot);

	//step 3: set the update flag
	node = subtree;
	while( node != T->root )
	{
		node->nodeflag = NODE_CHANGED;
		node = node->parent;
	}
	node->nodeflag = NODE_CHANGED;
}

void proposeRerootPhyloTree(PhyloTree *T)
{
	int		index;
	BinaryTreeNode  *newroot;

	index = irand( T->ntaxa, T->nnodes-1 );
	newroot = T->nodes + index;
	rerootPhyloTree(T, newroot);
}

void proposeTBROld(PhyloTree *T)
{
	int		index, numNodes;
	BinaryTreeNode  *newroot, *temp, *subtree1, *subtree2;

	//choose the bisection point
	index = irand( T->ntaxa, T->nnodes-1 );
	newroot = T->nodes + index;
	rerootPhyloTree(T, newroot);

	//index = irand(0, 2);
	//switch( index )
	//{
	//case 0:
	//	temp = newroot->left;
	//	newroot->left = newroot->parent;
	//	newroot->parent = temp;
	//	break;
	//case 1:
	//	temp = newroot->right;
	//	newroot->right = newroot->parent;
	//	newroot->parent = temp;
	//	break;
	//case 2:
	//	break;
	//}

	//choose subtree1 and subtree2
	subtree1 = newroot;
	subtree2 = newroot->parent;

	getNodeListInSubTree( subtree1, g_nodelist, &numNodes);
	index = g_nodelist[irand(0, numNodes-1 )];
	newroot = T->nodes + index;
	rerootSubTree(subtree1, newroot);

	getNodeListInSubTree( subtree2, g_nodelist, &numNodes);
	index = g_nodelist[irand(0, numNodes-1 )];
	newroot = T->nodes + index;
	rerootSubTree(subtree2, newroot);
}


void proposeTBR(PhyloTree *T)
{
	int		index, numNodes;
	BinaryTreeNode  *u, *v, *p, *q, *rv, *newroot, *temp, *subtree1, *subtree2;

	/*debug("TBR");*/
	//choose the bisection branch
	index = irand( T->ntaxa, T->nnodes-1 );
	u = T->nodes + index;
	v = u->parent;

	T->root->brlen = T->root->parent->brlen;
	u->nodeflag = NODE_CHANGED;
	v->nodeflag = NODE_CHANGED;

	if( u == v->parent) //case 1: either us or v is the root
	{
		if( u != T->root && v!= T->root )
			error("either u or v must be the root of the tree in proposeTBR:(1001)");
		
		//disconnect u and v
		u->parent = 0;
		v->parent = 0;
	}
	else //case 2: u is not the root
	{
		//disconnect u from v
		u->parent = 0;
		if( u == v->left ) 
		{
			v->left = 0;
			q = v->right;
		}
		else if ( u == v->right )
		{
			v->right = 0;
			q = v->left;
		}
		else
			error("u must be the child of v in proposeTBR:(1002)");

		//disconnect v from its parent
		p = v->parent;
		v->parent = 0;
		if( v == p->parent ) //case 2.1 either v or p is the root of the tree
		{
			if ( v != T->root && p!=T->root )
				error("either p or v must be the root of the tree in proposeTBR:(1003)");

			p->parent = v;
			q->parent = v;
			v->left   = p;
			v->right  = q;
			p->nodeflag = NODE_CHANGED;
		}
		else //case 2.2  v and p are children nodes 
		{
			//connect q to p
			if( v == p->left )
			{
				p->left = q;
				q->parent = p;
			}
			else if ( v == p->right )
			{
				p->right  = q;
				q->parent = p;
			}
			else
				error("v must be the child of p in proposeTBR:(1004)");
			
			//find the root of the tree
			rv = p;
			while( rv != T->root )
			{
				rv = rv->parent;
				rv->nodeflag = NODE_CHANGED;
			}
			rv->nodeflag = NODE_CHANGED;
			p = rv->parent;
			v->left = rv;
			v->right = p;
			rv->parent = v;
			p->parent  = v;
			v->parent  = 0;
		}
	}
	//get the attach point of subtree u
	getNodeListInSubTree( u, g_nodelist, &numNodes);
	index = g_nodelist[irand(0, numNodes-1 )];
	newroot = T->nodes + index;
	if( newroot != u )
		rerootSubTree(u, newroot);

	//get the attach point of subtree v
	getNodeListInSubTree( v, g_nodelist, &numNodes);
	index = g_nodelist[irand(0, numNodes-1 )];
	newroot = T->nodes + index;
	if( newroot != v )
		rerootSubTree(v, newroot);

	//make v as the new root
	u->parent = v;
	v->parent = u;
	if( v->nodetype == LEAF_NODE )
	{
		temp = u;
		u = v;
		v = temp;
	}
	T->root->nodetype = INTERNAL_NODE;
	v->nodetype       = ROOT_NODE;
	T->root           = v;
}


//backbone slide and scale
static double lamda_BSS2 = 1.0;
void proposalTreeBSS2_old(PhyloTree *T)
{
	int Iu, flag, flagD;
	BinaryTreeNode *u, *v, *A, *B, *C, *D, *temp, *root;
	double y1, y2, y3;
	double ys, s;
	boolean reverse=FALSE;

	//must be at least 4 leaf nodes
	if( T->ntaxa < 4 )
	{
		warning("BSS2 requires at least 4 leaf nodes");
		return;
	}
	
	//adjust root such that root->parent is not a leaf node;
	root = T->root;
	root->brlen = root->parent->brlen;
	if(root->parent->nodetype == LEAF_NODE)
	{
		temp = root->parent;
		root->parent = root->left;
		root->left = temp;
	}
	////debug("adjust the root:");
	//drawPhyloTree(T, TRUE);
	//debug("select u and v:");

	//choose u and v
	Iu = irand(T->ntaxa, T->nnodes-1);
	//debug("Iu = %d", Iu);
	u = T->nodes + Iu;
	v = u->parent;
	//debug("select u=%d v=%d root=%d", u->index, v->index, T->root->index);
	if(u->nodetype == ROOT_NODE )
	{
		temp = v;
		v = u;
		u = temp;
	}
	//debug("select u=%d v=%d root=%d", u->index, v->index, T->root->index);
	//drawPhyloTree(T, TRUE);

	//case 1: v is the root
	if( v->nodetype == ROOT_NODE)
	{
		//debug("case 1");
		//case 1.1 u == v->parent;
		//case 1.2 u == v->left;
		//case 1.3 u == v->right
		if( v->parent == u)
		{
			//debug("case 1.1");
			v->parent = u;
		}
		else if( v->left == u)
		{
			//debug("case 1.2");
			temp = v->parent;
			v->parent = u;
			v->left = temp;
		} else if (v->right == u)
		{
			//debug("case 1.3");
			temp = v->parent;
			v->parent = u;
			v->right = temp;
		}

		//debug("case 1 starting");

		flag = irand(0, 3);
		switch( flag )
		{
		case 0:
			break;
		case 1:
			temp = u->left;
			u->left = u->right;
			u->right = temp;
			break;
		case 2:
			temp = v->left;
			v->left = v->right;
			v->right = temp;
			break;
		case 3:
			temp = u->left;
			u->left = u->right;
			u->right = temp;

			temp = v->left;
			v->left = v->right;
			v->right = temp;
			break;
		default:
			bug("irand return unexpected value");
		}
		//debug("flag=%d", flag);
		//drawPhyloTree(T, TRUE);

		A = u->left;
		B = u->right;
		C = v->left;
		D = v->right;

		y1 = A->brlen;
		y2 = y1 + u->brlen;
		y3 = y2 + D->brlen;

		s = exp( lamda_BSS2 * (unifdev()-0.5) );
		y1 = y1 * s;
		y2 = y2 * s;
		y3 = y3 * s;

		ys = y3 * unifdev();
		if( ys < y2 ) //no topology change
		{
			//debug("no topology change");
			A->brlen = ys;
			u->brlen = y2 - ys;
			D->brlen = y3 - y2;
		}
		else//has topology change
		{
			//debug("has topology change: u=%d:A=%d B=%d\tv=%d:C=%d D=%d",
			//	u->index, A->index, B->index, v->index, C->index, D->index);
			v->right = A;
			A->parent = v;

			u->left = D;
			D->parent = u;

			A->brlen = y2;
			D->brlen = y3 - ys;
			u->brlen = ys - y2;

			//debug("has topology change: u=%d:A=%d B=%d\tv=%d:C=%d D=%d",
			//	u->index, u->left->index, u->right->index, v->index, v->left->index, v->right->index);

		}
		u->nodeflag = NODE_CHANGED;
		v->nodeflag = NODE_CHANGED;
	}
	else
	{
		//debug("BSS2: case 2");
		if (u==v->right)
		{
			temp = v->left;
			v->left = u;
			v->right = temp;
		}

		flag = irand(0, 3);
		switch( flag )
		{
		case 0:
			flagD = FALSE;
			break;
		case 1:
			flagD = FALSE;
			temp = u->left;
			u->left = u->right;
			u->right = temp;
			break;
		case 2:
			flagD = TRUE;
			break;
		case 3:
			flagD = TRUE;
			temp = u->left;
			u->left = u->right;
			u->right = temp;
			break;
		default:
			bug("irand return unexpected value");
		}

		A = u->left;
		B = u->right;
		C = v->right;
		D = v->parent;

		if( flagD )
		{
			//debug("BSS2: case 2.1");
			y1 = A->brlen;
			y2 = y1 + u->brlen;
			y3 = y2 + C->brlen;
			
			s = exp( lamda_BSS2 * (unifdev()-0.5) );
			y1 = y1 * s;
			y2 = y2 * s;
			y3 = y3 * s;
			
			ys = y3 * unifdev();
			if( ys < y2 )
			{
				A->brlen = ys;
				u->brlen = y2 - ys;
				C->brlen = y3 - y2;
			}
			else
			{
				u->left = C;
				C->parent = u;

				v->right = A;
				A->parent = v;

				A->brlen = y2;
				C->brlen = y3 - ys;
				u->brlen = ys - y2;
			}
		}
		else
		{
			//debug("BSS2: case 2.2");
			y1 = A->brlen;
			y2 = y1 + u->brlen;
			y3 = y2 + v->brlen;
			s = exp( lamda_BSS2 * (unifdev()-0.5) );
			y1 = y1 * s;
			y2 = y2 * s;
			y3 = y3 * s;
			ys = y3 * unifdev();
			if (ys < y2 )
			{
				A->brlen = ys;
				u->brlen = y2 - ys;
				v->brlen = y3 - y2;
			}
			else
			{
				A->brlen = y2;
				u->brlen = ys - y2;
				v->brlen = y3 - ys;

				C->parent = u;
				u->right = C;

				B->parent = v;
				v->right = B;
			}
		}
	}

	//drawPhyloTree(T, TRUE);
	u->nodeflag = NODE_CHANGED;
	while( u!= T->root )
	{
		u->parent->nodeflag = NODE_CHANGED;
		u = u->parent;
	};
}


void proposalTreeBSS2(PhyloTree *T)
{
	int Iu, flag, flagD;
	BinaryTreeNode *u, *v, *A, *B, *C, *D, *temp, *root;
	double y1, y2, y3;
	double ys, s;
	boolean reverse=FALSE;

	//must be at least 4 leaf nodes
	if( T->ntaxa < 4 )
	{
		warning("BSS2 requires at least 4 leaf nodes");
		return;
	}
	
	//adjust root such that root->parent is not a leaf node;
	root = T->root;
	root->brlen = root->parent->brlen;
	if(root->parent->nodetype == LEAF_NODE)
	{
		temp = root->parent;
		root->parent = root->left;
		root->left = temp;
	}
	////debug("adjust the root:");
	//drawPhyloTree(T, TRUE);
	//debug("select u and v:");

	//choose u and v
	Iu = irand(T->ntaxa, T->nnodes-1);
	//debug("Iu = %d", Iu);
	u = T->nodes + Iu;
	v = u->parent;
	//debug("select u=%d v=%d root=%d", u->index, v->index, T->root->index);
	if(u->nodetype == ROOT_NODE )
	{
		temp = v;
		v = u;
		u = temp;
	}
	//debug("select u=%d v=%d root=%d", u->index, v->index, T->root->index);
	//drawPhyloTree(T, TRUE);

	//case 1: v is the root
	if( v->nodetype == ROOT_NODE)
	{
		//debug("case 1");
		//case 1.1 u == v->parent;
		//case 1.2 u == v->left;
		//case 1.3 u == v->right
		if( v->parent == u)
		{
			//debug("case 1.1");
			v->parent = u;
		}
		else if( v->left == u)
		{
			//debug("case 1.2");
			temp = v->parent;
			v->parent = u;
			v->left = temp;
		} else if (v->right == u)
		{
			//debug("case 1.3");
			temp = v->parent;
			v->parent = u;
			v->right = temp;
		}

		//debug("case 1 starting");

		flag = irand(0, 3);
		switch( flag )
		{
		case 0:
			break;
		case 1:
			temp = u->left;
			u->left = u->right;
			u->right = temp;
			break;
		case 2:
			temp = v->left;
			v->left = v->right;
			v->right = temp;
			break;
		case 3:
			temp = u->left;
			u->left = u->right;
			u->right = temp;

			temp = v->left;
			v->left = v->right;
			v->right = temp;
			break;
		default:
			bug("irand return unexpected value");
		}
		//debug("flag=%d", flag);
		//drawPhyloTree(T, TRUE);

		A = u->left;
		B = u->right;
		C = v->left;
		D = v->right;

		y1 = A->brlen;
		y2 = y1 + u->brlen;
		y3 = y2 + D->brlen;

		s = exp( lamda_BSS2 * (unifdev()-0.5) );
		y1 = y1 * s;
		y2 = y2 * s;
		y3 = y3 * s;

		ys = y3 * unifdev();
		if( ys < y2 ) //no topology change
		{
			//debug("no topology change");
			A->brlen = ys;
			u->brlen = y2 - ys;
			D->brlen = y3 - y2;
		}
		else//has topology change
		{
			//debug("has topology change: u=%d:A=%d B=%d\tv=%d:C=%d D=%d",
			//	u->index, A->index, B->index, v->index, C->index, D->index);
			v->right = A;
			A->parent = v;

			u->left = D;
			D->parent = u;

			A->brlen = y2;
			D->brlen = y3 - ys;
			u->brlen = ys - y2;

			//debug("has topology change: u=%d:A=%d B=%d\tv=%d:C=%d D=%d",
			//	u->index, u->left->index, u->right->index, v->index, v->left->index, v->right->index);

		}
		u->nodeflag = NODE_CHANGED;
		v->nodeflag = NODE_CHANGED;
	}
	else
	{
		//debug("BSS2: case 2");
		if (u==v->right)
		{
			temp = v->left;
			v->left = u;
			v->right = temp;
		}

		flag = irand(0, 3);
		switch( flag )
		{
		case 0:
			flagD = FALSE;
			break;
		case 1:
			flagD = FALSE;
			temp = u->left;
			u->left = u->right;
			u->right = temp;
			break;
		case 2:
			flagD = TRUE;
			break;
		case 3:
			flagD = TRUE;
			temp = u->left;
			u->left = u->right;
			u->right = temp;
			break;
		default:
			bug("irand return unexpected value");
		}

		A = u->left;
		B = u->right;
		C = v->right;
		D = v->parent;

		if( flagD )
		{
			//debug("BSS2: case 2.1");
			y1 = A->brlen;
			y2 = y1 + u->brlen;
			y3 = y2 + C->brlen;
			
			s = exp( lamda_BSS2 * (unifdev()-0.5) );
			y1 = y1 * s;
			y2 = y2 * s;
			y3 = y3 * s;
			
			ys = y3 * unifdev();
			if( ys < y2 )
			{
				A->brlen = ys;
				u->brlen = y2 - ys;
				C->brlen = y3 - y2;
			}
			else
			{
				u->left = C;
				C->parent = u;

				v->right = A;
				A->parent = v;

				A->brlen = y2;
				C->brlen = y3 - ys;
				u->brlen = ys - y2;
			}
		}
		else
		{
			//debug("BSS2: case 2.2");
			y1 = A->brlen;
			y2 = y1 + u->brlen;
			y3 = y2 + v->brlen;
			s = exp( lamda_BSS2 * (unifdev()-0.5) );
			y1 = y1 * s;
			y2 = y2 * s;
			y3 = y3 * s;
			ys = y3 * unifdev();
			if (ys < y2 )
			{
				A->brlen = ys;
				u->brlen = y2 - ys;
				v->brlen = y3 - y2;
			}
			else
			{
				A->brlen = y2;
				u->brlen = ys - y2;
				v->brlen = y3 - ys;

				C->parent = u;
				u->right = C;

				B->parent = v;
				v->right = B;
			}
		}
	}

	//drawPhyloTree(T, TRUE);
	u->nodeflag = NODE_CHANGED;
	while( u!= T->root )
	{
		u->parent->nodeflag = NODE_CHANGED;
		u = u->parent;
	};
}


typedef enum MoveType
{
	MT_BRLEN	=	0,
	MT_BSS2		=	1,
	MT_RST		=	2,
	MT_TBR		=	3,
	MT_RER		=	4
}MoveType;
static int	nMoveType	=	4;

void proposeNextTree( PhyloTree *pNextTree )
{
	MoveType mt;

	mt = irand(0, nMoveType-1);
	switch( mt )
	{
	case MT_BRLEN:
		proposeTreeBrlen( pNextTree );
		break;
	case MT_BSS2:
		proposalTreeBSS2( pNextTree );
		break;	
	case MT_RST:
		proposeRerootSubTree( pNextTree );
		break;
	case MT_RER:
		proposeRerootPhyloTree( pNextTree );
		break;
	case MT_TBR:
		proposeTBR( pNextTree );
		break;
	default:
		proposalTreeBSS2( pNextTree );
		break;
	}
	g_rs.lastProposalType = mt;
}

