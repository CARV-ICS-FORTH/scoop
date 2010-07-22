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
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <string.h>

#include "model.h"
#include "dataset.h"
#include "likelihood.h"
#include "error.h"
#include "tree.h"
#include "runsetting.h"
#include "mcmc.h"

#include "align_alloc.h"
#include "tpc.app.h"
//#include "app.c-spu.h"
#include <tpc_common.h>
#include <tpc_ppe.h>

#include "assert_aligned.h"
#include "ppu.app.h"
#include "common.h"
#include "stlpc.h"

#define ALIGNMENT_ASSERTIONS_ENABLED_

#define DEBUG_L0_LP0_
#define DEBUG_L1_LP0_
#define DEBUG_L2_LP0_

#define DEBUG_L0_LP1_
#define DEBUG_L1_LP1_
#define DEBUG_L2_LP1_

#define DEBUG_L0_LP2_
#define DEBUG_L1_LP2_
#define DEBUG_L2_LP2_

extern RunSetting		g_rs;
extern MarkovChain		*g_chains;				//pointer to chains on local process
extern Dataset			g_ds;					//pointer to current dataset
extern ChainProcessMap	g_map;					//pointer to current chain-process map
extern double *			g_likedata;				//global site likelihood
extern double **		g_leafdata;

extern unsigned int		use_SPEs;				//Make use of SPEs==1 or run Initial==0
extern unsigned int		vectorized;				//Make use of vectorization on SPEs
extern unsigned int		no_of_SPEs;				//The number of SPE's to be used (1 - 6).
extern unsigned int		SPE_frg_lp0;			//fragments loaded per SPE for loop 0.(1 - 1360).
extern unsigned int		SPE_frg_lp1;			//fragments loaded per SPE for loop 1.(1 - 1008).
extern unsigned int		SPE_frg_lp2;			//fragments loaded per SPE for loop 2.(1 - 3632).
extern struct tpc_ds		*ds[MAX_PROCS_NO];
extern struct tpc_ds		*ds0[2];
extern struct tpc_ds		*ds1[2];
extern struct tpc_ds		*ds2[2];
extern struct tpc_ds	        *ds_ppe;

const int tpc_lp0_args = 6;
const int tpc_lp1_args = 8;
const int tpc_lp2_args = 5;
const unsigned int constD16 = 16*sizeof(double);
const unsigned int tpc_ds_size = sizeof(struct tpc_ds);
const unsigned int constLD1 = sizeof(long double);
const unsigned int constD4 = 4*sizeof(double);

extern struct loop_properties lp0p;
extern struct loop_properties lp1p;
extern struct loop_properties lp2p;

extern long double *tmp_lnL;
extern unsigned int total_lp2_sgms;

extern uint64_t     time_lp0;
extern uint64_t     time_lp1;
extern uint64_t     time_lp2;

uint64_t     tmp_t_lp0 = 0;
uint64_t     tmp_t_lp1 = 0;
uint64_t     tmp_t_lp2 = 0;

void Character2SiteLike(char c, SiteLike p)
{
	p[0] = p[1] = p[2] = p[3] = 0.0;

	switch (c)
	{
	case 'A':
	case 'a':
		p[0]=1.0;
		break;
	case 'C':
	case 'c':
		p[1]=1.0;
		break;
	case 'G':
	case 'g':
		p[2]=1.0;
		break;
	case 'T':
	case 't':
		p[3]=1.0;
		break;
	case 'U':
	case 'u':
		p[3]=1.0;
		break;
	case 'M':
	case 'm':
		p[0] = 1.0;
		p[1] = 1.0;
		break;
	case 'R':
	case 'r':
		p[0] = 1.0;
		p[2] = 1.0;
		break;
	case 'W':
	case 'w':
		p[0] = 1.0;
		p[3] = 1.0;
		break;
	case 'S':
	case 's':
		p[1] = 1.0;
		p[2] = 1.0;
		break;

	case 'y':
		p[1] = 1.0;
		p[3] = 1.0;
		break;
	case 'K':
	case 'k':
		p[2] = 1.0;
		p[3] = 1.0;
		break;
	case 'B':
	case 'b':
		p[1] = 1.0;
		p[2] = 1.0;
		p[3] = 1.0;
		break;
	case 'D':
	case 'd':
		p[0] = 1.0;
		p[2] = 1.0;
		p[3] = 1.0;
		break;
	case 'H':
	case 'h':
		p[0] = 1.0;
		p[1] = 1.0;
		p[3] = 1.0;
		break;
	case 'V':
	case 'v':
		p[0] = 1.0;
		p[1] = 1.0;
		p[2] = 1.0;
		break;
	case 'N':
	case 'n':
	case 'O':
	case 'o':
	case '?':
	case '-':
	default:
		p[0] = 1.0;
		p[1] = 1.0;
		p[2] = 1.0;
		p[3] = 1.0;
		break;
	}
}

void dumpPattern( )
{
	int i, j;
	printf("Dataset\nntaxa=%d nchar=%d npattern=%d\n",
			g_ds.nTaxa, g_ds.nChar, g_ds.nPattern);
	for(i=0; i<g_ds.nTaxa; i++)
	{
		printf("%-5d\t", i);
		for(j=0; j<g_ds.nPattern; j++)
			putchar( g_ds.data.data[i][g_ds.compressedPattern.v[j]]);
		putchar('\n');
	}
	printf("\n\n");
}

void buildLeafSiteLikeData( )
{
	int		iTaxa,
	iPattern,
	*patterns;
	double	*sitelike;
	char	**matrix,
	c;

	//dumpPattern( );

	matrix		= g_ds.data.data;
	patterns	= g_ds.compressedPattern.v;

	for(iTaxa = 0; iTaxa < g_ds.nTaxa; iTaxa++)
	{
		sitelike = g_leafdata[ iTaxa ];
		for(iPattern = 0; iPattern < g_ds.nPattern; iPattern++)
		{
			c = matrix[ iTaxa ][ patterns[ iPattern ] ];
			Character2SiteLike(c, sitelike);

			/*	printf("[%d] %d:(%d) %c %10.2e %10.2e %10.2e %10.2e\n",
				iTaxa,
				iPattern,
				patterns[ iPattern ],
				c,
				sitelike[0],
				sitelike[1],
				sitelike[2],
				sitelike[3]);	*/

			sitelike = sitelike + NUM_BASE_STATES;
		}
	}
}

void ComputeNodeLikeData(BinaryTreeNode *node, Model *model)
{
	int			iPattern, iState;
	double			*pl, *pr, *pn, *pp;
	double			*tl, *tr, *tp;
	double			t1, t2, t3;
	BinaryTreeNode		*left, *right, *parent;

#ifdef ALIGNMENT_ASSERTIONS_ENABLED
	assertion_table_p lp0_at = NULL, lp1_at = NULL;
#endif

	struct Flag{
		unsigned value : 1;
	}flag;

	unsigned char wait_stop;
	unsigned int i, tmp_SPE_frg;
	register unsigned distance, fixed;
	register int limit;

	int total = g_ds.nPattern;
	struct tpc_ds *local_ds;

#ifdef ALIGNMENT_ASSERTIONS_ENABLED
	lp0_at = create_aligned(6);
	lp1_at = create_aligned(8);

	init_aligned(lp0_at, 0 , "pn");
	init_aligned(lp0_at, 1 , "pl");
	init_aligned(lp0_at, 2 , "pr");
	init_aligned(lp0_at, 3 , "tl");
	init_aligned(lp0_at, 4 , "tr");
	init_aligned(lp0_at, 5 , "ds");

	init_aligned(lp1_at, 0 , "pn");
	init_aligned(lp1_at, 1 , "pl");
	init_aligned(lp1_at, 2 , "pr");
	init_aligned(lp1_at, 3 , "pp");
	init_aligned(lp1_at, 4 , "tl");
	init_aligned(lp1_at, 5 , "tr");
	init_aligned(lp1_at, 6 , "tp");
	init_aligned(lp1_at, 7 , "ds");
#endif

	if( node->nodeflag == NODE_NOCHANG )
		return;
	else if( node->nodetype == LEAF_NODE )
	{
		return;
	}
	else if( node->nodetype == INTERNAL_NODE || (node->nodetype == ROOT_NODE && !node->parent) )
	{

                int idx=0;
		left = node->left;
		calcTransitionProbability( model, left->brlen, left->trprob);
		ComputeNodeLikeData( left, model );

		right = node->right;
		calcTransitionProbability( model, right->brlen, right->trprob);
		ComputeNodeLikeData( right, model );

		tmp_t_lp0 = __mftb();

		pn = node->siteLike;
		pl = left->siteLike;
		tl = left->trprob;
		pr = right->siteLike;
		tr = right->trprob;
		if( !use_SPEs ){
//			ds[0]->vcted = vectorized;
//			ds[0]->start = 0;
			ds[0]->end = g_ds.nPattern;
			ppu_loop0(pn, pl, pr, tl, tr, ds[0]);
		}
		else{
//	    		printf("Trying to execute loop : 0");
	    		tpc_wait_all();
			limit = lp0p.full_iterations;
			fixed = lp0p.full_fixed;
			distance = lp0p.full_distance;
			local_ds = ds0[0];
			while(limit > 0){
				#ifdef ALIGNMENT_ASSERTIONS_ENABLED
				set_aligned(lp0_at, 0 , pn, fixed);
				set_aligned(lp0_at, 1 , pl, fixed);
				set_aligned(lp0_at, 2 , pr, fixed);
				set_aligned(lp0_at, 3 , tl, constD16);
				set_aligned(lp0_at, 4 , tr, constD16);
				set_aligned(lp0_at, 5 , local_ds, tpc_ds_size);
				set_debug_info(lp0_at, limit, " Loop 0 [Full] ") ;

				assert_aligned(lp0_at);
				#endif
				tpc_call(LOOP_0, tpc_lp0_args,
						pn, fixed, TPC_INOUT_ARG,
						pl, fixed, TPC_IN_ARG,
						pr, fixed, TPC_IN_ARG,
						tl, constD16, TPC_IN_ARG,
						tr, constD16, TPC_IN_ARG,
						local_ds, tpc_ds_size, TPC_IN_ARG);
				pn += distance;
				pl += distance;
				pr += distance;
				--limit;
			}
			limit = lp0p.mod_iterations;
			fixed = lp0p.mod_fixed;
			distance = lp0p.mod_distance;
			local_ds = ds0[1];
			while(limit > 0){
//				printf("LOOP 0 Entered mod iter case.\n");
//				printf("fixed = %d.\n", lp0p.mod_fixed);
//				printf("ds0[1] = %d.\n", ds0[1]->end);

				#ifdef ALIGNMENT_ASSERTIONS_ENABLED
				set_aligned(lp0_at, 0 , pn, fixed);
				set_aligned(lp0_at, 1 , pl, fixed);
				set_aligned(lp0_at, 2 , pr, fixed);
				set_aligned(lp0_at, 3 , tl, constD16);
				set_aligned(lp0_at, 4 , tr, constD16);
				set_aligned(lp0_at, 5 , local_ds, tpc_ds_size);
				set_debug_info(lp0_at, limit, " Loop 0 [Mod] ") ;

				assert_aligned(lp0_at);
				#endif

			tpc_call(LOOP_0, tpc_lp0_args,
						pn, fixed, TPC_INOUT_ARG,
						pl, fixed, TPC_IN_ARG,
						pr, fixed, TPC_IN_ARG,
						tl, constD16, TPC_IN_ARG,
						tr, constD16, TPC_IN_ARG,
						local_ds, tpc_ds_size, TPC_IN_ARG);
				pn += distance;
				pl += distance;
				pr += distance;
				--limit;
			}
			//			printf("prepearing to wait.\n");
			//tpc_wait_all();
			//			printf("returned from wait.\n");
		}
		time_lp0 += (__mftb() - tmp_t_lp0);
	}
	else if( node->nodetype == ROOT_NODE && node->parent)
	{

                int idx=0;
		left = node->left;
		calcTransitionProbability( model, left->brlen, left->trprob);
		ComputeNodeLikeData( left, model );

		right = node->right;
		calcTransitionProbability( model, right->brlen, right->trprob);
		ComputeNodeLikeData( right, model );

		parent = node->parent;
		calcTransitionProbability( model, parent->brlen, parent->trprob);
		ComputeNodeLikeData( parent, model );

	    	tmp_t_lp1 = __mftb();

		tpc_wait_all();
		pn = node->siteLike;
		pl = left->siteLike;
		tl = left->trprob;
		pr = right->siteLike;
		tr = right->trprob;
		pp = parent->siteLike;
		tp = parent->trprob;

		if( !use_SPEs ){
//			ds[0]->vcted = vectorized;
//			ds[0]->start = 0;
			ds[0]->end = g_ds.nPattern;
			ppu_loop1(pn, pl, pr, pp, tl, tr, tp, ds[0]);
		}
		else{
//	    		printf("Trying to execute loop : 1");
			limit = lp1p.full_iterations;
			fixed = lp1p.full_fixed;
			distance = lp1p.full_distance;
			local_ds = ds1[0];

			while(limit > 0){
				#ifdef ALIGNMENT_ASSERTIONS_ENABLED
				set_aligned(lp1_at, 0 , pn, fixed);
				set_aligned(lp1_at, 1 , pl, fixed);
				set_aligned(lp1_at, 2 , pr, fixed);
				set_aligned(lp1_at, 3 , pp, fixed);
				set_aligned(lp1_at, 4 , tl, constD16);
				set_aligned(lp1_at, 5 , tr, constD16);
				set_aligned(lp1_at, 6 , tp, constD16);
				set_aligned(lp1_at, 7 , local_ds, tpc_ds_size);
				set_debug_info(lp1_at, limit, " Loop 1 [Full] ") ;

				assert_aligned(lp1_at);
				#endif

				tpc_call(LOOP_1, tpc_lp1_args,
						pn, fixed, TPC_INOUT_ARG,
						pl, fixed, TPC_IN_ARG,
						pr, fixed, TPC_IN_ARG,
						pp, fixed, TPC_IN_ARG,
						tl, constD16, TPC_IN_ARG,
						tr, constD16, TPC_IN_ARG,
						tp, constD16, TPC_IN_ARG,
						local_ds, tpc_ds_size, TPC_IN_ARG);
				pn += distance;
				pl += distance;
				pr += distance;
				pp += distance;
				--limit;
			}
			limit = lp1p.mod_iterations;
			fixed = lp1p.mod_fixed;
			distance = lp1p.mod_distance;
			local_ds = ds1[1];
			while(limit > 0){
				#ifdef ALIGNMENT_ASSERTIONS_ENABLED
				set_aligned(lp1_at, 0 , pn, fixed);
				set_aligned(lp1_at, 1 , pl, fixed);
				set_aligned(lp1_at, 2 , pr, fixed);
				set_aligned(lp1_at, 3 , pp, fixed);
				set_aligned(lp1_at, 4 , tl, constD16);
				set_aligned(lp1_at, 5 , tr, constD16);
				set_aligned(lp1_at, 6 , tp, constD16);
				set_aligned(lp1_at, 7 , local_ds, tpc_ds_size);
				set_debug_info(lp1_at, limit, " Loop 1 [Mod] ") ;

				assert_aligned(lp1_at);
				#endif

				tpc_call(LOOP_1, tpc_lp1_args,
						pn, fixed, TPC_INOUT_ARG,
						pl, fixed, TPC_IN_ARG,
						pr, fixed, TPC_IN_ARG,
						pp, fixed, TPC_IN_ARG,
						tl, constD16, TPC_IN_ARG,
						tr, constD16, TPC_IN_ARG,
						tp, constD16, TPC_IN_ARG,
						local_ds, tpc_ds_size, TPC_IN_ARG);
				pn += distance;
				pl += distance;
				pr += distance;
				pp += distance;
				--limit;
			}
		}
	    	time_lp1 += (__mftb() - tmp_t_lp1);
	}
}


void dumpSiteLikeData( PhyloTree *T );
void checkWeight()
{
	int		countPattern	= 0,
	numPattern		= g_ds.nPattern,
	*weight			= g_ds.compressedWeight.v;

	printf("Check Weight\n%5s %5s\n", "index", "weight");
	for( countPattern=0; countPattern<numPattern; countPattern++)
	{
		printf("%-5d %5d\n", countPattern, weight[countPattern] );
	}
	printf("\n\n");
}

void checkSiteLike(BinaryTreeNode *node, int low, int top)
{
	int		countPattern	= 0,
	numPattern		= g_ds.nPattern;
	double	*sitelike		= node->siteLike;

	//printf("Check SiteLike of Node #%d\n%5s %10s\n", node->index, "index", "sitelike");
	//printf("brlen=%.4f\n", node->brlen);
	//printf("trprob=%10.2e %10.2e %10.2e %10.2e %10.2e %10.2e %10.2e %10.2e %10.2e %10.2e %10.2e %10.2e %10.2e %10.2e %10.2e %10.2e\n",
	//	node->trprob[0], node->trprob[1], node->trprob[2], node->trprob[3],
	//	node->trprob[4], node->trprob[5], node->trprob[6], node->trprob[7],
	//	node->trprob[8], node->trprob[9], node->trprob[10], node->trprob[11],
	//	node->trprob[12], node->trprob[13], node->trprob[14], node->trprob[15]);

	for( countPattern=low; countPattern<=top; countPattern++)
	{
		sitelike = node->siteLike + 4 * countPattern;
		printf("NODE[%5d](%5d %5d %5d)   %-5d   %c  %10.2e   %10.2e   %10.2e   %10.2e\n",
			node->index,
			node->left?node->left->index:-1,
			node->right?node->right->index:-1,
			node->parent?node->parent->index:-1,
			countPattern,
			(node->nodetype == LEAF_NODE) ?
			g_ds.data.data[ node->index ][ g_ds.compressedPattern.v[ countPattern ] ]:
			'*',
			sitelike[0],
			sitelike[1],
			sitelike[2],
			sitelike[3]);
	}
	//if( node->left )
	//	checkSiteLike( node->left);
	//if( node->right )
	//	checkSiteLike( node->right);
	//if( node->parent )
	//	checkSiteLike( node->parent);
}

void checkTreeLike(BinaryTreeNode *node, int low, int top)
{
	checkSiteLike( node, low, top );
	if( node->left )
		checkTreeLike( node->left, low, top );
	if( node->right )
		checkTreeLike( node->right, low, top );
	if( node->nodetype == ROOT_NODE && node->parent )
		checkTreeLike( node->parent, low, top );
}

double ComputeLikelihood(MarkovState *state)
{
	int			iPattern, iState, *weight, iNode;
	double			*sroot, *freq, temp;
	PhyloTree		*tree	= &(state->tree);
	Model			*model	= &(state->model);

	struct Flag{
		unsigned value : 1;
	}flag;
	unsigned char wait_stop;
	register unsigned distance, fixed, sroot_fixed, i;
	register int limit;
	int total = g_ds.nPattern;
	long double *tmpp = NULL;//, *tmp_lnL = NULL;
	double lnL;
	struct tpc_ds *local_ds;
	//	unsigned titer = lp2p.full_iterations+lp2p.mod_iterations;

#ifdef ALIGNMENT_ASSERTIONS_ENABLED
	assertion_table_p lp2_at = NULL;
#endif

	//	tmp_lnL = c_malloc(titer*sizeof(long double));
	//	assert(tmp_lnL != NULL);

	ComputeNodeLikeData( tree->root, model );


	sroot		 = tree->root->siteLike;
	freq		 = model->daStateFreqs;
	weight		 = g_ds.compressedWeight.v;
	lnL		 = 0.0;

#ifdef ALIGNMENT_ASSERTIONS_ENABLED
	lp2_at = create_aligned(5);

	init_aligned(lp2_at, 0 , "tmpp");
	init_aligned(lp2_at, 1 , "sroot");
	init_aligned(lp2_at, 2 , "freq");
	init_aligned(lp2_at, 3 , "ds");
	init_aligned(lp2_at, 4 , "weight");
#endif

	tmp_t_lp2 = __mftb();
        tpc_wait_all();

	if( !use_SPEs ){
		ds[0]->vcted = vectorized;
		ds[0]->start = 0;
		ds[0]->end = g_ds.nPattern;
		ppu_loop2(&lnL, sroot, freq, ds[0], weight);
		tree->lnL = -(double)lnL;
	}
	else{
//	    	printf("Trying to execute loop : 2");
		limit = lp2p.full_iterations;
		fixed = lp2p.full_fixed;
		distance = lp2p.full_distance;
		sroot_fixed = lp2p.full_sroot_fixed;
		local_ds = ds2[0];
		tmpp = &tmp_lnL[0];
		while(limit > 0){
			#ifdef ALIGNMENT_ASSERTIONS_ENABLED
			set_aligned(lp2_at, 0 , tmpp, constLD1);
			set_aligned(lp2_at, 1 , sroot, sroot_fixed);
			set_aligned(lp2_at, 2 , freq, constD4);
			set_aligned(lp2_at, 3 , local_ds, tpc_ds_size);
			set_aligned(lp2_at, 4 , weight, fixed);
			set_debug_info(lp2_at, limit, " Loop 2 [Full] ") ;

			assert_aligned(lp2_at);
			#endif

			tpc_call(LOOP_2, tpc_lp2_args,
					tmpp, constLD1, TPC_INOUT_ARG,
					sroot, sroot_fixed, TPC_IN_ARG,
					freq, constD4, TPC_IN_ARG,
					local_ds, tpc_ds_size, TPC_IN_ARG,
					weight, fixed, TPC_IN_ARG);
			weight += distance;
			sroot += fixed;
			tmpp++;
			--limit;
		}
		limit = lp2p.mod_iterations;
		fixed = lp2p.mod_fixed;
		distance = lp2p.mod_distance;
		sroot_fixed = lp2p.mod_sroot_fixed;
		local_ds = ds2[1];
		while(limit > 0){
			#ifdef ALIGNMENT_ASSERTIONS_ENABLED
			set_aligned(lp2_at, 0 , tmpp, constLD1);
			set_aligned(lp2_at, 1 , sroot, sroot_fixed);
			set_aligned(lp2_at, 2 , freq, constD4);
			set_aligned(lp2_at, 3 , local_ds, tpc_ds_size);
			set_aligned(lp2_at, 4 , weight, fixed);
			set_debug_info(lp2_at, limit, " Loop 2 [Mod] ") ;

			assert_aligned(lp2_at);
			#endif

			tpc_call(LOOP_2, tpc_lp2_args,
					tmpp, constLD1, TPC_INOUT_ARG,
					sroot, sroot_fixed, TPC_IN_ARG,
					freq, constD4, TPC_IN_ARG,
					local_ds, tpc_ds_size, TPC_IN_ARG,
					weight, fixed, TPC_IN_ARG);
			weight += distance;
			sroot += fixed;
			tmpp++;
			--limit;
		}
		if(lp2p.ppe_iterations){
			double tmp_local = 0.0;
			ppu_loop2(&tmp_local, sroot, freq, ds_ppe, weight);
			*tmpp = tmp_local;
		}
		tpc_wait_all();
		for(i=0; i<total_lp2_sgms; i++) {
			lnL += (double)tmp_lnL[i];
		}
		tree->lnL = -lnL;

#ifdef DEBUG_L1_LP2
		printf("lnL result of loop 2 : %lf\n", tree->lnL);
#endif

//		c_free(tmp_lnL);
	}

	time_lp2 += (__mftb() - tmp_t_lp2);

	return lnL;
}

void dumpSiteLikeData( PhyloTree *T )
{
	int i, j;

	for(i=0; i<T->nnodes; i++)
	{
		printf("NODE=%2d (Site=%2d)\t", T->nodes[i].index, (T->nodes[i].siteLike - g_likedata)/(4 * g_ds.nPattern));
		for(j=0; j<g_ds.nPattern; j++)
		{
			if( j>79 && j<=82 )
				printf("[%8.2e %8.2e %8.2e %8.2e] ",
						T->nodes[i].siteLike[4*j + 0],
						T->nodes[i].siteLike[4*j + 1],
						T->nodes[i].siteLike[4*j + 2],
						T->nodes[i].siteLike[4*j + 3]);
		}
		printf("\n");
	}
}

