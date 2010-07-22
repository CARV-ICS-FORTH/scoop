#ifndef _PBPI_LIKELIHOOD_H
#define _PBPI_LIKELIHOOD_H

#include "mcmc.h"
#include "tree.h"
#include "model.h"
#include "dataset.h"


typedef	double *SiteLike;		//the likelihood of each state for each site

void Character2SiteLike(char c, SiteLike p);
void buildLeafSiteLikeData( );
double ComputeLikelihood(MarkovState *state);
void checkSiteLike(BinaryTreeNode *node, int low, int top);

#endif //_PBPI_LIKELIHOOD_H

