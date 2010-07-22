#ifndef _PBPI_PROPOSAL_H
#define _PBPI_PROPOSAL_H

#include "tree.h"
double proposeTreeBrlen(PhyloTree *T);
void proposalTreeBSS2(PhyloTree *T);
void proposeNextTree( PhyloTree *pNextTree );
void proposeRerootSubTree(PhyloTree *T);
void proposeRerootPhyloTree(PhyloTree *T);
void proposeTBR(PhyloTree *T);


#endif

