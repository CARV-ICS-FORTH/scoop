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

#include <assert.h>
#include <math.h>
#include "model.h"
#include "util.h"
#include "pbpi.h"
#include "error.h"


/*
 * initModel
 * Pramaters:
 *		model:		the model to be initilized
 *		modeType:	one of the supported model type {JC69, K2P, HKY, GTR}
 *		
 */
void initModel(Model *model, ModelType modelType)
{
	int i;
	double dTmp;

	assert( model );

	model->type = modelType;
	model->nStates = 4;		//only deal with DNA/RNA data here

	//set state frequencies
	dTmp = 1.0 / model->nStates;
	for(i=0; i<model->nStates; i++)
		model->daStateFreqs[i] = dTmp;

	//set substitution parameters
	model->nSubstitutionParameters = 1;
	model->dTransitionTranversionRatio = 0.5;
	for(i=0; i<6; i++)
		model->daRevMatrix[i] = 1.0;
	
	switch( modelType )
	{
	case JC69:
		break;

	case K2P:
		model->nSubstitutionParameters  = 2;
		model->dTransitionTranversionRatio = 1.0;
		break;

	case HKY:
		model->nSubstitutionParameters  = 2;
		model->dTransitionTranversionRatio = 1.0;
		break;

	case GTR:
		model->nSubstitutionParameters  = 6;
		break;
	default:
		break;		//same as JC69
	}
}

void copyModel(Model *pDestModel, Model *pSrcModel)
{
	int i;
	pDestModel->type	=		pSrcModel->type;
	pDestModel->dTransitionTranversionRatio = pSrcModel->dTransitionTranversionRatio;
	pDestModel->nSubstitutionParameters = pSrcModel->nSubstitutionParameters;
	pDestModel->nStates	=		pSrcModel->nStates;
	for(i=0; i<6; i++)
		pDestModel->daRevMatrix[i] = pSrcModel->daRevMatrix[i];
	for(i=0; i<pSrcModel->nStates; i++)
		pDestModel->daStateFreqs[i] = pSrcModel->daStateFreqs[i];
}

ModelType getModelType(char *name)
{
	if ( tokenMatch(name, "JC69", FALSE) )
		return JC69;
	else if ( tokenMatch(name, "K2P", FALSE) )
		return K2P;
	else if ( tokenMatch(name, "HKY", FALSE) )
		return HKY;
	else if ( tokenMatch(name, "GTR", FALSE) )
		return GTR;
	else
		return JC69;
}

//effects:	matrix p is filled with transition probability
void calcTransitionProbability(
							   Model   *m,	//evolutionary model
							   double  brlen,	//time
							   double *p	//transition probability matrix
							   )
{
	int i;
	//for(i=0; i<16; i++)
	//	p[i] = 1.0;
	//return;
	double tmp, tmp1, tmp2;
	switch(m->type)
	{
	case JC69:
		tmp = exp(-4.0/3.0*brlen);
		tmp1 = 0.25 - 0.25 * tmp;	//p(i,j) i!=j
		tmp2 = 0.25 + 0.75 * tmp;	//p(i,i) i==j
		p[0] =  p[5]  = p[10] = p[15] = tmp2;
		p[1] =  p[2]  = p[3]  = tmp1;
		p[4] =  p[6]  = p[7]  = tmp1;
		p[8] =  p[9]  = p[11] = tmp1;
		p[12] = p[13] = p[14] = tmp1;
		break;

	case K2P:
		break;
	case HKY:
		break;
	case GTR:
		break;
	default:
		break;
	}
	return;
}

