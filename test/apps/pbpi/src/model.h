#ifndef _PBPI_MODEL_H
#define _PBPI_MODEL_H

#include "util.h"

typedef enum ModelType {JC69, K2P, HKY, GTR} ModelType;

#define		NUM_BASE_STATES			4		
//here we only deal with DNA
//should be changed to handle others

//Now we only deal with HKY model
typedef struct Model
{
	ModelType	type;							//model type
	int			nStates;						//number of character states
	int			nSubstitutionParameters;		//number of substitution parameters
	double		daStateFreqs[NUM_BASE_STATES];	//State frequencies
	double		dTransitionTranversionRatio;	//Transition/Tranversion Ratio
	double		daRevMatrix[6];					//Transition Matrix
}Model;

void initModel(Model *model, ModelType modelType);
void copyModel(Model *pDestModel, Model *pSrcModel);
ModelType getModelType(char *name);
void calcTransitionProbability(
							   Model   *m,	//evolutionary model
							   double  t,	//time
							   double *p	//transition probability matrix
							   );

#endif

