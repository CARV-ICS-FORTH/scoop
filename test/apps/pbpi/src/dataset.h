#ifndef _PBPI_DATASET_H
#define _PBPI_DATASET_H

#include <stdio.h>
#include "util.h"
#include "pbpi.h"

#define		MAX_LABEL_LENGTH	31		//the default maximum length of the label

//define a SequenceDatatype
typedef enum SequenceDatatype {DNA, RNA} SequenceDatatype;

typedef struct Dataset
{
	int			flag,			//check if required data has been setup
				nTaxa,			//number of taxa
				nChar,			//number of characters
				nlen,			//the maximum length of the label
				nPattern,		//number of local patterns
				nTotalPatterns,	//number total patterns
				nPartPos[2];	//local psegment of data partition
				
	
	char		cMissing,		//character for missing chars
				cMatchchar,		//match character
				cGap;			//character for gap

	boolean		interleave;		//flag = 1; data is in interlevae format
								//flag = 0; data is in not interlevae format, i.e.
								//the next sequence comes after the finish of all 
								//characters of all current sequence

	CMatrix		data,			//store the data matrix
				label,			//store the taxa labels
				translate;		//store the taxa labels

	IVector		patternMap,		//store the map between Site and Pattern
				weight,			//store the weight of each pattern
				compressedPattern,	//store the compressed pattern
				compressedWeight;	//store the compressed pattern weight
	
	SequenceDatatype datatype;	//the datatype of the sequence
}Dataset;

int readDataset( Dataset *ds, FILE *in );
void writeDataset( char *filename, Dataset *pDS);
void writePartialDataset( char *filename, Dataset *pDS, int ntaxaStart, int ntaxaStop, int ncharStart, int ncharStop);
void dumpDatasetAndPattern( Dataset *pDS );
void compressDataset(Dataset *pData);
void dumpDataset( Dataset *pDS );
void freeDataset( Dataset *pData);

int		searchTaxaLabel(Dataset *pDataset, char *szLabel);
int		searchTranslate(Dataset *pDataset, char *szLabel);
boolean addTranslate(Dataset *pDataset, char *szTranslate, char *szLabel);

void resampleDataMatrix(
						Dataset *pDataset,	 //the original dataset
						double LengthRatio,  //how many sites will appear in the final dataset
						double ResampleRatio //how many sites will be sampled with replacement
						);

#endif

