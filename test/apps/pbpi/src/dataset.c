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

#include "dataset.h"
#include "token.h"
#include "util.h"
#include "pbpi.h"
#include "error.h"
#include "rand.h"

int skipSpace(FILE *in)
{
	char c;

	c = fgetc(in);
	while(c==' ' || c=='\n' || c=='\t'||c==13) c = fgetc(in);

	if(c==EOF){     return 0; }
	else{ ungetc(c, in);    return 1;}
}

char *CharSet="ACGTUMRWSYKBDHVN?O-acgtumrwsykbdhvno.";
boolean isLegalChar(char c)
{
	char *p=CharSet;
	while(*p!='\0')
	{
		if(*p++==c) 
			return TRUE;
	}
	return FALSE;
}

char WhiteSpace[]={' ', '\t', '\n', 13, '\0'};
boolean isWhiteSpace(char c)
{
	char *p=WhiteSpace;
	while(*p!='\0')
	{
		if(*p++==c) 
			return TRUE;
	}
	return FALSE;
}

int readSequenceOld(FILE *in, int nsite, char *buf)
{
	int c, i=0;

	skipSpace(in);

	while( i < nsite )
	{
		c=fgetc(in); if(c==EOF) break;
		if( isLegalChar((char)c))
		{       

			if( c>='a' && c<='z')
				c = 'A' + c - 'a';
			buf[i++]=c;
		}
	}
	buf[i]='\0';
	if(c==EOF) 
		return FALSE;
	else       
		return TRUE;
}

//read one whole sequence
int readSequence(FILE *in, int nsite, char *buf)
{
	int	c = 0,
		count = 0;
	
	for( count=0; count < nsite; )
	{
		//skip whitespace first
		c = fgetc( in );
		while( isWhiteSpace( c ) && c!=EOF )
			c = fgetc( in );
		
		//check if comments
		if ( c == '[' )
		{
			//if yes, skip the comments
			c = fgetc(in);
			while( c != ']' && c!=EOF )
				c = fgetc( in );
			continue;	//back to the begin of the for-loop
		}

		if( isLegalChar( c ) )
		{
			buf[ count ] = c;
			count++;
		}
		else if( c == EOF )
			break;
		else
		{
			debug("ILLEGAL CHARACTER [%c:%d] at column [%d]", c, count);
			break;
		}
	}
	buf[count] = '\0';

	if( count != nsite )
	{
		debug("read sequence failed");
		return FALSE;
	}
	else
	{
		return TRUE;
	}
}

int readDataset( Dataset *pDS, FILE *in )
{
	NEXUSToken *pNEXUSToken;
	int i;

	//step 0: init Dataset
	pDS->cGap = '-';
	pDS->cMissing = '?';
	pDS->cMatchchar = '.';
	pDS->datatype = DNA;
	pDS->flag = 0;
	pDS->interleave = FALSE;
	pDS->nChar = 0;
	pDS->nlen = 0;
	pDS->nTaxa = 0;
	freeCMatrix( &(pDS->data));
	freeCMatrix( &(pDS->label));

	//step 1: read the NEXUS flag
	pNEXUSToken = nextNEXUSToken( in );
	if( pNEXUSToken->type != TOK_STRING || !tokenMatch( pNEXUSToken->token, "#nexus", TRUE ) )
	{
		error("dataset is not in NEXUS format");
		goto dataset_error;
	}

	//step 2: read one block
	if ( searchNEXUSToken( in, "begin" ) )
	{
		pNEXUSToken = nextNEXUSToken( in );
		//2.1 read data block
		if ( tokenMatch( pNEXUSToken->token, "data", TRUE ) )
		{
			//2.1.1 find ; to find current command
			pNEXUSToken = nextNEXUSToken( in );
			if ( pNEXUSToken->type != TOK_SEMICOLON)
			{
				error("\';\' is expected");
				goto dataset_error;
			}

			while(1)
			{	
				//each iteration handle one nexus command
				pNEXUSToken = nextNEXUSToken( in );
				if( pNEXUSToken->type != TOK_STRING )
				{
					error("keyword is expected");
					goto dataset_error;
				}

				//handle dimension command
				if ( tokenMatch(pNEXUSToken->token, "dimensions", TRUE) )
				{
					while(1)
					{
						pNEXUSToken = nextNEXUSToken( in );
						//read ntax=value pair
						if( tokenMatch(pNEXUSToken->token, "ntax", TRUE) )
						{
							if( searchNEXUSToken(in, "=") )
							{
								pNEXUSToken = nextNEXUSToken( in );
								pDS->nTaxa = atoi( pNEXUSToken->token );
							}
							else
							{
								error("= is expected");
								goto dataset_error;

							}
						}
						//read nchar=value pair
						else if( tokenMatch(pNEXUSToken->token, "nchar", TRUE) )
						{
							if( searchNEXUSToken(in, "=") )
							{
								pNEXUSToken = nextNEXUSToken( in );
								pDS->nChar = atoi( pNEXUSToken->token );
							}
							else
							{
								error("= is expected");
								goto dataset_error;

							}
						}
						//finish current command
						else if ( tokenMatch(pNEXUSToken->token, ";", TRUE) )
						{
							break;
						}
						//error
						else
						{
							error("parameter %s in command dimensiosn is not supported", pNEXUSToken->token);
							goto dataset_error;

						}
					}
				}
				//handle command format
				else if ( tokenMatch(pNEXUSToken->token, "format", TRUE) )
				{
					while(1)
					{

						pNEXUSToken = nextNEXUSToken( in );

						//read datatype=<type>
						if( tokenMatch(pNEXUSToken->token, "datatype", TRUE) )
						{
							if( searchNEXUSToken(in, "=") )
							{
								pNEXUSToken = nextNEXUSToken( in );
								if( tokenMatch(pNEXUSToken->token, "dna", TRUE) )
								{
									pDS->datatype = DNA;
								}
								else
								{
									error("datatype %s in not supported in current version", pNEXUSToken->token);
									goto dataset_error;
								}						}
							else
							{
								error("= is expected");
								goto dataset_error;
							}
						}
						//read missing=<char>
						else if( tokenMatch(pNEXUSToken->token, "missing", TRUE) )
						{
							if( searchNEXUSToken(in, "=") )
							{
								pNEXUSToken = nextNEXUSToken( in );
								pDS->cMissing = pNEXUSToken->token[0];
							}
							else
							{
								error("= is expected");
								goto dataset_error;

							}
						}
						//read missing=<char>
						else if( tokenMatch(pNEXUSToken->token, "matchchar", TRUE) )
						{
							if( searchNEXUSToken(in, "=") )
							{
								pNEXUSToken = nextNEXUSToken( in );
								pDS->cMatchchar = pNEXUSToken->token[0];
							}
							else
							{
								error("= is expected");
								goto dataset_error;

							}
						}
						//read gap=<char>
						else if( tokenMatch(pNEXUSToken->token, "gap", TRUE) )
						{
							if( searchNEXUSToken(in, "=") )
							{
								pNEXUSToken = nextNEXUSToken( in );
								pDS->cGap = pNEXUSToken->token[0];
							}
							else
							{
								error("= is expected");
								goto dataset_error;
							}
						}
						else if( tokenMatch(pNEXUSToken->token, "interleave", TRUE) )
						{
							pDS->interleave = TRUE;

						}
						//finish current command
						else if ( tokenMatch(pNEXUSToken->token, ";", TRUE) )
						{
							break;
						}
						//error
						else
						{
							error("parameter %s in command format is not supported", pNEXUSToken->token);
							goto dataset_error;
						}
					}//end while loop for format
				}
				//handle command matrix
				else if ( tokenMatch(pNEXUSToken->token, "matrix", TRUE) )
				{

					allocCMatrix( &(pDS->data), pDS->nTaxa, pDS->nChar);

					pDS->nlen = MAX_LABEL_LENGTH;
					allocCMatrix( &(pDS->label), pDS->nTaxa, pDS->nlen);
					allocCMatrix( &(pDS->translate), pDS->nTaxa, pDS->nlen );

					//interleaved format
					if (pDS->interleave)
					{
						error("This version of PBPI only support non-interleaved data matrix", pNEXUSToken->token);
						goto dataset_error;
					}
					//non interleaved format
					else
					{
						for( i = 0; i < pDS->nTaxa; i++ )
						{
							pNEXUSToken = nextNEXUSToken( in );
							if ( pNEXUSToken->type == TOK_STRING )
								tokenSafeCopy(pDS->label.data[i], pDS->nlen, pNEXUSToken->token);
							else
							{
								error("\';\' is expected");
								goto dataset_error;
							}

							if ( !readSequence(in, pDS->nChar, pDS->data.data[i]))
							{
								error("Read %d(taxa=%s) Sequnce error", i, pDS->label.data[i]);
								goto dataset_error;
							}
							else
							{
								int j;
								for(j=0; j<pDS->nChar; j++)
								{
									if( pDS->data.data[i][j] == pDS->cMatchchar )
										pDS->data.data[i][j] = pDS->data.data[0][j];
								}
								//printf("[%5d]TAXA=(%20s) Data=(%s)\n",
								//	i,
								//	pDS->label.data[i],
								//	pDS->data.data[i]);

							}

						}
					}
					pDS->flag = 1;

					if ( searchNEXUSToken(in, ";") )
						break;
					else
					{
						error("\';\' is expected");
						goto dataset_error;
					}
				}
				//close current block
				else if ( tokenMatch(pNEXUSToken->token, "end", TRUE) )
				{
					if( searchNEXUSToken(in, ";") )
						break;
					else
					{
						error("\';\' is expected");
						goto dataset_error;
					}
				}
				else
				{
					error("command %s is not supported", pNEXUSToken->token);
					goto dataset_error;
				}
			}
		}
		else
		{
			error("data block is required");
			goto dataset_error;
		}
	}

	//writeDataset( "dataset.sav", pDS);
	//dumpDataset( pDS );
	return 1;

	//error processing
dataset_error:
	return 0;
}

void dumpDataset( Dataset *pDS )
{
	int i, j;
	printf("Dataset\nntaxa=%d nchar=%d npattern=%d\n", pDS->nTaxa, pDS->nChar, pDS->nPattern);
	for(i=0; i<pDS->nTaxa; i++)
	{
		printf("%10s\t", pDS->label.data[i]);
		for(j=0; j<10; j++)
			putchar( pDS->data.data[i][j]);
		putchar('\n');
	}
	printf("\n\n");
}

void writeDataset( char *filename, Dataset *pDS)
{
	FILE *fpOut;
	
	fpOut = fopen( filename, "w+" );
	if ( fpOut )
	{
		int	iTaxa;

		fprintf(fpOut, "#NEXUS\n");
		fprintf(fpOut, "Begin DATA;\n");
		fprintf(fpOut, "%4c dimensions ntax=%d nchar=%d;\n",
			' ', pDS->nTaxa, pDS->nChar);
		fprintf(fpOut, "%4c format missing=%c gap=%c datatype=DNA;\n",
			' ', pDS->cMissing, pDS->cGap);
		fprintf(fpOut, "%4c matrix\n", ' ');

		for(iTaxa = 0; iTaxa < pDS->nTaxa; iTaxa++)
		{
			fprintf(fpOut, "%20s%4c%s\n", 
				pDS->label.data[iTaxa],
				' ',
				pDS->data.data[iTaxa]);
		}
		fprintf(fpOut, "%4c ;\n", ' ');
		fprintf(fpOut, "END;\n");
		
		fclose( fpOut );
	}
	else
	{
		error("can not open file: %s", filename);
	}
}


void writePartialDataset( char *filename, Dataset *pDS, int ntaxaStart, int ntaxaStop, int ncharStart, int ncharStop)
{
	FILE *fpOut;
	
	fpOut = fopen( filename, "w" );
	if ( fpOut )
	{
		int	iTaxa, ichar;

		fprintf(fpOut, "#NEXUS\n");
		fprintf(fpOut, "Begin DATA;\n");
		fprintf(fpOut, "%4c Dimensions ntax=%d nchar=%d;\n",
			' ', min2(pDS->nTaxa, ntaxaStop - ntaxaStart + 1), min2(pDS->nChar, ncharStop-ncharStart+1) );
		fprintf(fpOut, "%4c Format missing=%c gap=%c datatype=DNA;\n",
			' ', pDS->cMissing, pDS->cGap);
		fprintf(fpOut, "%4c Matrix\n", ' ');

		for(iTaxa = ntaxaStart; iTaxa <= min2(pDS->nTaxa, ntaxaStop); iTaxa++)
		{
			fprintf(fpOut, "%-20s% ", pDS->label.data[iTaxa]);
			fprintf(stdout, "%-20s% ", pDS->label.data[iTaxa]);
			for( ichar = ncharStart; ichar <= min2(ncharStop, pDS->nChar); ichar++)
			{
				putc(pDS->data.data[iTaxa][ichar], stdout);
				fprintf(fpOut, "%c", pDS->data.data[iTaxa][ichar]);
			}
			fputc('\n', fpOut);				
			fputc('\n', stdout);				
		}
		fprintf(fpOut, "%4c ;\n", ' ');
		fprintf(fpOut, "END;\n");
		
		fclose( fpOut );
	}
	else
	{
		error("can not open file: %s", filename);
	}
}

void initDataset( Dataset *pDataset)
{
	pDataset->nTaxa = 0;
	pDataset->nChar = 0;
	pDataset->cGap  = '-';
	pDataset->cMissing = '?';
	pDataset->flag = -1;
	pDataset->interleave = FALSE;
	pDataset->nlen = 0;
	pDataset->nPattern = 0;
		
	initCMatrix( &(pDataset->data ) );
	initCMatrix( &(pDataset->label ) );
	initCMatrix( &(pDataset->translate) );
	initIVector( &(pDataset->compressedPattern) );
	initIVector( &(pDataset->compressedWeight) );
	initIVector( &(pDataset->weight) );
	initIVector( &(pDataset->patternMap) );
}

void freeDataset( Dataset *pData)
{
	freeCMatrix( &(pData->data) );
	freeCMatrix( &(pData->label) );
	freeCMatrix( &(pData->translate) );
	freeIVector( pData->compressedPattern );
	freeIVector( pData->compressedWeight );
	freeIVector( pData->patternMap );
	freeIVector( pData->weight );
}

int comparePattern( char  *base, char *p, int size)
{
	int	count;
	for(count = 0; count<size; count++)
	{
		if( p[count] != base[count] )
			return p[count] - base[count];
	}
	return 0;
}

void compressDataset(Dataset *pData)
{
	int i, j;

	CMatrix transposeData;
	int *vp, *vw, nchar, ntaxa;
	char **pattern;

	transposeData.__data	=	0;
	transposeData.data		=	0;

	freeIVector( pData->patternMap );
	allocIVector( &pData->patternMap, pData->nChar );

	freeIVector( pData->compressedPattern );
	allocIVector( &pData->compressedPattern, pData->nChar );	//allocate a large vector
	
	freeIVector( pData->compressedWeight );
	allocIVector( &pData->compressedWeight, pData->nChar );	//allocate a large vector

	freeIVector( pData->weight );
	allocIVector( &pData->weight, pData->nChar );	//allocate a large vector

	allocCMatrix( &transposeData, pData->nChar, pData->nTaxa );
	tranposeCMatrix( &pData->data, &transposeData );

	nchar = pData->nChar;
	ntaxa = pData->nTaxa;
	vp = pData->patternMap.v;
	for(i=0; i<nchar; i++)
		vp[i] = i;

	vw = pData->weight.v;
	for(i=0; i<nchar; i++)
		vw[i] = 1;

	//get all unique patterns
	pattern = transposeData.data;
	for(i=0; i<nchar; i++)
	{
		if ( vw[i] == 0 ) 
			continue;
		for(j=i+1; j<nchar; j++)
		{
			if ( vw[j] == 0 ) 
				continue;
			if (strncmp(pattern[i], pattern[j], ntaxa)==0)
			//if( comparePattern(pattern[i], pattern[j], pData->nTaxa) == 0)
			{
				vw[i] += vw[j];
				vw[j] = 0;
				vp[j] = vp[i];
			}
		}
	}
	freeCMatrix( &transposeData );

	//get compressed patterns
	for(i=0, j=0; i<nchar; i++)
	{
		if ( vw[i] > 0 ) 
		{
			pData->compressedPattern.v[j] = vp[i];
			pData->compressedWeight.v[j]  = vw[i];
			j++;
		}
	}
	pData->nPattern=j;
	pData->nTotalPatterns = pData->nPattern;

	for(i=pData->nPattern; i<nchar; i++)
	{
		pData->compressedPattern.v[i] = -1;
		pData->compressedWeight.v[i]  = 0;
	}
}

int		searchTaxaLabel(Dataset *pDataset, char *szLabel)
{
	int		index;

	for(index = 0; 	index < pDataset->nTaxa;	index++)
	{
		if ( tokenMatch(szLabel, pDataset->label.data[ index ], TRUE) )
			return index;
	}

	return -1;		//NOT FOUND
}

int		searchTranslate(Dataset *pDataset, char *szLabel)
{
	int		index;

	for(index = 0; 	index < pDataset->nTaxa;	index++)
	{
		if ( tokenMatch(szLabel, pDataset->translate.data[ index ], TRUE) )
			return index;
	}

	return -1;		//NOT FOUND
}

boolean addTranslate(Dataset *pDataset, char *szTranslate, char *szLabel)
{
	int		index;

	index = searchTaxaLabel( pDataset, szLabel);
	if( index >= 0)
	{
		tokenSafeCopy(  pDataset->translate.data[ index ], MAX_LABEL_LENGTH, szTranslate);
		return TRUE;
	}
	else
	{
		error("Taxa label (%s) is not found", szLabel);
		return FALSE;
	}
}

void dumpDatasetAndPattern( Dataset *pDS )
{
	int i, j;

	printf("Dataset\nntaxa=%d nchar=%d npattern=%d\n", pDS->nTaxa, pDS->nChar, pDS->nPattern);

	for(j=0; j<pDS->nChar*3+11; j++)
		putchar('-');
	printf("\n");


	for(i=0; i<pDS->nTaxa; i++)
	{
		printf("%10s ", pDS->label.data[i]);
		for(j=0; j<pDS->nChar; j++)
		{
			printf(" %c ", pDS->data.data[i][j]);
		}
		putchar('\n');
	}

	for(j=0; j<pDS->nChar*3+11; j++)
		putchar('-');
	printf("\n");
	printf("%10s ", "index");
	for(j=0; j<pDS->nChar; j++)
	{
		printf("%2d ", j);
	}
	printf("\n");
	printf("%10s ", "Pattern");
	for(j=0; j<pDS->nChar; j++)
	{
		printf("%2d ", pDS->patternMap.v[j]);
	}
	printf("\n");
	printf("%10s ", "Weight");
	for(j=0; j<pDS->nChar; j++)
	{
		printf("%2d ", pDS->weight.v[j]);
	}
	printf("\n");

	printf("%10s ", "Comp.P");
	for(j=0; j<pDS->nChar; j++)
	{
		printf("%2d ", pDS->compressedPattern.v[j]);
	}
	printf("\n");
	printf("%10s ", "Comp.W");
	for(j=0; j<pDS->nChar; j++)
	{
		printf("%2d ", pDS->compressedWeight.v[j]);
	}
	printf("\n");

}



void resampleDataMatrix(
						Dataset *pDataset,	 //the original dataset
						double LengthRatio,  //how many sites will appear in the final dataset
						double ResampleRatio //how many sites will be sampled with replacement
						)
{
	int	ichar,
		ichar1,
		jchar,
		temp,
		numDeleted,
		numRemain,
		numKeeped,
		*p,
		*p0,
		*w,
		*w0;


	if( LengthRatio > 1.0 ) 
		LengthRatio = 1.0;
	else if ( LengthRatio <= 0.1 ) 
		LengthRatio = 0.1;

	numDeleted = pDataset->nChar * ( 1.0 - LengthRatio ) + 0.5;
	numRemain  = pDataset->nChar - numDeleted;
	p = pDataset->compressedPattern.v;
	w = pDataset->compressedWeight.v;
	w0 = pDataset->weight.v;
	p0 = pDataset->patternMap.v;
	for(ichar = 0; ichar < pDataset->nChar; ichar++)
	{
		p[ichar] = ichar;
		w[ichar] = 0;
		w0[ichar] = 0;
	}

	//knifing
	for(ichar = 0, ichar1 = pDataset->nChar-1; ichar < numDeleted; ichar++, ichar1--)
	{
		jchar = irand(0, ichar1);
		temp      = p[ichar1];
		p[ichar1] = p[jchar];
		p[jchar]  = temp;
		w[ichar1] = -1;
	}
	
	//resampling
	if( ResampleRatio > 1.0 )
		ResampleRatio = 1.0;
	else if (ResampleRatio < 0.0)
		ResampleRatio = 0.0;
	numKeeped = numRemain * ( 1.0 - ResampleRatio ) + 0.5;
	//debug("ratio=%.2f keepd=%d\n", ResampleRatio, numKeeped);
	//copy unchanged part
	for(ichar = 0, ichar1 = 0; ichar < numKeeped; ichar++, ichar1++)
	{
		
		jchar = irand(ichar1, numKeeped-1);
		//debug("choose %d", jchar);
		temp      = p[ichar1];
		p[ichar1] = p[jchar];
		p[jchar]  = temp;
		w[ichar1] = 1;
	}

	//sampling with replacement
	for(ichar = 0; ichar < numRemain-numKeeped; ichar++)
	{
		jchar = irand(0, numRemain-1);
		w[jchar]++;
	}

	for(ichar = 0; ichar < numRemain; ichar++)
	{
		w0[ p0[ p[ ichar ] ] ] += w[ichar];
	}
	
	//now comparess
	//now compress the pattern
	for(ichar=0, jchar=0; ichar<pDataset->nChar; ichar++)
	{
		if ( w0[ichar] > 0 ) 
		{
			p[jchar] = p0[ichar];
			w[jchar] = w0[ichar];
			jchar++;
		}
	}
	pDataset->nPattern=jchar;
	pDataset->nTotalPatterns = pDataset->nPattern;

	for(ichar=pDataset->nPattern; ichar<pDataset->nChar; ichar++)
	{
		p[ichar] = -1;
		w[ichar] = 0;
	}
}
