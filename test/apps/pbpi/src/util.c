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
#include <stdarg.h>
#include <unistd.h>
#include "util.h"
#include "error.h"
#include "align_alloc.h"

void str2upper(char *str)
{
	int i;
	char c;
	for(i=0; str[i]!='\0'; i++)
	{
		c = str[i];
		if(c>='a' && c<='z')
			str[i] = c - 'a' + 'A';
	}
}

void str2lower(char *str)
{
	int i;
	char c;
	for(i=0; str[i]!='\0'; i++)
	{
		c = str[i];
		if(c>='A' && c<='Z')
			str[i] = c - 'A' + 'a';
	}
}

int  tokenMatch(char *tokA, char *tokB, int ignoreCase)
{
	if ( ignoreCase ){
		str2lower(tokA);
		str2lower(tokB);
	};
	return (strcmp(tokA, tokB) == 0);
}

//requires: dest must be allocated maxsize + 1 characters
int  tokenSafeCopy(char *dest, int maxsize, char *src)
{
	int i;
	for(i=0; i<maxsize && (*src); i++)
	{
		dest[i] = *src++;
	}
	dest[i] = '\0';
	return i;
}

void initCMatrix( CMatrix *pCMatrix )
{
	pCMatrix->__data = 0;
	pCMatrix->data   = 0;
	pCMatrix->flag	= -1;
	pCMatrix->ncols = 0;
	pCMatrix->nrows = 0;
}

int allocCMatrix( CMatrix *pCMatrix, int nrows, int ncols)
{
	int i;

	assert( pCMatrix );	//check the pointer

	/*freeCMatrix( pCMatrix );*/

	pCMatrix->flag = -1;
	if (nrows<=0 || ncols<=0){
		warning("wrong parameters for allocCMatrix: nrow=%d ncol=%d", nrows, ncols);
		return -1;
	}
	pCMatrix->nrows = nrows;
	pCMatrix->ncols = ncols;

	pCMatrix->__data = (char *)c_malloc( sizeof(char) * nrows * (ncols+1) );
	if ( !pCMatrix->__data ){
		fatal("can not allocate memory in allocCMatrix");
		return -1;
	}

	pCMatrix->data = (char **)c_malloc( sizeof(char *) * nrows );
	if ( !pCMatrix->data ){
		fatal("can not allocate memory in allocCMatrix");
		return -1;
	}

	pCMatrix->data[0] = pCMatrix->__data;
	pCMatrix->data[0][0] = '\0';
	for(i=1; i<nrows; i++){
		pCMatrix->data[i] = pCMatrix->data[i-1] + (ncols + 1);
		pCMatrix->data[i][0] = '\0';
	}

	pCMatrix->flag = 1;	//allocated successfully

	return 0;
}

void freeCMatrix( CMatrix *pCMatrix )
{
	assert( pCMatrix );

	if ( pCMatrix ){
		if (pCMatrix->data){
			free(pCMatrix->data);
		}
		if (pCMatrix ->__data){
			free(pCMatrix->__data);
		}
	}
}

void initIMatric( IMatrix *pIMatrix )
{
	pIMatrix->__data = 0;
	pIMatrix->data   = 0;
	pIMatrix->flag	 = -1;
	pIMatrix->ncols = 0;
	pIMatrix->nrows = 0;
}

int allocIMatrix( IMatrix *pIMatrix, int nrows, int ncols)
{
	int i, *p;

	assert( pIMatrix );	//check the pointer

	pIMatrix->flag = -1;
	pIMatrix->__data = 0;
	pIMatrix->data   = 0;
	if (nrows<=0 || ncols<=0){
		debug("allocIMatrix: nrows = %d ncols = %d", nrows, ncols);
		warning("wrong parameters for allocIMatrix");
		return -1;
	}
	pIMatrix->nrows = nrows;
	pIMatrix->ncols = ncols;

	pIMatrix->__data = (int *)c_malloc( sizeof(int) * nrows * (ncols+1) );
	pIMatrix->data = (int **)c_malloc( sizeof(int *) * nrows );
	
	if ( !pIMatrix->__data || !pIMatrix->data ){
		fatal("can not allocate memory in allocIMatrix");
		return -1;
	}

	p = pIMatrix->__data;
	for(i=0; i<nrows; i++){
		p[0] = '\0';
		pIMatrix->data[i] = p;
		p = p + (ncols+1);
	}

	pIMatrix->flag = 1;	//allocated successfully

	return 0;
}

void freeIMatrix( IMatrix *pIMatrix )
{
	assert( pIMatrix );

	if ( pIMatrix ){
		if (pIMatrix->data){
			free(pIMatrix->data);
			pIMatrix->data = 0;
		}
		if (pIMatrix ->__data){
			free(pIMatrix->__data);
			pIMatrix->__data = 0;
		}
	}
}

void	initIVector( IVector *pIVector )
{
	pIVector->size = 0;
	pIVector->v    = 0;
}

void	allocIVector(IVector *iv, int size )
{
	int i;

	iv->size = size;
	iv->v = (int *)c_malloc(sizeof( int ) * size );
	if ( iv->v == NULL )
		fatal("allocIVector: can not allocate memory");

	for(i=0; i<iv->size; i++)
		iv->v[i] = 0;
}

void	freeIVector( IVector iv )
{
	if(iv.v)
	{
		free( iv.v );
		iv.v = 0;
	}
}
void	dumpIVector( IVector iv )
{
	int i;
	printf("iv[%d]=(%d", iv.size, iv.v[0]);
	for(i=1; i< iv.size; i++)
		printf(" %d", iv.v[i]);
	printf(")\n");
}


void	allocDVector(DVector *dv, int size )
{
	int i;

	dv->size = size;
	dv->v = (double *)c_malloc(sizeof( int ) * size );
	if ( dv->v == NULL )
		fatal("allocIVector: can not allocate memory");

	for(i=0; i<dv->size; i++)
		dv->v[i] = 0;
}

void freeDVector( DVector dv )
{
	if(dv.v)
		free( dv.v );
}
void	dumpDVector( DVector dv )
{
	int i;
	printf("dv[%d]=(%.4f", dv.size, dv.v[0]);
	for(i=1; i< dv.size; i++)
		printf(" %.4f", dv.v[i]);
	printf(")\n");
}

void tranposeCMatrix( CMatrix *pOrigional, CMatrix *pTransposed)
{
	int i, j;
	char **om = pOrigional->data;
	char **ot = pTransposed->data;

	if ( pOrigional->ncols != pTransposed->nrows ||
		pTransposed->ncols != pOrigional->nrows)
		bug("Tranpose CMatrix and Origional CMatrix does not match");
	for(i=0; i<pOrigional->nrows; i++)
	{
		for(j=0; j<pOrigional->ncols; j++)
			ot[j][i] = om[i][j];
	}
}

int dumpCMatrix( CMatrix *pCMatrix )
{
	int i, j;

	assert( pCMatrix );	//check the pointer

	if ( pCMatrix->flag !=1 ){
		error(" cmatrix is not allocated");
		return -1;
	}

	printf("dump Cmatrix\nnrows=%d\nncols=%d\ndata=[\n",
		pCMatrix->nrows, pCMatrix->ncols);

	for(i=0; i<pCMatrix->nrows; i++){
		for(j=0; j<pCMatrix->ncols; j++){
			putc(pCMatrix->data[i][j], stdout);
		}
		putc('\n', stdout);
	}
	printf("]\n");

	return 0;
}

int dumpIMatrix( IMatrix *pIMatrix )
{
	int i, j;

	assert( pIMatrix );	//check the pointer

	if ( pIMatrix->flag !=1 ){
		error(" IMatrix is not allocated");
		return -1;
	}

	printf("dump Cmatrix\nnrows=%d\nncols=%d\ndata=[\n",
		pIMatrix->nrows, pIMatrix->ncols);

	for(i=0; i<pIMatrix->nrows; i++){
		for(j=0; j<pIMatrix->ncols; j++){
			printf("%d ", pIMatrix->data[i][j]);
		}
		putc('\n', stdout);
	}
	printf("]\n");

	return 0;
}


void writelog(char *logfile, const char *fmt, ...)
{
	FILE *fplog, *fplock;
	static char *lockfile = "pbpilog.lock";
	static char logbuf[512];
	int	   ntry;

	//test lock
	for(ntry=0; ntry<5; ntry++)
	{
		if( access(lockfile, F_OK) )
			break;
	}

	//create lock
	fplock = fopen(lockfile, "w");
	fclose(fplock);

	//append log
	fplog = fopen(logfile, "a");
	if( fplog )
	{
		va_list	ap;
		va_start(ap, fmt);
		vsprintf(logbuf, fmt, ap);
		va_end(ap);
		fputs(logbuf, fplog);
		fclose(fplog);
	}

	//remove lock
	unlink( lockfile );
}

