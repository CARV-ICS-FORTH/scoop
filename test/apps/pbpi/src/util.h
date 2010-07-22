#ifndef _PBPI_UTIL_H
#define _PBPI_UTIL_H

typedef struct CMatrix
{
	int		flag;
	int		nrows;
	int		ncols;
	char	*__data;
	char	**data;
} CMatrix;

int allocCMatrix( CMatrix *pCMatrix, int nrows, int ncols);
void freeCMatrix( CMatrix *pCMatrix );
void initCMatrix( CMatrix *pCMatrix );
int dumpCMatrix( CMatrix *pCMatrix );

typedef struct IMatrix
{
	int		flag;
	int		nrows;
	int		ncols;
	int		*__data;
	int		**data;
} IMatrix;

int allocIMatrix( IMatrix *pIMatrix, int nrows, int ncols);
void freeIMatrix( IMatrix *pIMatrix );
void initIMatric( IMatrix *pIMatrix );
int dumpIMatrix( IMatrix *pIMatrix );

void tranposeCMatrix( CMatrix *pOrigional, CMatrix *pTransposed);

typedef struct
{
	int	size;
	int *v;
}IVector;

typedef struct
{
	int	size;
	double *v;
}DVector;

void	initIVector( IVector *iv );
void	allocIVector(IVector *iv, int size );
void	freeIVector( IVector iv );
void	dumpIVector( IVector iv );

void	allocDVector(DVector *dv, int size );
void	freeDVector( DVector dv );
void	dumpDVector( DVector dv );

void str2upper(char *str);
void str2lower(char *str);
int  tokenMatch(char *tokA, char *tokB, int ignoreCase);
int  tokenSafeCopy(char *dest, int maxsize, char *src);
void writelog(char *logfile, const char *fmt, ...);

#endif //PBPI_UTIL_H

