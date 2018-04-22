//============================================================
// Allocating and Freeing Vectors and Matrices
//
// Note that the Matrix allocation functions allocate the
// memory for the entire matrix in a contiguous block.

#include <R.h>

//==================================================
// Vectors
double* R_Vector(int n){
    double* v;
    v = (double *) R_alloc(n,sizeof(double));
    return(v);
}

double* R_VectorInit(int n,double init){
    double* v;
    v = (double *) R_alloc(n,sizeof(double));
    for(int i=0; i<n; i++)
	v[i]=init;
    return(v);
}

/* For possible Calloc version
void R_FreeVector(double *v, int n){
    Free(v);
}
*/

//==================================================
// Matrices
double** R_Matrix(int nr, int nc){
    double** m;
    m = (double **) R_alloc(nr,sizeof(double*));
    *m = (double *) R_alloc(nr*nc,sizeof(double));
    for (int i=1; i<nr; i++)
	*(m+i) = *m + i*nc;
    return(m);
}

double** R_MatrixInit(int nr, int nc, double init){
    double** m;
    int i, j;
    m = (double **) R_alloc(nr,sizeof(double*));
    *m = (double *) R_alloc(nr*nc,sizeof(double));
    for (i=1; i<nr; i++)
	*(m+i) = *m + i*nc;
    for (i=0; i<nr; i++)
	for (j=0; j<nc; j++)
	    m[i][j] = init;
    return(m);
}

/* for possible Calloc versions
void R_FreeMatrix(double** m, int nr, int nc){
   Free(*m); // because it was allocated in a contiguous block
   Free(m);
//for (int i=nr-1; i>=nr; i--)
//Free(m[i]);
//Free(m);
}
*/

double** R_Data2Matrix(double* d, int nr, int nc){
    double** m;
    m = (double **) R_alloc(nr,sizeof(double*));
    for (int i=0; i<nr; i++)
	*(m+i) = d + i*nc;
    return(m);
}

int** R_Data2iMatrix(int* d, int nr, int nc){
    int** m;
    m = (int **) R_alloc(nr,sizeof(int*));
    for (int i=0; i<nr; i++)
	*(m+i) = d + i*nc;
    return(m);
}
