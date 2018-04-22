//************************************************************
// Printing Matrices and Vectors
//
// The following functions
//
// Rprintvec, Rprintmat, RprintIvec, RprintImat,
//
// are modified versions of functions
// provided by Howard Seltman at the following web page:
// http://www.stat.cmu.edu/~hseltman/files/vmr.c
//
// I have modified the functions to work with R and to
// provide slightly modified output to suit my tastes.
//

#include <R.h>

// for doubles
void Rprintvec(char* title, double *v, int l){
    if (title!=NULL)
	Rprintf("%s\n",title);
    for (int i=0; i<l; i++)
	Rprintf("%f\n", v[i]);
    Rprintf("\n");
    return;
}

void Rprintmat(char* title, double **m, int nr, int nc){
    if (title!=NULL)
	Rprintf("%s\n", title);
    for (int i=0; i<nr; i++) {
	for (int j=0; j<nc; j++)
	    Rprintf("%f ", m[i][j]);
	Rprintf("\n");
    }
    Rprintf("\n");
    return;
}

// for integers
void RprintIvec(char* title, int* v, int n){
    if (title!=NULL)
	Rprintf("%s\n", title);
    for (int i=0; i<n; i++)
	Rprintf("%i\n", v[i]);
    Rprintf("\n");
    return;
}

void RprintImat(char* title, int** m, int nr, int nc){
    if (title!=NULL)
	Rprintf("%s\n", title);
    for (int i=0; i<nr; i++) {
	for (int j=0; j<nc; j++)
	    Rprintf("%i ", m[i][j]);
	Rprintf("\n");
    }
    Rprintf("\n");
    return;
}

// Print a vector as a matrix
void RprintVecAsMat(char* title, double *v, int nr, int nc){
    if (title!=NULL)
	Rprintf("%s\n", title);
    for (int i=0; i<nr; i++) {
	for (int j=0; j<nc; j++)
	    Rprintf("%f ", v[i*nc + j]);
	Rprintf("\n");
    }
    Rprintf("\n");
    return;
}


void ProgressBar(int c, int t){
    /* Prints a progress bar
     * INPUTS:
     * c = current iteration
     * t = total iterations
     * */
    int barlength = 50;
    int nstars = barlength*c/t;
    Rprintf("\r|");
    for (int j=1; j<=nstars; j++){
	Rprintf("*");
    }
    Rprintf("%-*s| %*i%%", barlength - nstars, "", 3, 100*c/t);
}

