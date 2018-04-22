//============================================================
// Matrix Operations
//

double quform(double *x, double* A, int dim){
    /* PURPOSE:
     * Calculate x'Ax
     * */
    int i,j;
    double sm=0.0;
    for (i=1; i<dim; i++)
	for (j=0; j<i; j++)
	    sm += x[i]*x[j]*A[i*dim+j];
    sm*=2;
    for(i=0; i<dim; i++)
	sm += x[i]*x[i]*A[i*dim+i];
    return(sm);
}


double biform(double* x, double** A, double* y, int l){
    double sm=0.0;
    for (int i=0; i<l; i++)
	for (int j=0; j<l; j++)
	    sm += x[i]*A[i][j]*y[j];
    return(sm);
}

