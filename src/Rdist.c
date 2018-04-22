//============================================================
// Random Number Generation
//

#include <R.h>
#include <Rmath.h>
#include <R_ext/Arith.h>
#include "RCutils_include/Rmat.h"

void rmvnorm(double* m, double* cholV, int dim, double* z, double* out){
/*************************************************************
 * PURPOSE:
 * Generate a draw from a multivariate normal distribution.
 *
 * INPUTS:
 * m     = an array for the mean
 * cholV = an array for the cholesky decomposition
 *         of the variance (note this is a 1D array that
 *         that will be treated as 2D).
 * dim   = dimension of the multivariate normal distribution
 * z     = a scratch array of length dim
 *         to be filled with N(0,1) draws
 *
 * OUTPUTS:
 * out   = final output array to be filled with a random
 *         multivariate normal draw
 *
 *************************************************************/
    int i,j;
    for (i=0; i<dim; i++){
	z[i] = rnorm(0,1);
	out[i] = m[i];
	for (j=0; j<=i; j++){
	    out[i] += cholV[i*dim + j]*z[j];
	}
    }
}


void rwish(int nu, double* cholS, int dim, double* z, double* x, double* zeros, double* out){
/*************************************************************
 * PURPOSE:
 * Generate a random draw from Wishart distribution with
 * degrees of freedom nu and parameter matrix S
 *
 * INPUTS:
 * nu    = degrees of freedom
 * cholS = cholesky decomposition of matrix parameter S
 *         This is a 1D array but is accessed
 *         as a 2D array as in cholS[i*dim + j]
 * dim   = the dimension of the Wishart distribution
 * z     = scratch vector of length dim to be passed
 *         to the function rmvnorm
 * x     = scratch vector of length dim to be passed
 *         to the function rmvnorm
 * zeros = vector of zeros of length dim to be passed
 *         to the function rmvnorm as the mean
 * out   = matrix to contain the random draw from the
 *         Wishart distribution
 *
 *************************************************************/

    int i, j, k;

    /* Zero the "out" matrix */
    for (j=0; j<dim; j++)
	for (k=0; k<dim; k++)
	    out[j*dim + k] = 0.0;

    for (i=0; i<nu; i++){
	rmvnorm(zeros,cholS,dim,z,x);
	for (j=0; j<dim; j++)
	    for (k=0; k<=j; k++)
		out[j*dim + k] += x[j]*x[k];
    }

    /* fill the upper triangular part with lower triangular part */
    for (j=0; j<dim; j++)
	for (k=0; k<j; k++)
	    out[k*dim + j] = out[j*dim + k];
}

double rtnorm(double m, double s, double a, double b){
    /* PURPOSE:
     * Sample from a univariate truncated random
     * normal distribution. Algorithm from Robert (1995).
     *
     * INPUTS:
     * m = location parameter
     * s = scale parameter
     * a = lower limit. This can be negative inifinity
     *     if the value is the special R value "R_NegInf".
     *     (See the Writing R Extensions for more details.)
     * b = upper limit. This can be positive infinity
     *     if the value is the special "R_PosInf"
     *
     * OUTPUT:
     * a random truncated normal draw
     *
     * REFERENCES:
     * Robert, C. P. (1995) "Simulation of truncated normal variables"
     * Statistics and Computing, 5(2), 121-125.
     */


    double x=0.0, z=0.0, alstar=0.0, rho=0.0, u=1.0, diff=0.0;

    /****************************************************
     * Both limits INFINITE
     ****************************************************/
    if (!R_FINITE(a) && !R_FINITE(b)){
	x = rnorm(m,s);
	return x;
    }

    /****************************************************
     * Both limits finite
     ****************************************************/
    if (R_FINITE(a) && R_FINITE(b)){
	a = (a - m)/s;
	b = (b - m)/s;
	if (b < 0.0){
	    while (u > rho){
		z = runif(a, b);
		rho = exp(0.5*(b*b - z*z));
		u = runif(0.0, 1.0);
	    }
	}
	else if (a > 0.0) {
	    while (u > rho){
		z = runif(a, b);
		rho = exp(0.5*(a*a - z*z));
		u = runif(0.0, 1.0);
	    }
	}
	else {
	    while (u > rho){
		z = runif(a, b);
		rho = exp(-0.5*z*z);
		u = runif(0.0, 1.0);
	    }
	}
	x = z*s + m;
	return x;
    }

    /****************************************************
     * Only one limit finite
     ****************************************************/
    if (R_FINITE(b)){
	a = -b;
	m = -m;
    }

    /* If limit less than mean use
     * simple rejection sampling
     * else use Robert's algorithm */
    if (a < m) {
	x = a;
	while (x <= a) {
	    x = rnorm(m, s);
	}
    }
    else {
	a = (a - m)/s;
	alstar = 0.5*(a + sqrt(a*a + 4));
	while (u > rho){
	    z = a + rexp(1.0/alstar);
	    diff = z - alstar;
	    rho = exp(-0.5*diff*diff);
	    u = runif(0.0,1.0);
	}
	x = z*s + m;
    }
    if (R_FINITE(b)) {
	x=-x;
    }
    return x;
}

//============================================================
// Multivariate density functions
//

//Multivariate normal
double dmvnorm(double* y, double* mu, double* iSig, int dim, double ld, double* scr, int logout){
    int i;
    double qf, out;
    for(i=0; i<dim; i++)
	scr[i] = y[i] - mu[i];
    qf = quform(scr, iSig, dim);
    out = -(double) dim*M_LN_SQRT_2PI - 0.5*(ld + qf);
    if (logout)	return out;
    return exp(out);
}
