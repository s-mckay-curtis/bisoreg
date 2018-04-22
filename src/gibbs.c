/* Gibbs sampling algorithm for Bayesian isotonic
 * regression.  The code here implements the algorithm
 * described in
 *
 * Curtis, S. M. and Ghosh, S. K. (2009) "A variable selection
 * approach to monotonic regression with Bernstein
 * polynomials."
 *
 * The code here is copyright (c) 2009 S. McKay Curtis
 * and may be used under the terms of the GNU Public Licsense
 * version 2.0 or a later version which can be found at
 *
 * http://www.gnu.org/licenses/gpl.html
 *
 *  This code comes with no warranty.  Use at your own risk.
 *
 * */

#include <R.h>
#include <Rmath.h>
#include "RCutils_include/Rprint.h"
#include "RCutils_include/Rmem.h"
#include "RCutils_include/Rdist.h"

#ifndef MAX
#define MAX(a, b) ( ((a) > (b)) ? (a) : (b) )
#endif

void isogibbs(
    // MCMC parameters
    int *ntot,
    int *nthin,
    int *nburn,
    // Constants/Data
    int *n,
    int *kmax,
    double *y,
    double *wdata,
    // Parameters: u[1], ..., u[n]
    double *u,
    // Parameter: tausq
    double *tausq,
    double *atau,
    double *btau,
    // Parameter: p
    double *p,
    double *ap,
    double *bp,
    // Parameter: sigsq
    double *sigsq,
    double *asig,
    double *bsig,
    // Parameter: u0
    double *u0,
    double *m0,
    double *ssq0,
    // Parameters: gamma
    int *gam,
    // Results storage
    double *udraws,
    double *tausqdraws,
    double *pdraws,
    double *sigsqdraws,
    double *u0draws,
    int *gamdraws,
    double *dev){

    int i, j, k, ii, unz, sbiter=MAX(*ntot/50, 1);
    double ssqu, res, mn, a, b, zi, zbar,
	num, den, smzsq, bhatk,
	sigsqk, pg1, pg0, m, sigsqstar;

    double** W = R_Data2Matrix(wdata, *n, *kmax);
    double* z = (double *) R_alloc(*n, sizeof(double));

    ProgressBar(0, *ntot);
    GetRNGstate();

    for(ii=1; ii<=*ntot; ii++){

	// Update sigsq
	ssqu = 0;
	for(i=0; i<*n; i++){
	    mn = *u0;
	    for(j=0; j<*kmax; j++){
		mn+=u[j]*W[i][j];
	    }
	    res = y[i] - mn;
	    ssqu += (res*res);
	}
	a = *asig + (double)(*n)/2.0;
	b = 1.0/(*bsig + ssqu/2.0);
	*sigsq = 1.0/rgamma(a, b);

	// Update u0
	zbar=0.0;
	for(i=0; i<*n; i++){
	    mn = 0.0;
	    for(j=0; j<*kmax; j++){
		mn+=u[j]*W[i][j];
	    }
	    zi = y[i] - mn;
	    zbar+=zi;
	}
	zbar = zbar / (double)(*n);
	num = (*m0 / *ssq0) + ((double)(*n)*zbar) / *sigsq;
	den = (1.0 / *ssq0) + ((double)(*n) / *sigsq);
	*u0 = rnorm(num/den, sqrt(1.0/den));

	// Update gam_k and u_k
	for(k=0; k<*kmax; k++){

	    // Update gam_k
	    smzsq=0.0; bhatk=0.0; den=0.0;
	    for(i=0; i<*n; i++){
		mn=0.0;
		for(j=0; j<*kmax; j++){
		    if(j!=k) mn += W[i][j]*u[j];
		}
		z[i] = y[i] - *u0 - mn;
		smzsq += z[i]*z[i];
		bhatk += W[i][k]*z[i];
		den += W[i][k]*W[i][k];
	    }
	    bhatk = bhatk/den;
	    sigsqk = *sigsq/den;
	    m = *tausq*bhatk / (*tausq + sigsqk);
	    sigsqstar = sigsqk*(*tausq) / (*tausq + sigsqk);

	    ssqu = 0.0;
	    for(i=0; i<*n; i++){
		res = z[i] - bhatk*W[i][k];
		ssqu += res*res;
	    }
	    a = smzsq/(2.0*(*sigsq));
	    pg1 = *p;
	    b = ssqu/(2*(*sigsq)) + bhatk*bhatk/(2*sigsqk) - m*m/(2*sigsqstar);
	    pg0 = 2*sqrt(sigsqstar/(*tausq))*exp(a-b)*pnorm(0.0, m, sqrt(sigsqstar), 0, 0)*(1.0 - *p);
	    if(runif(0.0, 1.0) < (pg1/(pg0 + pg1))) gam[k] = 1;
	    else gam[k] = 0;

	    // Update u_k
	    if(gam[k]) u[k] = 0.0;
	    else u[k] = rtnorm(m, sqrt(sigsqstar), 0.0, R_PosInf);
	}

	// Update tausq
	ssqu=0.0; unz=0;
	for(k=0; k<*kmax; k++){
	    if(u[k]!=0.0){
		ssqu += u[k]*u[k];
		unz  += 1;
	    }
	}
	a = *atau + (double)unz/2.0;
	b = 1.0/(*btau + ssqu/2.0);
	*tausq = 1.0/rgamma(a, b);

        // Update p_gamma
	a = *ap + ((double)(*kmax - unz));
	b = *bp + ((double)unz);
	*p = rbeta(a, b);

	// Record Draws compute deviance
	if (((ii%*nthin)==0) && (ii>*nburn)){

	    // Compute deviance
	    *dev = 0.0;
	    for(i=0; i<*n; i++){
		mn = *u0;
		for(j=0; j<*kmax; j++){
		    mn += W[i][j]*u[j];
		}
		*dev += dnorm(y[i], mn, sqrt(*sigsq), 1);
	    }
	    *dev = -2.0*(*dev);
	    dev++;

	    // Record Draws
	    *u0draws = *u0;
	    u0draws++;

	    for (k=0; k<*kmax; k++){
		*udraws = u[k];
		*gamdraws = gam[k];
		udraws++;
		gamdraws++;
	    }

	    *pdraws = *p;
	    pdraws++;

	    *sigsqdraws = *sigsq;
	    sigsqdraws++;

	    *tausqdraws = *tausq;
	    tausqdraws++;

	}

	if(ii%sbiter==0)
	    ProgressBar(ii, *ntot);

    }

    PutRNGstate();

    Rprintf("\r");
}



