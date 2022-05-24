/******************************************************************************/
/* Program: randnorm.c
   By: Brad Duthie                                             
   Description: Will produce one norm or pois random number
   Compile: gcc randnormINT.c -ansi -Wall -pedantic                           */
/******************************************************************************/

#include <stdio.h>     /* Standard input/output */
#include <stdlib.h>    /* Standard library */
#include <math.h>      /* Math library (for log and sqrt) */
#include "randunif.h" /* Need to generate random uniforms */

double randnorm(double mean, double sd){
    double x1, x2, w, y;    
    do{
        x1 = 2.0 * randunif() - 1;
        x2 = 2.0 * randunif() -1;
        w  = x1*x1 + x2*x2;
    } while(w >= 1.0);
    w = sqrt((-2.0 * log(w)) / w);
    y = x1 * w;
    y = y * sd;
    y = y + mean;
    return y; 
}


int randpois(double lambda){
    double L, u, p;
    int k;

    p = 1;
    k = 0;
    L = exp(-1*lambda);
    do{
        k++;
        u = randunif();
        p = p * u;
    }while(p > L);
    
    return k-1;
}
