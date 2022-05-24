/******************************************************************************/
/* Program: randnorm.c
   By: Brad Duthie                                             
   Description: Will produce one norm or pois random number
   Compile: gcc randnormINT.c -ansi -Wall -pedantic                           */
/******************************************************************************/
#include "as183.h"

double randunif(void){
    int seed[3];
    double randun;
       seed[0] = rand();
       seed[1] = rand();
       seed[2] = rand();
       randun  = as183(seed); 
    return randun;
}

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

int randbinom(double pr){
    int val;
    double u;
    
    u = randunif();

    if(u < pr){
      val = 0;    
    }else{
      val = 1;
    }
    
    return val;
}


int randunifint(int min, int max){
    int val, diff;
    double u;

    diff = (max + 1) - min;
    u    = diff * randunif();
    val  = (int) floor(u) + min;

    return val;
}


