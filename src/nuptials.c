/******************************************************************************/
/* Program: nuptials.c
   By: Brad Duthie                                             
   Description: IBM to simulate evolution of nuptial gift giving
   Compile: gcc nuptials.c -ansi -Wall -pedantic                              */
/******************************************************************************/
#include <time.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "rand_sampling.h"

#define length(x) (sizeof (x) / sizeof *(x))

/******************************************************************************/
/* Initialise the population                                                  */
/******************************************************************************/
void initialise(int N, int ind_traits, double Tm, double Tf, double rejg, 
                double mim, double mom, double gam, double mov, double a1, 
                double lambd, int xdim, int ydim, double **inds){

    int row;
    
    for(row = 0; row < N; row++){
      inds[row][0]  = row + 1;
      inds[row][1]  = randbinom(0.5);
      inds[row][2]  = randunifint(0, xdim - 1);
      inds[row][3]  = randunifint(0, ydim - 1);
      inds[row][4]  = 1;
      inds[row][5]  = Tm;
      inds[row][6]  = Tf;
      inds[row][7]  = gft;
      inds[row][8]  = rejg;
      inds[row][9]  = mim;
      inds[row][10] = mom;
      inds[row][11] = gam;
      inds[row][12] = mov;
      inds[row][13] = a1;
      inds[row][14] = 0;
      inds[row][15] = 0;
      inds[row][16] = 0;
      inds[row][17] = 0;
      inds[row][18] = lambd;
      inds[row][19] = 0;
      inds[row][20] = 0;
    }

}

/******************************************************************************/
/* Main outer function that runs a nuptial gift giving simulation over time   */
/******************************************************************************/
void nuptials(int time_steps, int N, double Tm, double Tf, double rejg,
              double mim, double mom, double gam, double mov, double a1,
              double lambd, int xdim, int ydim){

    int ts, row, ind_traits;
    double **inds;
    
    /* =======================================================================*/
    /* DEFINE VARIABLES TO BE USED: */
    /* =======================================================================*/
    ind_traits = 20;
    
    inds = (double **) malloc(N * sizeof(double));
    for(row = 0; row < N; row++){
      inds[row] = (double *) malloc(ind_traits * sizeof(double));
    }


    /* =======================================================================*/
    /* INITIALISE INDIVIDUALS: */
    /* =======================================================================*/
    initialise(N, ind_traits, Tm, Tf, rejg, mim, mom, gam, mov, a1, lambd, xdim,
               ydim, **inds);
    
    ts = 0;
    while(ts < time_steps){

        /* ==========================================================*/
        /* MOVE INDIVIDUALS                                          */
        /* ==========================================================*/        


        /* ==========================================================*/
        /* GET OFFSPRING                                             */
        /* ==========================================================*/
        
        
        /* ==========================================================*/
        /* ADD OFFSPRING                                             */
        /* ==========================================================*/
        
        /* ==========================================================*/
        /* REMOVE DEAD                                               */
        /* ==========================================================*/
        
        /* ==========================================================*/
        /* CARRYING CAPACITY                                         */
        /* ==========================================================*/
        
        ts++;
    }
}


