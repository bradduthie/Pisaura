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
void initialise(int N, double Tm, double Tf, double rejg, double mim, 
                double mom, double gam, double mov, double a1, double lambd, 
                int xdim, int ydim, double **inds){

    int row;
    
    for(row = 0; row < N; row++){
      inds[row][0]  = row + 1;
      inds[row][1]  = randbinom(0.5);
      inds[row][2]  = randunifint(0, xdim - 1);
      inds[row][3]  = randunifint(0, ydim - 1);
      inds[row][4]  = 1;
      inds[row][5]  = Tm;
      inds[row][6]  = Tf;
      inds[row][7]  = 0;
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
/* Move individuals                                                           */
/******************************************************************************/
void move_inds(double **inds, int xdim, int ydim, int N){

    int i, xloc, yloc, move_max_x, new_xloc, move_max_y, new_yloc;
    
    for(i = 0; i < N; i++){
      xloc       = (int) inds[i][2];
      yloc       = (int) inds[i][3];
      move_max_x = (int) inds[i][12];
      move_max_y = (int) inds[i][12];
      new_xloc   = xloc + randunifint(-1 * move_max_x, move_max_x);
      new_yloc   = yloc + randunifint(-1 * move_max_y, move_max_y);
      if(new_xloc < 0){
        xloc = new_xloc + xdim;
      }
      if(new_xloc >= xdim){
        xloc = new_xloc - xdim;
      }
      if(new_yloc < 0){
        yloc = new_yloc + ydim;
      }
      if(new_yloc >= ydim){
        yloc = new_yloc - ydim;
      }
      inds[i][2] = (double) xloc;
      inds[i][3] = (double) yloc;
    }
}


/******************************************************************************/
/* Get offspring numbers                                                      */
/******************************************************************************/
void get_offspring(double **inds, int N){

    int i, *IDs;
    
    IDs = malloc(N * sizeof(int));

    for(i = 0; i < N; i++){
      IDs[i] = (int) inds[i][0];
    }

    rand_int_shuffle(IDs, N); /* Shuffling the IDs */
    
    /* LET OFF HERE XXX: 
    
    XXX Need to cycle through random order to see if interaction happens
    
    */
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
    initialise(N, Tm, Tf, rejg, mim, mom, gam, mov, a1, lambd, xdim, ydim, 
               inds);
    
    ts = 0;
    while(ts < time_steps){

        /* ==========================================================*/
        /* MOVE INDIVIDUALS                                          */
        /* ==========================================================*/        
        move_inds(inds, xdim, ydim, N);

        /* ==========================================================*/
        /* GET OFFSPRING                                             */
        /* ==========================================================*/
        get_offspring(inds, N);
        
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
        
        printf("Time step: %d\n", ts);
    }

}


