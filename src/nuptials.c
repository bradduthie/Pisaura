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
      inds[row][17] = lambd;
      inds[row][18] = 0;
      inds[row][19] = 0;
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
/* Female and male interaction                                                */
/******************************************************************************/
void female_male_int(double **inds, int female, int male){

    double rej_gift, acceptml, male_add, birth_par, offspring;

    rej_gift = inds[female][8];
    acceptml = randunif();
    if(rej_gift < acceptml){
      male_add         = inds[male][11] * inds[male][7];
      birth_par        = inds[female][17] + male_add;
      offspring        = randpois(birth_par);
      inds[female][14] = offspring; 
      inds[female][4]  = 0;
      inds[male][4]    = 0;
      inds[male][7]    = 0;
      inds[female][19] = inds[male][0];
    }
}

/******************************************************************************/
/* Females enter the mating pool                                              */
/******************************************************************************/
void female_enter(double **inds, int female){

  double iTf, enter_prob, enter_rand;

  iTf        = inds[female][6];
  enter_prob = exp(-1 * iTf);
  enter_rand = randunif();
  if(enter_rand < enter_prob){
    inds[female][4] = 1.0;
  }
}

/******************************************************************************/
/* Males searching for nuptial gift                                           */
/******************************************************************************/
void male_search(double **inds, int male){

  int isin;
  double iTm, a1, enter_prob, enter_rand, gift_prob, gift_rand;

  isin       = (int) inds[male][4];
  iTm        = inds[male][5];
  a1         = inds[male][13];
  enter_prob = exp(-1 * iTm);
  enter_rand = randunif();
  if(enter_rand < enter_prob){
    inds[male][4] = 1.0;
    isin          = 1;
  }
  gift_prob = 1 - exp(-1 * (1 / a1) * iTm);
  gift_rand = randunif();
  if(isin > 0 && gift_rand < gift_prob){
    inds[male][7] = 1.0;
  }
}

/******************************************************************************/
/* Assess mortality of individual                                             */
/******************************************************************************/
void ind_mortality(double **inds, int i){
  
  double in_or_out, rand_mort, in_mort, mort_pr, out_mort;

  in_or_out = inds[i][4];
  rand_mort = randunif();
  if(in_or_out > 0){
    in_mort  = inds[i][9];
    mort_pr  = 1 - exp(-1 * in_mort);
  }else{
    out_mort = inds[i][10];
    mort_pr  = 1 - exp(-1 * out_mort);
  }
  if(rand_mort < mort_pr){
    inds[i][18] = 1.0;
  }
}


/******************************************************************************/
/* Get offspring numbers                                                      */
/******************************************************************************/
void get_offspring(double **inds, int N){

    int i, j, row, isex, ixloc, iyloc, iisin, jsex, jxloc, jyloc, jisin; 
    int *IDs;
    
    IDs = malloc(N * sizeof(int));

    for(i = 0; i < N; i++){
      IDs[i] = i;
    }

    rand_int_shuffle(IDs, N); /* Shuffling the IDs */
    
    for(row = 0; row < N; row++){
        i     = (int) IDs[row];
        isex  = (int) inds[i][1];
        ixloc = (int) inds[i][2];
        iyloc = (int) inds[i][3];
        iisin = (int) inds[i][4];
        if(isex > 0 && iisin > 0){
          for(j = 0; j < N; j++){
              jsex  = (int) inds[j][1];
              jxloc = (int) inds[j][2];
              jyloc = (int) inds[j][3];
              jisin = (int) inds[j][4];
              if(jxloc == ixloc && jyloc == iyloc && jsex == 0 && jisin > 0){
                   female_male_int(inds, i, j);
              }
          }
        }
        if(iisin == 0 && isex == 0){
           female_enter(inds, row);
        }
        if(iisin == 0 && isex == 1){
           male_search(inds, row);
        }
        ind_mortality(inds, i);
    }

    free(IDs);
}

/******************************************************************************/
/* Main outer function that runs a nuptial gift giving simulation over time   */
/******************************************************************************/
void nuptials(int time_steps, int N, double Tm, double Tf, double rejg,
              double mim, double mom, double gam, double mov, double a1,
              double lambd, int xdim, int ydim){

    int ts, row, col, ind_traits;
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
        
        /* XXX XXX CHECK get_offspring XXX But left off HERE XXX XXX */

        /* ==========================================================*/
        /* REMOVE DEAD                                               */
        /* ==========================================================*/
        
        /* ==========================================================*/
        /* CARRYING CAPACITY                                         */
        /* ==========================================================*/
        
        ts++;
        
        printf("Time step: %d\n", ts);
    }

    for(row = 0; row < N; row++){
        for(col = 0; col < 6; col++){
            printf("%f\t", inds[row][col]);
        } 
        printf("\n");
    }

    for(row = 0; row < N; row++){
      free(inds[row]);
    }
    free(inds);
}


