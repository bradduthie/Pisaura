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

/* =============================================================================
 * Swap pointers to rewrite ARRAY_B into ARRAY_A for a an array of any dimension
 * ========================================================================== */
void swap_arrays(void **ARRAY_A, void **ARRAY_B){
    
    void *TEMP_ARRAY;
    
    TEMP_ARRAY = *ARRAY_A;
    *ARRAY_A   = *ARRAY_B;
    *ARRAY_B   = TEMP_ARRAY;
}

/******************************************************************************/
/* Initialise the population                                                  */
/******************************************************************************/
void initialise(int N, double Tm, double Tf, double rejg, double mim, 
                double mom, double gam, double mov, double a1, double lambd, 
                int xdim, int ydim, double **inds){

    int row;
    
    for(row = 0; row < N; row++){
      inds[row][0]  = (double) row + 1;
      inds[row][1]  = randbinom(0.5);
      inds[row][2]  = randunifint(0, xdim - 1);
      inds[row][3]  = randunifint(0, ydim - 1);
      inds[row][4]  = 1.0;
      inds[row][5]  = Tm;
      inds[row][6]  = Tf;
      inds[row][7]  = 0.0;
      inds[row][8]  = rejg;
      inds[row][9]  = mim;
      inds[row][10] = mom;
      inds[row][11] = gam;
      inds[row][12] = mov;
      inds[row][13] = a1;
      inds[row][14] = 0.0;
      inds[row][15] = 0.0;
      inds[row][16] = 0.0;
      inds[row][17] = lambd;
      inds[row][18] = 0.0;
      inds[row][19] = 0.0;
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
/* Count the total number of offspring                                        */
/******************************************************************************/
int count_offspring(double **inds, int N){
    int i, count;

    count = 0;
    for(i = 0; i < N; i++){
        count += inds[i][14];
    }

    return count;
}

/******************************************************************************/
/* Find the dad for a mum's offspring                                         */
/******************************************************************************/
int find_dad(double **inds, int N, double dad_ID){
    int i;
    
    i = 0;
    while(inds[i][0] != dad_ID && i < N){
        i++;
    }

    return i;
}

/******************************************************************************/
/* Offspring trait from mum and dad                                           */
/******************************************************************************/
double off_trait(double **inds, int mum_row, int dad_row, int trait_col){
    
    double p_mean, mu_val, off_val;

    p_mean = 0.5 * (inds[mum_row][trait_col] + inds[dad_row][trait_col]);
    mu_val = randnorm(0, 0.02);

    off_val = p_mean + mu_val;
    if(off_val < 0.0){
        off_val = 0.0;
    }

    return off_val;
}


/******************************************************************************/
/* Add offspring to a new array                                               */
/******************************************************************************/
void add_offspring(double **inds, int N, double **offs, int off_N, int traits,
                   int *ID){

    int mum_row, dad_row, off_pos, dad_ID;

    off_pos = 0;
    for(mum_row = 0; mum_row < N; mum_row++){
        while(inds[mum_row][14] > 0){
            dad_ID   = inds[mum_row][19];
            dad_row  = find_dad(inds, N, dad_ID);
            /* Inserting offspring traits below */
            offs[off_pos][0]  = (double) ID[0];
            offs[off_pos][1]  = randbinom(0.5);
            offs[off_pos][2]  = inds[mum_row][2];
            offs[off_pos][3]  = inds[mum_row][3];
            offs[off_pos][4]  = 1.0;
            offs[off_pos][5]  = off_trait(inds, mum_row, dad_row, 5);
            offs[off_pos][6]  = off_trait(inds, mum_row, dad_row, 6);
            offs[off_pos][7]  = 0.0;
            offs[off_pos][8]  = off_trait(inds, mum_row, dad_row, 8);
            offs[off_pos][9]  = inds[mum_row][9];
            offs[off_pos][10] = inds[mum_row][10];
            offs[off_pos][11] = inds[mum_row][11];
            offs[off_pos][12] = inds[mum_row][12];
            offs[off_pos][13] = inds[mum_row][13];
            offs[off_pos][14] = 0.0;
            offs[off_pos][15] = inds[mum_row][0];
            offs[off_pos][16] = inds[dad_row][0];
            offs[off_pos][17] = inds[mum_row][17];
            offs[off_pos][18] = 0.0;
            offs[off_pos][19] = 0.0;
            /* Prepare for next offspring */
            off_pos++;
            ID[0]++;
            inds[mum_row][14]--;
        }
    }
}


/******************************************************************************/
/* Apply the carrying capacity to inds and offspring                          */
/******************************************************************************/
void apply_K(double **inds, int N, double **offs, int off_N, int K, int new_N){

    int old_N, kill;

    old_N = N + off_N;

    while(new_N > K){
      kill = randunifint(0, old_N - 1);
      if(kill < N){
        if(inds[kill][18] < 1.0){
          inds[kill][18] = 1.0;
          new_N--;
        }
      }else{
        kill -= N;
        if(offs[kill][18] < 1.0){        
          offs[kill][18] =  1.0;
          new_N--;
        }
      }
    }
}

/******************************************************************************/
/* Counts the number of living individuals                                    */
/******************************************************************************/
int count_living(double **inds, int N, double **offs, int off_N){

    int count, i;

    count = 0;

    for(i = 0; i < N; i++){
        if(inds[i][18] < 1.0){
          count++;
        }
    }
    for(i = 0; i < off_N; i++){
        if(offs[i][18] < 1.0){
          count++;
        }
    }

    return count;
}


/******************************************************************************/
/* Apply the carrying capacity to inds and offspring                          */
/******************************************************************************/
void build_new_N_offs(double **inds, int N, double **offs, int off_N, int new_N,
                      double **news, int ind_traits){

    int row, col, new_row;

    new_row = 0;
    for(row = 0; row < N; row++){
      if(inds[row][18] < 1.0){
        for(col = 0; col < ind_traits; col++){
          news[new_row][col] = inds[row][col];
        }
        new_row++;
      }
    }
    for(row = 0; row < off_N; row++){
      if(offs[row][18] < 1.0){
        for(col = 0; col < ind_traits; col++){
          news[new_row][col] = offs[row][col];
        }
        new_row++;
      }
    }
}

/******************************************************************************/
/* Apply the carrying capacity to inds and offspring                          */
/******************************************************************************/
void build_newN(double **inds, int N, int new_N, double **news, int ind_traits){

    int row, col, new_row;

    new_row = 0;
    for(row = 0; row < N; row++){
      if(inds[row][18] < 1.0){
        for(col = 0; col < ind_traits; col++){
          news[new_row][col] = inds[row][col];
        }
        new_row++;
      }
    }
}

/******************************************************************************/
/* Get summary statistics                                                     */
/******************************************************************************/
void sumstats(double **inds, int N, int ind_traits, int stats, int ts,
              int off_N){

    int row, col, pid;
    double sex_ratio, time_in, Tm, Tf, Gift, RejPr, count;
    char outfile[20];

    FILE *fptr;

    pid = getpid();

    sprintf(outfile, "results_%d.csv", pid);
    fptr = fopen(outfile, "a+");

    switch(stats){
      case 0:

        break;
      case 1:
        sex_ratio = 0.0;
        time_in   = 0.0;
        Tm        = 0.0;
        Tf        = 0.0;
        Gift      = 0.0;
        RejPr     = 0.0;
        count     = 0.0;
        for(row = 0; row < N; row++){
            sex_ratio += inds[row][1];
            time_in   += inds[row][4];
            Tm        += inds[row][5];
            Tf        += inds[row][6];
            Gift      += inds[row][7];
            RejPr     += inds[row][8];
            count++;
        }
        sex_ratio = sex_ratio / count;
        time_in   = time_in   / count;
        Tm        = Tm        / count;
        Tf        = Tf        / count;
        Gift      = Gift      / count;
        RejPr     = RejPr     / count;
        fprintf(fptr, "%d,%f,%f,%f,%f,%f,%f,%d,%d\n", ts, sex_ratio, 
                time_in, Tm, Tf, Gift, RejPr, off_N, N);
        break;
      case 2:
        for(row = 0; row < N; row++){
          fprintf(fptr, "%d,", ts);
          for(col = 0; col < ind_traits; col++){
            if(col < ind_traits - 1){
               fprintf(fptr, "%f,", inds[row][col]);
            }else{
               fprintf(fptr, "%f", inds[row][col]);
            }
          }
          fprintf(fptr, "\n");
        }
        break;

    }
}

/******************************************************************************/
/* Main outer function that runs a nuptial gift giving simulation over time   */
/******************************************************************************/
void nuptials(int time_steps, int N, double Tm, double Tf, double rejg,
              double mim, double mom, double gam, double mov, double a1,
              double lambd, int xdim, int ydim, int K, int stats){

    int ts, row, ind_traits, off_N, new_N, *ID, pid;
    double **inds, **offs, **news;
    char outfile[20];

    FILE *fptr;    

    ind_traits = 20;
    
    if(stats < 2){
      pid = getpid();
      sprintf(outfile, "results_%d.csv", pid);
      fptr = fopen(outfile, "a+");
      fprintf(fptr, "Time, Sex_ratio, Time-in, Tm, Tf, Gift, RejPr, Offs, N\n");
    }

    ID   = malloc(sizeof(int));
    inds = (double **) malloc(N * sizeof(double));
    for(row = 0; row < N; row++){
      inds[row] = (double *) malloc(ind_traits * sizeof(double));
    }
    
    initialise(N, Tm, Tf, rejg, mim, mom, gam, mov, a1, lambd, xdim, ydim, 
               inds);
    ID[0] = N;
    
    ts = 0;
    while(ts < time_steps){
   
        move_inds(inds, xdim, ydim, N);

        get_offspring(inds, N);

        off_N = count_offspring(inds, N);

        if(off_N > 0){   
          offs  = (double **) malloc(off_N * sizeof(double));
          for(row = 0; row < off_N; row++){
              offs[row] = (double *) malloc(ind_traits * sizeof(double));
          }

          add_offspring(inds, N, offs, off_N, ind_traits, ID);

          new_N = count_living(inds, N, offs, off_N);

          apply_K(inds, N, offs, off_N, K, new_N);

          new_N = count_living(inds, N, offs, off_N);

          news = (double **) malloc(new_N * sizeof(double));
          for(row = 0; row < new_N; row++){
            news[row] = (double *) malloc(ind_traits * sizeof(double));
          }

          build_new_N_offs(inds, N, offs, off_N, new_N, news, ind_traits);

          for(row = 0; row < N; row++){
            free(inds[row]);
          }
          free(inds);
     
          N = new_N; 

          inds = (double **) malloc(N * sizeof(double));
          for(row = 0; row < N; row++){
            inds[row] = (double *) malloc(ind_traits * sizeof(double));
          }

          swap_arrays((void*)&news, (void*)&inds); 

          for(row = 0; row < new_N; row++){
            free(news[row]);
          }
          free(news);

          for(row = 0; row < off_N; row++){
              free(offs[row]);
          }
          free(offs);

        }else{

          new_N = count_living(inds, N, offs, off_N);

          news = (double **) malloc(new_N * sizeof(double));
          for(row = 0; row < new_N; row++){
            news[row] = (double *) malloc(ind_traits * sizeof(double));
          }

          build_newN(inds, N, new_N, news, ind_traits);

          for(row = 0; row < N; row++){
            free(inds[row]);
          }
          free(inds);
       
          N = new_N; 

          inds = (double **) malloc(N * sizeof(double));
          for(row = 0; row < N; row++){
            inds[row] = (double *) malloc(ind_traits * sizeof(double));
          }

          swap_arrays((void*)&news, (void*)&inds); 

          for(row = 0; row < new_N; row++){
            free(news[row]);
          }
          free(news);


        }

        sumstats(inds, N, ind_traits, stats, ts, off_N);
        
        ts++;
        
        printf("Time step: %d\t%d\t%d\n", ts, N, off_N);
    }

    for(row = 0; row < N; row++){
      free(inds[row]);
    }
    free(inds);
    free(ID);
}


