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
/* Males searching for nuptial gift                                           */
/******************************************************************************/
void male_search(double **inds, int male){

  double time_out, pr_gift, rand_unif;

  time_out = 0.0;
  pr_gift  = 0.0;
  if(inds[male][5] > 0){
    time_out  = (double) randpois(inds[male][5]);
    if(inds[male][13] <= 0){
        pr_gift = 1.0;
    }else{
        pr_gift = 1 - exp(-1 * inds[male][5] / inds[male][13]);
    }
  }
  
  rand_unif = randunif();

  if(rand_unif < pr_gift){
    inds[male][7] = 1.0;
  }else{
    inds[male][7] = 0.0;
  }

  if(time_out > 0){
    inds[male][4]  = 0.0;
    inds[male][23] = time_out;
  }else{
    inds[male][4]  = 1.0;
    inds[male][23] = 0.0;
  }

}

/******************************************************************************/
/* Females processing an offspring                                            */
/******************************************************************************/
void female_process(double **inds, int female){

  int time_out;

  time_out = (int) randpois(inds[female][6]);

  if(time_out > 0){
    inds[female][4]  = 0.0;
    inds[female][23] = (double) time_out;
  }else{
    inds[female][4]  = 1.0;
    inds[female][23] = 0.0;
  }
}


/******************************************************************************/
/* Initialise the population                                                  */
/******************************************************************************/
void initialise(int N, double Tm, double Tf, double rejg, double mim, 
                double mom, double gam, double mov, double a1, double lambd, 
                int xdim, int ydim, double **inds){

    int row;
    
    for(row = 0; row < N; row++){
      inds[row][0]  = (double) row + 1;          /* ID                        */
      inds[row][1]  = randbinom(0.5);            /* Sex                       */
      inds[row][2]  = randunifint(0, xdim - 1);  /* xloc                      */
      inds[row][3]  = randunifint(0, ydim - 1);  /* yloc                      */
      inds[row][4]  = 0.0;
      inds[row][5]  = Tm;                        /* Male search time          */
      inds[row][6]  = Tf;                        /* Female processing time    */
      inds[row][7]  = 0.0;                       /* Male has nuptial gift?    */
      inds[row][8]  = rejg;                      /* Female rejection prob     */
      inds[row][9]  = mim;                       /* Mortality in mating pool  */
      inds[row][10] = mom;                       /* Mortality out mating pool */
      inds[row][11] = gam;                       /* Bonus for nuptial gift    */
      inds[row][12] = mov;                       /* Movement parameter        */
      inds[row][13] = a1;                        /* Mean search time needed   */
      inds[row][14] = 0.0;                       /* Offspring number          */
      inds[row][15] = 0.0;                       /* Mother's row              */
      inds[row][16] = 0.0;                       /* Father's row              */
      inds[row][17] = lambd;                     /* Baseline fecundity        */
      inds[row][18] = 0.0;                       /* Is dead?                  */
      inds[row][19] = 0.0;                       /* Mate's ID                 */
      inds[row][20] = 0.0;                       /* Has obtained gift?        */
      inds[row][21] = 0.0;                       /* Mating encounters         */
      inds[row][22] = 0.0;                       /* Neutral allele            */
      inds[row][23] = 0.0;                       /* Time steps out            */
      inds[row][24] = inds[row][4];              /* Was in the mating pool?   */
      if(inds[row][1] > 0){
        male_search(inds, row);
      }
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

    inds[female][21]++;
    inds[male][21]++;
  
    rej_gift = inds[female][8];
    acceptml = randunif();
    if(rej_gift < acceptml || inds[male][7] > 0){
      male_add         =  inds[male][11] * inds[male][7];
      birth_par        =  inds[female][17] + male_add;
      offspring        =  (double) randpois(birth_par);
      inds[female][14] =  offspring; 
      inds[male][7]    =  0.0;
      inds[female][19] =  inds[male][0];
      male_search(inds, male);
      female_process(inds, female);
    }
}


/******************************************************************************/
/* Assess mortality of individual                                             */
/******************************************************************************/
void ind_mortality(double **inds, int i){
  
  double in_pool, rand_mort, in_mort, mort_pr, out_mort;

  in_pool   = inds[i][4];
  rand_mort = randunif();
  if(in_pool > 0){
    in_mort  = inds[i][9];
    mort_pr  = 1.0 - exp(-1.0 * in_mort);
  }else{
    out_mort = inds[i][10];
    mort_pr  = 1.0 - exp(-1.0 * out_mort);
  }
  if(rand_mort < mort_pr){
    inds[i][18] = 1.0;
  }
}


/******************************************************************************/
/* Cycle through individual mortality                                         */
/******************************************************************************/
void mortality(double **inds, int N){

    int row; 
    
    for(row = 0; row < N; row++){
        ind_mortality(inds, row);
    }
}


/******************************************************************************/
/* Offspring enter mating pool                                                */
/******************************************************************************/
void enter_mating_pool(double **inds, int N){

    int row, iisin; 
    
    for(row = 0; row < N; row++){
        
        inds[row][21] = 0.0;
        inds[row][24] = 0.0;
        iisin         = (int) inds[row][4];
        
        if(iisin < 1){
            inds[row][23]--; 
            if(inds[row][23] < 1.0){
                inds[row][4]  = 1.0;
                inds[row][24] = 1.0;
            }
        }
    }
}

/******************************************************************************/
/* Get offspring numbers                                                      */
/******************************************************************************/
void get_offspring(double **inds, int N, double M_exp){

    int i, j, isex, ixloc, iyloc, iisin, jsex, jxloc, jyloc, jisin, Ms; 

    Ms = M_exp * N;  

    while(Ms > 0){
        do{
            i = randunifint(0, N - 1);
            j = randunifint(0, N - 1);
        }while(i == j);
        isex  = (int) inds[i][1];
        ixloc = (int) inds[i][2];
        iyloc = (int) inds[i][3];
        iisin = (int) inds[i][4];
        jsex  = (int) inds[j][1];
        jxloc = (int) inds[j][2];
        jyloc = (int) inds[j][3];
        jisin = (int) inds[j][4];
        if(iisin > 0 && jisin > 0 && ixloc == jxloc && iyloc == jyloc){
            if(isex == 0 && jsex == 1){
                female_male_int(inds, i, j);
            }
            if(isex == 1 && jsex == 0){
                female_male_int(inds, j, i);
            }
        }
        Ms--;
    }
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
double off_trait(double **inds, int mum_row, int dad_row, int trait_col, 
                 double mu){
    
    double p_mean, mu_val, off_val;

    p_mean = 0.5 * (inds[mum_row][trait_col] + inds[dad_row][trait_col]);
    mu_val = randnorm(0, mu);

    off_val = p_mean + mu_val;

    return off_val;
}


/******************************************************************************/
/* Add offspring to a new array                                               */
/******************************************************************************/
void add_offspring(double **inds, int N, double **offs, int off_N, int traits,
                   int *ID, double Tm_mu, double rg_mu, double N_mu,
                   int xdim, int ydim){

    int mum_row, dad_row, off_pos, dad_ID;

    off_pos = 0;
    for(mum_row = 0; mum_row < N; mum_row++){
        while(inds[mum_row][14] > 0){
            dad_ID   = inds[mum_row][19];
            dad_row  = find_dad(inds, N, dad_ID);
            /* Inserting offspring traits below */
            offs[off_pos][0]  = (double) ID[0];
            offs[off_pos][1]  = randbinom(0.5);
            offs[off_pos][2]  = randunifint(0, xdim - 1);
            offs[off_pos][3]  = randunifint(0, ydim - 1);
            if(offs[off_pos][1] == 0){
              offs[off_pos][4]  = 1.0;
            }else{
              offs[off_pos][4]  = 0.0;
            }
            offs[off_pos][5]  = off_trait(inds, mum_row, dad_row, 5, Tm_mu);
            offs[off_pos][6]  = off_trait(inds, mum_row, dad_row, 6, 0.0);
            offs[off_pos][7]  = 0.0;
            offs[off_pos][8]  = off_trait(inds, mum_row, dad_row, 8, rg_mu);
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
            offs[off_pos][20] = 0.0;
            offs[off_pos][21] = 0.0;
            offs[off_pos][22] = off_trait(inds, mum_row, dad_row, 22, N_mu);
            if(offs[off_pos][1] == 0 || offs[off_pos][5] <= 0){
              offs[off_pos][23] = 0.0;
            }else{
              offs[off_pos][23] = (double) randpois(offs[off_pos][5]);
            }
            offs[off_pos][24] = 0.0;
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
              int off_N, int time_steps, double mim, double mom, double gam, 
              double mov, double a1, double lambd, int xdim, int ydim, int K,
              double Tm_init, double rg_init, double Tm_mu, double rg_mu,
              double N_mu){

    int row, col, pid;
    double sex_ratio, time_in, Tm, Tf, Gift, RejPr, count, mcount, fcount;
    double sex_ratio_m, time_in_m, Tm_m, Tf_m, Gift_m, RejPr_m, SS_Tm, SS_Rj;
    double Vr_Tm, Vr_Rj, M_tot, N_tot, M_m, N_m, Beta, m_in, f_in;
    char outfile[17];

    FILE *fptr;

    pid = getpid();

    sprintf(outfile, "results_%d.csv", pid);
    fptr = fopen(outfile, "a+");

    switch(stats){
      case 0:
        if(ts == time_steps - 1){
          sex_ratio = 0.0;
          time_in   = 0.0;
          Tm        = 0.0;
          Tf        = 0.0;
          Gift      = 0.0;
          RejPr     = 0.0;
          M_tot     = 0.0;
          N_tot     = 0.0;
          count     = 0.0;
          fcount    = 0.0;
          mcount    = 0.0;
          f_in      = 0.0;
          m_in      = 0.0;
          for(row = 0; row < N; row++){
              sex_ratio += inds[row][1];
              if(inds[row][24] > 0){
                M_tot     += inds[row][21];
                time_in++;
              }
              Tm        += inds[row][5];
              Tf        += inds[row][6];
              Gift      += inds[row][20];
              RejPr     += inds[row][8];
              N_tot     += inds[row][22];
              count++;
              if(inds[row][1] == 0){
                  fcount++;
              }
              if(inds[row][1] == 1){
                  mcount++;
              }
              if(inds[row][1] == 0 && inds[row][24] == 1){
                f_in++;
              }
              if(inds[row][1] == 1 && inds[row][24] == 1){
                m_in++;
              }
          }
          sex_ratio_m = sex_ratio / count;
          time_in_m   = time_in   / count;
          Tm_m        = Tm        / count;
          Tf_m        = Tf        / count;
          Gift_m      = Gift      / mcount;
          RejPr_m     = RejPr     / count;
          M_m         = M_tot     / time_in;
          N_m         = N_tot     / count;
          Beta        = m_in      / f_in;
          SS_Tm       = 0.0;
          SS_Rj       = 0.0;
          for(row = 0; row < N; row++){
              SS_Tm += (inds[row][5] - Tm_m) * (inds[row][5] - Tm_m);
              SS_Rj += (inds[row][8] - RejPr_m) * (inds[row][8] - RejPr_m);
          }
          Vr_Tm = SS_Tm / (N - 1);
          Vr_Rj = SS_Rj / (N - 1);
          fprintf(fptr, "%f,%f,%f,%f,%f,%f,%d,%d,%d,%f,%f,%f,%f,%d,%f,%f,%f,%f,\
                         %f,%f,%d,%d,%f,%f,%f,%f,%f\n", mim, mom, gam, mov, a1, 
                         lambd, xdim, ydim, K, Tm_init, rg_init, Tm_mu, rg_mu, 
                         ts, sex_ratio_m, time_in_m, Tm_m, Tf_m, Gift_m, 
                         RejPr_m, off_N, N, Vr_Tm, Vr_Rj, M_m, N_m, Beta);
        }
        break;
      case 1:
        sex_ratio = 0.0;
        time_in   = 0.0;
        Tm        = 0.0;
        Tf        = 0.0;
        Gift      = 0.0;
        RejPr     = 0.0;
        M_tot     = 0.0;
        N_tot     = 0.0;
        count     = 0.0;
        fcount    = 0.0;
        mcount    = 0.0;
        f_in      = 0.0;
        m_in      = 0.0;
        for(row = 0; row < N; row++){
            sex_ratio += inds[row][1];
            if(inds[row][24] > 0){
              M_tot     += inds[row][21];
              time_in++;
            }
            Tm        += inds[row][5];
            Tf        += inds[row][6];
            Gift      += inds[row][20];
            RejPr     += inds[row][8];
            N_tot     += inds[row][22];
            count++;
            if(inds[row][1] == 0){
                fcount++;
            }
            if(inds[row][1] == 1){
                mcount++;
            }
            if(inds[row][1] == 0 && inds[row][24] == 1){
              f_in++;
            }
            if(inds[row][1] == 1 && inds[row][24] == 1){
              m_in++;
            }
        }
        sex_ratio_m = sex_ratio / count;
        time_in_m   = time_in   / count;
        Tm_m        = Tm        / count;
        Tf_m        = Tf        / count;
        Gift_m      = Gift      / mcount;
        RejPr_m     = RejPr     / count;
        M_m         = M_tot     / time_in;
        N_m         = N_tot     / count;
        Beta        = m_in      / f_in;
        SS_Tm       = 0.0;
        SS_Rj       = 0.0;
        for(row = 0; row < N; row++){
            SS_Tm += (inds[row][5] - Tm_m) * (inds[row][5] - Tm_m);
            SS_Rj += (inds[row][8] - RejPr_m) * (inds[row][8] - RejPr_m);
        }
        Vr_Tm = SS_Tm / (N - 1);
        Vr_Rj = SS_Rj / (N - 1);
        fprintf(fptr, "%d,%f,%f,%f,%f,%f,%f,%d,%d,%f,%f,%f,%f,%f\n", ts, 
                sex_ratio_m, time_in_m, Tm_m, Tf_m, Gift_m, RejPr_m, off_N, N, 
                Vr_Tm, Vr_Rj, M_m, N_m, Beta);
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
    
    for(row = 0; row < N; row++){
      inds[row][21] = 0.0;
      inds[row][24] = 0.0;
    }

    fclose(fptr);
}

/******************************************************************************/
/* Main outer function that runs a nuptial gift giving simulation over time   */
/******************************************************************************/
void nuptials(int time_steps, int N, double Tm, double Tf, double rejg,
              double mim, double mom, double gam, double mov, double a1,
              double lambd, int xdim, int ydim, int K, int stats, 
              double Tm_mu, double rg_mu, double N_mu){

    int ts, row, ind_traits, off_N, new_N, *ID, pid;
    double **inds, **offs, **news;
    char outfile[20];

    FILE *fptr;    

    ind_traits = 25;
    
    if(stats == 1){
      pid = getpid();
      sprintf(outfile, "results_%d.csv", pid);
      fptr = fopen(outfile, "a+");
      fprintf(fptr, "Time,Sex_ratio,Time-in,Tm,Tf,Gift,RejPr,Offs,N,Vr_Tm,\
                     Vr_Rj,M_m,N_m,Beta\n");
      fclose(fptr);
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
    while(ts < time_steps && N > 10){
   
        move_inds(inds, xdim, ydim, N);

        enter_mating_pool(inds, N);

        get_offspring(inds, N, 2);

        mortality(inds, N);

        off_N = count_offspring(inds, N);

        sumstats(inds, N, ind_traits, stats, ts, off_N, time_steps, mim, mom,
                 gam, mov, a1, lambd, xdim, ydim, K, Tm, rejg, Tm_mu, rg_mu,
                 N_mu);

        if(off_N > 0){   
          offs  = (double **) malloc(off_N * sizeof(double));
          for(row = 0; row < off_N; row++){
              offs[row] = (double *) malloc(ind_traits * sizeof(double));
          }

          add_offspring(inds, N, offs, off_N, ind_traits, ID, Tm_mu, rg_mu, 
                        N_mu, xdim, ydim);

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
        
        ts++;
        
        if(stats > 0){
          printf("Time step: %d\t%d\t%d\n", ts, N, off_N);
        }
    }

    for(row = 0; row < N; row++){
      free(inds[row]);
    }
    free(inds);
    free(ID);
}


