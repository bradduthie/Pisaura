#include "rand_sampling.h"

void swap_arrays(void **ARRAY_A, void **ARRAY_B);

void initialise(int N, double Tm, double Tf, double rejg, double mim, 
                double mom, double gam, double mov, double a1, double lambd, 
                int xdim, int ydim, double **inds);

void move_inds(double **inds, int xdim, int ydim, int N);

void female_male_int(double **inds, int male, int female);

void female_enter(double **inds, int female);

void male_search(double **inds, int male);

void ind_mortality(double **inds, int i);

void get_offspring(double **inds, int N);

int count_offspring(double **inds, int N);

int find_dad(double **inds, int N, double dad_ID);

double off_trait(double **inds, int mum_row, int dad_row, int trait_col);

void add_offspring(double **inds, int N, double **offs, int off_N, int traits,
                   int *ID);

void apply_K(double **inds, int N, double **offs, int off_N, int K);

int count_living(double **inds, int N, double **offs, int off_N);

void build_new_N_offs(double **inds, int N, double **offs, int off_N, int new_N,
                      double **news, int ind_traits);

void build_newN(double **inds, int N, int new_N, double **news, int ind_traits);

void sumstats(double **inds, int N, int ind_traits, int stats, int ts, 
              int off_N);

void nuptials(int time_steps, int N, double Tm, double Tf, double rejg,
              double mim, double mom, double gam, double mov, double a1,
              double lambd, int xdim, int ydim, int K, int stats, 
              double Tm_mu, double rg_mu);
