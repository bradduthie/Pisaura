#include "rand_sampling.h"

void initialise(int N, double Tm, double Tf, double rejg, double mim, 
                double mom, double gam, double mov, double a1, double lambd, 
                int xdim, int ydim, double **inds);

void move_inds(double **inds, int xdim, int ydim, int N);

void female_male_int(double **inds, int female, int male);

void female_enter(double **inds, int female);

void male_search(double **inds, int male);

void ind_mortality(double **inds, int i);

void get_offspring(double **inds, int N);

void nuptials(int time_steps, int N, double Tm, double Tf, double rejg,
              double mim, double mom, double gam, double mov, double a1,
              double lambd, int xdim, int ydim);
