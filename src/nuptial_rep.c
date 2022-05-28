/******************************************************************************/
/* Program: nuptial_rep.c
   By: Brad Duthie                                             
   Description: 
   Compile: To compile with makefile, type `make' in directory, then hit ENTER
   Run:     To run, type `./nuptial_rep' on command line, then hit ENTER      */
/******************************************************************************/
#include <time.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "nuptials.h"

int main(void){

    int i, time_steps, N, rep, xdim, ydim, K, stats, pid;
    double Tm, Tf, rejg, mim, mom, gam, mov, a1, lambd, Tm_mu, rg_mu;
    char outfile[20];
    FILE *fptr; 

    /* ======= VARIABLES BETWEEN THE Xs BELOW ADJUST MODEL PARAMETERS ========*/
    /*  XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX   */
    /* =======================================================================*/
    /* Model parameter values                                                 */
    /* =======================================================================*/
    time_steps = 1000;  /* Simulation time steps                              */
    N          = 2000;  /* Population size                                    */
    Tm         = 0.0;   /* Initial male search time                           */
    Tf         = 1.0;   /* Initial female processing time                     */
    rejg       = 0.5;   /* Initial female rejection probability               */
    mim        = 0.1;   /* Mortality rate for time-in stage                   */
    mom        = 0.1;   /* Mortality rate for time-out stage                  */
    gam        = 0.0;   /* Offspring increase for nuptial gift                */
    mov        = 1.0;   /* Movement parameter                                 */
    a1         = 0.10;  /* Search time needed to find a gift                  */
    lambd      = 8.0;   /* Baseline female reproduction                       */
    xdim       = 1;     /* dimension of x-loc landscape                       */
    ydim       = 1      /* dimension of y-loc landscape                       */
    K          = 1000;  /* Population carrying capacity                       */
    Tm_mu      = 0.02;  /* Error of offspring Tm from mean parent             */
    rg_mu      = 0.00;  /* Error of offspring rejg from mean parent           */
    /* =======================================================================*/

    /* =======================================================================*/
    /* Simulation details                                                     */
    /* =======================================================================*/
    rep    = 1000; /* Simulations run                                         */
    stats  = 0;   /* 0: end of time step, 1: each time step, 2: all inds      */
    /* =======================================================================*/
    /*  XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX   */
    /* =======================================================================*/
    srand(time(NULL) ^ (getpid()<<16)); /* Use time to generate random seed */

    i    = 0; /* Loops through different replicate simulations */

    if(stats == 0){
      pid = getpid();
      sprintf(outfile, "results_%d.csv", pid);
      fptr = fopen(outfile, "a+");
      fprintf(fptr, "mim,mom,gam,mov,a1,lambd,xdim,ydim,K,Tm_init,rg_init,\
                     Tm_mu,rg_mu,Time,Sex_ratio,Time-in,Tm,Tf,Gift,RejPr,Offs,\
                     N,Vr_Tm,Vr_Rj\n");
      fclose(fptr);
    }

    while(i < rep){

        nuptials(time_steps, N, Tm, Tf, rejg, mim, mom, gam, mov, a1, lambd, 
                 xdim, ydim, K, stats, Tm_mu, rg_mu);
                 
        i++;

        if(i % 200 == 0){
          gam += 1.0;
        }

        printf("Rep: %d of %d\n", i, rep);
    }
    return 0;
}

