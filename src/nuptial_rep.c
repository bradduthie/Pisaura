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

    int i, time_steps, N, rep, xdim, ydim;
    double Tm, Tf, rejg, mim, mom, gam, mov, a1, lambd;

    /* ======= VARIABLES BETWEEN THE Xs BELOW ADJUST MODEL PARAMETERS ========*/
    /*  XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX   */
    /* =======================================================================*/
    /* Model parameter values                                                 */
    /* =======================================================================*/
    time_steps = 40;   /* Simulation time steps                               */
    N          = 1000; /* Population size                                     */
    Tm         = 0.0;  /* Initial male search time                            */
    Tf         = 1.0;  /* Initial female processing time                      */
    rejg       = 0.0;  /* Initial female rejection probability                */
    mim        = 1.0;  /* Mortality rate for time-in stage                    */
    mom        = 1.0;  /* Mortality rate for time-out stage                   */
    gam        = 1.0;  /* Offspring increase for nuptial gift                 */
    mov        = 1.0;  /* Movement parameter                                  */
    a1         = 0.4;  /* Search time needed to find a gift                   */
    lambd      = 2.0;  /* Baseline female reproduction                        */
    xdim       = 20;   /* dimension of x-loc landscape                        */
    ydim       = 20;   /* dimension of y-loc landscape                        */
    /* =======================================================================*/

    /* =======================================================================*/
    /* Simulation details                                                     */
    /* =======================================================================*/
    rep        = 1;     /* Simulations run                                    */
    /* =======================================================================*/
    /*  XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX   */
    /* =======================================================================*/
    srand(time(NULL) ^ (getpid()<<16)); /* Use time to generate random seed */

    i    = 0; /* Loops through different replicate simulations */

    while(i < rep){

        nuptials(time_steps, N, Tm, Tf, rejg, mim, mom, gam, mov, a1, lambd, 
                 xdim, ydim);
                 
        i++;

    }
    return 0;
}






