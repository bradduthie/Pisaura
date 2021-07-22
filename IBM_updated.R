N    <- 1000; # Number of females and males;
Qav  <- 1; # Payoff to virigin female given no-gift male
Qfv  <- 1; # Payoff to virigin female given worthless gift male   
Qgv  <- 2; # Payoff to virgin female given genuine gift male
Qam  <- 1; # Payoff to mated female given no-gift male
Qfm  <- 1; # Payoff to mated female given worthless gift male
Qgm  <- 2; # Payoff to mated female given genuine gift male
Pav  <- 1; # Payoff to male for no gift to virgin female
Pfv  <- 2; # Payoff to male for worthless gift to virgin female 
Pgv  <- 3; # Payoff to male for geniune gift to virgin female
Pam  <- 2; # Payoff to male for no gift to mated female
Pfm  <- 1; # Payoff to male for worthless gift to mated female
Pgm  <- 0; # Payoff to male for geniune gift to mated female

# Fix population size
# Start with resident no nuptual gift
# Input Ander's parameter values
make_inds <- function(N = 100, Qav = 1, Qfv = 1, Qgv = 2, Qam = 1, Qfm = 1, 
                      Qgm = 1, Pav = 2, Pfv = 1, Pgv = 1, Pam = 1, Pfm = 1, 
                      Pgm = 1){
  
  inds     <- matrix(data = 0, nrow = N, ncol = 19);
  as_raw   <- 1.0;
  fs_raw   <- 0.0;
  gs_raw   <- 0.0;
  sums     <- as_raw + fs_raw + gs_raw;
  as       <- 1;
  fs       <- 0;
  gs       <- 0;
  
  inds[, 1]  <- c(rep(0, 0.5*N), rep(1, 0.5*N));
  inds[, 2]  <- 0;
  inds[, 3]  <- 0;
  inds[, 4]  <- 0; 
  inds[, 5]  <- as;
  inds[, 6]  <- fs;
  inds[, 7]  <- gs;
  inds[, 8]  <- Qav;
  inds[, 9]  <- Qfv; 
  inds[, 10] <- Qgv; 
  inds[, 11] <- Qam; 
  inds[, 12] <- Qfm; 
  inds[, 13] <- Qgm; 
  inds[, 14] <- Pav; 
  inds[, 15] <- Pfv; 
  inds[, 16] <- Pgv; 
  inds[, 17] <- Pam; 
  inds[, 18] <- Pfm; 
  inds[, 19] <- Pgm; 
  
  return(inds);
}

W_female <- function(a, f, g, Qav, Qfv, Qgv, Qam, Qfm, Qgm, mated){
  if(mated == 0){
    payoff <- (a * Qav) + (f * Qfv) + (g * Qgv);
  }else{
    payoff <- (a * Qam) + (f * Qfm) + (g * Qgm);
  }
  return(payoff);
}

W_male <- function(a, f, g, Pav, Pfv, Pgv, Pam, Pfm, Pgm, mated){
  if(mated == 0){
    payoff <- (a * Pav) + (f * Pfv) + (Pgv);
  }else{
    payoff <- (a * Pam) + (f * Pfm) + (Pgm);
  }
  return(payoff);
}

encounter <- function(inds, male_row, female_row){
  
  a     <- inds[male_row, 5];
  f     <- inds[male_row, 6];
  g     <- inds[male_row, 7];
  Qav   <- inds[male_row, 8];
  Qfv   <- inds[male_row, 9];
  Qgv   <- inds[male_row, 10];
  Qam   <- inds[male_row, 11];
  Qfm   <- inds[male_row, 12];
  Qgm   <- inds[male_row, 13];
  Pav   <- inds[male_row, 14];
  Pfv   <- inds[male_row, 15];
  Pgv   <- inds[male_row, 16];
  Pam   <- inds[male_row, 17];
  Pfm   <- inds[male_row, 18];
  Pgm   <- inds[male_row, 19];
  mated <- inds[female_row, 2];
  
  fem_fitness <- W_female(a, f, g, Qav, Qfv, Qgv, Qam, Qfm, Qgm, mated);
  mal_fitness <- W_male(a, f, g, Qav, Qfv, Qgv, Qam, Qfm, Qgm, mated);
  
  # One more mating has occurred
  inds[female_row, 2] <- inds[female_row, 2] + 1;
  inds[male_row, 2]   <- inds[male_row, 2]   + 1;
  
  # Insert the fitness of each
  inds[female_row, 3] <- inds[female_row, 3] + fem_fitness;
  inds[male_row, 3]   <- inds[male_row, 3]   + mal_fitness;
  
  return(inds);
}


ind_encounters <- function(inds, encounter_rate){
  
  if(encounter_rate == "N"){
      encounter_rate <- dim(inds)[1];
  }
  
  while(encounter_rate > 0){
      #sample male
      N <- dim(inds)[1];
      i <- sample(x = 1:N, size = 1);
      while(inds[i] == 0){
          i <- sample(x = 1:N, size = 1);
      }
  
      #sample female
      j <- sample(x = 1:N, size = 1);
      while(inds[j] == 1){
          j <- sample(x = 1:N, size = 1);
      }
      inds <- encounter(inds, i, j);
      
      encounter_rate <- encounter_rate - 1;
  }
  
  return(inds);
}

reproduce <- function(inds){
  
  females  <- inds[inds[,1] == 0,];
  males    <- inds[inds[,1] == 1,];
  
  female_w <- females[, 3] / sum(females[, 3]);
  male_w   <- males[, 3] / sum(males[, 3]);
  
  N        <- dim(inds)[1];
  cols     <- dim(inds)[2];
  N_af     <- dim(females)[1];
  N_am     <- dim(males)[1];
  N_f      <- floor(0.5 * N);
  N_m      <- N - N_f;
  
  off_F    <- sample(x = 1:N_af, size = N_f, replace = TRUE, prob = female_w);
  off_M    <- sample(x = 1:N_am, size = N_m, replace = TRUE, prob = male_w);
  
  new_F    <- females[off_F, ];
  new_M    <- males[off_M, ];
  
  new_inds <- matrix(data = NA, nrow = N_f + N_m, ncol = cols);
  
  new_inds[1:N_f, ]                 <- new_F;
  new_inds[(N_f + 1):(N_f + N_m), ] <- new_M;
  
  new_inds[, 2:3] <- 0;
  
  return(new_inds);
}

mutation <- function(inds){
  
  N       <- dim(inds)[1];
  mu_dat  <- rbinom(n = 3 * N, size = 1, prob = 0.01);
  mu_mat  <- matrix(data = mu_dat, nrow = N);
  mu_vals <- rnorm(n = 3 * N, mean = 0, sd = 0.04);
  mv_mat  <- matrix(data = mu_vals, nrow = N);
  change  <- mv_mat * mu_mat;
  
  inds[, 5:7]         <- inds[, 5:7] + change;
  inds[inds[, 5] < 0] <- 0;
  inds[inds[, 6] < 0] <- 0;
  inds[inds[, 7] < 0] <- 0;
  
  col_sum <- apply(X = inds[, 5:7], MARGIN = 1, FUN = sum);
  
  inds[, 5] <- inds[, 5] / col_sum;
  inds[, 6] <- inds[, 6] / col_sum;
  inds[, 7] <- inds[, 7] / col_sum;
  
  return(inds);
}


run_sim <- function(N = 1000, gens = 2000, print_gen = TRUE, Qav = 1, Qfv = 1, 
                    Qgv = 2, Qam = 1, Qfm = 1, Qgm = 1, Pav = 2, Pfv = 1, 
                    Pgv = 1, Pam = 1, Pfm = 1, Pgm = 1, encounter_rate = 200){
  
    inds     <- make_inds(N = N, Qav, Qfv, Qgv, Qam, Qfm, Qgm, Pav, Pfv, Pgv, 
                          Pam, Pfm, Pgm);
    ind_hist <- matrix(data = NA, nrow = gens, ncol = 5);
    for(i in 1:gens){
        inds           <- ind_encounters(inds, encounter_rate);
        inds           <- reproduce(inds);
        inds           <- mutation(inds);
        ind_hist[i, 1] <- i; # Get just male freqs
        ind_hist[i, 2] <- mean(inds[inds[,1] == 1, 5]);
        ind_hist[i, 3] <- mean(inds[inds[,1] == 1, 6]);
        ind_hist[i, 4] <- mean(inds[inds[,1] == 1, 7]);
        ind_hist[i, 5] <- dim(inds)[1];
        if(dim(inds)[1] < 40){
          break;
        }
        if(print_gen == TRUE){
            print(ind_hist[i, ]);
        }
    }
    return(ind_hist);
}



# Need to set costs
c2 <- runif(1,0,1);
c1 <- runif(1,0,c2);
c3 <- runif(1,0,c2);

# Adding in empirical parameter values
P_av <- 0.3276 - c3;
P_fv <- 0.8966 - c2;
P_gv <- 1.0000 - c1;
P_am <- 0.0000 - c3;
P_fm <- 0.8966 - c2;
P_gm <- 1.0000 - c1;

# Need to set b values for females
b2 <- runif(1,0,1);
b1 <- runif(1,0,b2);
b3 <- runif(1,0,b2);

# Adding in empirical parameter values
Q_av <- 0.477 + b3;
Q_fv <- 0.477 + b2;
Q_gv <- 1.000 + b2;
Q_am <- 0.477 + b3;
Q_fm <- 0.477 + b2;
Q_gm <- 1.000 + b1;

replicate_sims <- function(N = 1000, gens = 200, print_end = TRUE, reps = 10,
                           Q_av = 0.477, Q_fv = 0.477, Q_gv = 1.000, 
                           Q_am = 0.477, Q_fm = 0.477, Q_gm = 1.000, 
                           P_av = 0.3276, P_fv = 0.8966, P_gv = 1.0000, 
                           P_am = 0.0000, P_fm = 0.8966, P_gm = 1.0, 
                           erl = 10, eru = 400, print_gen = FALSE, 
                           res_file = "results/results.csv", 
                           filegen = "results/gens"){
  
  rep_params  <- NULL;
  rep_results <- NULL;
  csv_head    <- c("sim,", "N,", "gens,", "Qav,", "Qfv,", "Qgv,", "Qam,", 
                   "Qfm,", "Qgm,", "Pav,", "Pfv,", "Pgv,", "Pam,", "Pfm,", 
                   "Pgm,", "gen,", "F_a,", "F_f,", "F_g", "N \n");
  cat(csv_head, file = res_file);
  
  for(i in 1:reps){
      
      encounter_rate  <- sample(x = erl:eru, size = 1);
    
      c2 <- runif(1,0,1);
      c1 <- runif(1,0,c2);
      c3 <- runif(1,0,c2);
      
      b2 <- runif(1,0,1);
      b1 <- runif(1,0,b2);
      b3 <- runif(1,0,b2);
    
      Qav <- Q_av + b3;
      Qfv <- Q_fv + b2;
      Qgv <- Q_gv + b2;
      Qam <- Q_am + b3;
      Qfm <- Q_fm + b2;
      Qgm <- Q_gm + b1;
      
      Pav <- P_av - c3;
      Pfv <- P_fv - c2;
      Pgv <- P_gv - c1;
      Pam <- P_am - c3;
      Pfm <- P_fm - c2;
      Pgm <- P_gm - c1;
    
      parameters       <- c(N, gens, Qav, Qfv, Qgv, Qam, Qfm, Qgm, Pav, Pfv, 
                            Pgv, Pam, Pfm, Pgm);  
      sim_res          <- run_sim(N = N, gens = gens, print_gen = print_gen, 
                                  Qav, Qfv, Qgv, Qam, Qfm, Qgm, Pav, Pfv, Pgv, 
                                  Pam, Pfm, Pgm , encounter_rate);
      end_results      <- c(i, parameters, sim_res[gens, ]);
      
      cat(end_results, file = res_file, append = TRUE, sep = ",");
      cat("\n", file = res_file, append = TRUE);
      
      sim_res_file <- paste(filegen, i, ".csv", sep = "");
      write.csv(sim_res, file = sim_res_file, row.names = FALSE);
      if(print_end == TRUE){
        print(sim_res[gens, ]);
      }
  }
  all_results <- list(rep_params, rep_results);
  return(all_results);
}

GEN  <- 12000;
REP  <- 1000;
sims <- replicate_sims(gens = GEN, reps = REP, print_gen = TRUE);


# See how far everything has moved
# chg <- matrix(data = 0, nrow = REP, ncol = 5);
# for(i in 1:REP){
#    chg[i,] <- sims[[2]][[i]][GEN,];
#}

# Plot the change
#for(i in 1:REP){
#    file <- paste("notebook/img/sim_", i, ".png", sep = "");
#    png(filename = file)
#    sim <- sims[[2]][[i]];
#    plot(sim[,1], sim[,2], type = "l", lwd = 2, col = "black", ylim = c(0, 1));
#    points(sim[,1], sim[,3], type = "l", lwd = 2, col = "red");
#    points(sim[,1], sim[,4], type = "l", lwd = 2, col = "blue");
#    dev.off();
#}

