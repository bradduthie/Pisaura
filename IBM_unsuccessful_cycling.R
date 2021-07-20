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


make_inds <- function(N = 100, Qav = 1, Qfv = 1, Qgv = 2, Qam = 1, Qfm = 1, 
                      Qgm = 1, Pav = 2, Pfv = 1, Pgv = 1, Pam = 1, Pfm = 1, 
                      Pgm = 1){
  
  inds     <- matrix(data = 0, nrow = N, ncol = 19);
  as_raw   <- runif(n = N, min = 0, max = 1);
  fs_raw   <- runif(n = N, min = 0, max = 1);
  gs_raw   <- runif(n = N, min = 0, max = 1);
  sums     <- as_raw + fs_raw + gs_raw;
  as       <- as_raw / sums;
  fs       <- fs_raw / sums;
  gs       <- gs_raw / sums;
  
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
  New_N    <- floor(sum(females[, 3])); # Aug. with female payoffs
  cols     <- dim(inds)[2];
  N_af     <- dim(females)[1];
  N_am     <- dim(males)[1];
  N_f      <- floor(0.5 * New_N);
  N_m      <- New_N - N_f;
  
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


replicate_sims <- function(N = 1000, gens = 200, print_end = TRUE, reps = 10,
                           Qav = 2, Qfv = 2, Qgv = 3, Qam = 0, Qfm = 0, 
                           Qgm = 1, Pav = 1, Pfv = 2, Pgv = 4, Pam = 3, Pfm = 2, 
                           Pgm = 0, encounter_rate = 200, print_gen = FALSE){
  
  rep_results <- NULL;
  for(i in 1:reps){
      sim_res          <- run_sim(N = N, gens = gens, print_gen = print_gen, 
                                  Qav, Qfv, Qgv, Qam, Qfm, Qgm, Pav, Pfv, Pgv, 
                                  Pam, Pfm, Pgm , encounter_rate);
      rep_results[[i]] <- sim_res;
      if(print_end == TRUE){
        print(sim_res[gens, ]);
      }
  }
  return(rep_results);
}

sims <- NULL;
for(i in 1:20){
  sims[[i]] <- test_sim <- replicate_sims(gens = 240, reps = 1, 
                                          encounter_rate = "N", 
                                          print_gen = TRUE);
}






Rep  <- 10;
Gens <- 200;
sim  <- replicate_sims(gens = Gens, reps = Rep, encounter_rate = "N");
res  <- matrix(data = NA, nrow = Rep, ncol = 5)
for(i in 1:Rep){
  res[i,]   <- sim[[i]][Gens,];
  res[i, 1] <- i;
}


