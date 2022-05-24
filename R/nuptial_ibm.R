################################################################################
# You can run all of the functions below, then try the following:
#
#    sim <- time_in_out(time_steps = 100)
#     
#
# It is a bit slower than I hoped, so I'm thinking I'll probably convert to C
################################################################################

time_in_out <- function(time_steps = 40, N = 1000, Tm = 0, Tf = 1, rejg = 0, 
                        mim = 1, mom = 1, gam = 1, mov = 1, a1 = 0.4, lambd = 2, 
                        xdim = 20, ydim = 20, K = 1000){
  
  ID    <- 1:N; # Individual ID
  sex   <- rbinom(n = N, size = 1, prob = 0.5); # Female (0) and male (1)
  xloc  <- sample(x = 1:xdim, size = N, replace = TRUE);
  yloc  <- sample(x = 1:xdim, size = N, replace = TRUE);
  isin  <- rep(x = 1, times = N); # Is (1) or not (0) time-in
  Tm    <- rep(x = Tm, times = N); # Male search time for a gift
  Tf    <- rep(x = Tf, times = N); # Female processing time
  gft   <- rep(x = 0, times = N); # Males have (1) or do not have (0) gift
  rejg  <- rep(x = rejg, times = N); # Female rejection Pr for no gift males
  mim   <- rep(x = mim, times = N); # Mortality rate for time-in stage
  mom   <- rep(x = mom, times = N); # Mortality rate for time-out stage
  gam   <- rep(x = gam, times = N); # Offspring increase for nuptial gift
  mov   <- rep(x = mov, times = N); # Movement parameter
  a1    <- rep(x = a1, times = N); # Search time needed to find gift
  offs  <- rep(x = 0, times = N); # Number of offspring individual has
  mum   <- rep(x = 0, times = N); # Mum ID of an individual
  dad   <- rep(x = 0, times = N); # Dad ID of an individual
  lambd <- rep(x = lambd, times = N); # Baseline female reproduction
  dead  <- rep(x = 0, times = N); # Is the individual dead?
  mate  <- rep(x = 0, times = N); # Mate ID of an individual
  
  inds <- cbind(ID, sex, xloc, yloc, isin, Tm, Tf, gft, rejg, mim, mom, gam, 
                mov, a1, offs, mum, dad, lambd, dead, mate);
  
  pop_stats <- matrix(data = NA, nrow = time_steps, ncol = dim(inds)[2]);
  ts        <- 1;
  while(time_steps > 0){
      inds            <- move_inds(inds, xdim = xdim, ydim = ydim);
      inds            <- get_offspring(inds);
      inds            <- add_offspring(inds);
      inds            <- remove_dead(inds);
      inds            <- apply_K(inds, K = K);
      pop_stats[ts, ] <- time_stats(inds);
      time_steps      <- time_steps - 1;
      ts              <- ts + 1;
      print(ts);
  }
  colnames(pop_stats) <- colnames(inds);
  return(pop_stats);
}

move_inds <- function(inds, xdim, ydim){
  N <- dim(inds)[1];
  for(i in 1:N){
    move_max_x <- inds[i, 13];
    move_rng_x <- (-move_max_x:move_max_x);
    inds[i, 3] <- inds[i, 3] + sample(x = move_rng_x, size = 1);
    move_max_y <- inds[i, 13];
    move_rng_y <- (-move_max_y:move_max_y);
    inds[i, 4] <- inds[i, 4] + sample(x = move_rng_y, size = 1);
    if(inds[i, 3] < 1){
      inds[i, 3] <- xdim + inds[i, 3];
    }
    if(inds[i, 3] > xdim){
      inds[i, 3] <- inds[i, 3] - xdim;
    }
    if(inds[i, 4] < 1){
      inds[i, 4] <- ydim + inds[i, 4];
    }
    if(inds[i, 4] > ydim){
      inds[i, 4] <- inds[i, 4] - ydim;
    }
  }
  return(inds);
}

get_offspring <- function(inds){
  N          <- dim(inds)[1];
  rand_order <- 1:dim(inds)[1] # sample(x = 1:N, size = N, replace = FALSE);
  for(i in rand_order){
    isex  <- inds[i, 2];
    ixloc <- inds[i, 3];
    iyloc <- inds[i, 4];
    iisin <- inds[i, 5];
    if(isex > 0 & iisin > 0){
      for(j in 1:N){
        jsex  <- inds[j, 2];
        jxloc <- inds[j, 3];
        jyloc <- inds[j, 4];
        jisin <- inds[j, 5];
        if(jxloc == ixloc & jyloc == iyloc & jsex == 0 & jisin > 0){
          inds <- female_male_int(inds = inds, female = j, male = i);
        }
      }
    }
    if(iisin == 0 & isex == 0){
        inds <- female_enter(inds = inds, female = i);
    }
    if(iisin == 0 & isex == 1){
        inds <- male_search(inds = inds, male = i);
    }
    inds <- ind_mortality(inds = inds, i = i);
  }
  return(inds);
}


female_male_int <- function(inds, female, male){
  has_gift <- inds[male, 8];
  rej_gift <- inds[male, 9];
  acceptml <- runif(n = 1, min = 0, max = 1);
  if(rej_gift < acceptml){
    male_add  <- inds[male, 12] * inds[male, 8];
    birth_par <- inds[female, 18] + male_add;
    offspring <- rpois(n = 1, lambda = birth_par);
    inds[female, 15] <- offspring;
    inds[female, 5]  <- 0;
    inds[male, 5]    <- 0;
    inds[male, 8]    <- 0;
    inds[female, 20] <- inds[male, 1];
  }
  return(inds);
}


female_enter <- function(inds, female){
  iisin      <- inds[female, 5];
  isex       <- inds[female, 2];
  iTf        <- inds[female, 7];
  enter_prob <- exp(-iTf);
  enter_rand <- runif(n = 1, min = 0, max = 1);
  if(enter_rand < enter_prob){
     inds[female, 5] <- 1;
  }
  return(inds);
}

male_search <- function(inds, male){
  iTm        <- inds[male, 6];
  gam        <- inds[male, 12];
  a1         <- inds[male, 14];
  isin       <- inds[male, 5];
  enter_prob <- exp(-iTm);
  enter_rand <- runif(n = 1, min = 0, max = 1);
  if(enter_rand < enter_prob){
     inds[male, 5] <- 1;
     isin                 <- 1;
  }
  gift_prob  <- 1 - exp(-(1/a1) * iTm);
  gift_rand  <- runif(n = 1, min = 0, max = 1);
  if(isin > 0 & gift_rand < gift_prob){
    inds[male, 8] <- 1;
  }
  return(inds);
}

ind_mortality <- function(inds, i){
    in_or_out <- inds[i, 5];
    rand_mort <- runif(n = 1, min = 0, max = 1);
    if(in_or_out > 0){
      in_mort <- inds[i, 10];
      mort_pr <- 1 - exp(-in_mort);
    }else{
      out_mort  <- inds[i, 11];
      mort_pr <- 1 - exp(-out_mort);
    }
    if(rand_mort < mort_pr){
       inds[i, 19] <- 1;
    }
    return(inds);
}

add_offspring <- function(inds){
  N       <- dim(inds)[1];
  traits  <- dim(inds)[2];
  tot_off <- sum(inds[,15]);
  ID_last <- inds[N, 1];
  if(tot_off > 0){
      off_mat <- matrix(data = NA, nrow = tot_off, ncol = traits);
      off_pos <- 1;
      for(mum_row in 1:N){
        while(inds[mum_row, 15] > 0){
          mum_ID  <- inds[mum_row, 1];
          dad_ID  <- inds[mum_row, 20];
          dad_row <- which(inds[,1] == dad_ID)[1];
          # Inserting the offspring traits below
          off_mat[off_pos, 1]  <- ID_last + 1;
          off_mat[off_pos, 2]  <- rbinom(n = 1, size = 1, prob = 0.5);
          off_mat[off_pos, 3]  <- inds[mum_row, 3];
          off_mat[off_pos, 4]  <- inds[mum_row, 4];
          off_mat[off_pos, 5]  <- 1;
          Tm_parents           <- 0.5 * (inds[mum_row, 6] + inds[dad_row, 6]);
          Tm_off               <- rnorm(n = 1, mean = Tm_parents, sd = 0.02);
          if(Tm_off < 0){
            Tm_off <- 0;
          }
          off_mat[off_pos, 6]  <- Tm_off;
          Tf_parents           <- 0.5 * (inds[mum_row, 7] + inds[dad_row, 7]);
          Tf_off               <- 1;
          if(Tf_off < 0){
            Tf_off <- 0;
          }
          off_mat[off_pos, 7]  <- Tf_off;
          off_mat[off_pos, 8]  <- 0;
          gf_parents           <- 0.5 * (inds[mum_row, 9] + inds[dad_row, 9]);
          gf_off               <- rnorm(n = 1, mean = gf_parents, sd = 0.02);
          if(gf_off < 0){
            gf_off <- 0;
          }
          off_mat[off_pos, 9]  <- gf_off;
          off_mat[off_pos, 10] <- inds[mum_row, 10];
          off_mat[off_pos, 11] <- inds[mum_row, 11];
          off_mat[off_pos, 12] <- inds[mum_row, 12];
          off_mat[off_pos, 13] <- inds[mum_row, 13];
          off_mat[off_pos, 14] <- inds[mum_row, 14];
          off_mat[off_pos, 15] <- 0;
          off_mat[off_pos, 16] <- mum_ID;
          off_mat[off_pos, 17] <- dad_ID;
          off_mat[off_pos, 18] <- inds[mum_row, 18];
          off_mat[off_pos, 19] <- 0;
          off_mat[off_pos, 20] <- 0;
          off_pos              <- off_pos + 1;
          ID_last              <- ID_last + 1;
          inds[mum_row, 15]    <- inds[mum_row, 15] - 1;
        }
      }
      inds_new <- rbind(inds, off_mat);
  }else{
    inds_new <- inds;
  }
  return(inds_new);
}



remove_dead <- function(inds){
  N <- dim(inds)[1];
  for(i in 1:N){
    ID <- inds[i, 1];
    if(inds[i, 19] > 0){
      living_offs <- sum(inds[,16] == ID) + sum(inds[,17] == ID);
      if(living_offs == 0){
        inds[i, 19] <- 2;
      }
    }
  }
  inds_new <- inds[inds[,19] < 2,];
  return(inds_new);
}



apply_K <- function(inds, K = 1000){
    N <- dim(inds)[1];
    if(N > K){
      to_rm    <- sample(x = 1:N, size = N - K, replace = FALSE);
      new_inds <- inds[-to_rm,];
    }else{
      new_inds <- inds;
    }
    return(new_inds);
}


time_stats <- function(inds){
  the_stats <- apply(X = inds, MARGIN = 2, FUN = mean);
  return(the_stats);
}

