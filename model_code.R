PrGf <- function(Tm, a1){
  vals <- 1 - exp(-(1/a1)*Tm);
  return(vals);
}

PrFf <- function(Tm, a1, a2){
  vals <- (1 - exp(-(1/a2)*Tm)) * exp(-(1/a1)*Tm);
  return(vals);
}

PrLf <- function(Tm, a1, a2){
  vals <- 1 - exp(-((1/a1) + (1/a2))*Tm);
  return(vals);
}

W_mf <- function(PrG, PrF, PrL, gamma, Tm, beta, M){
  val <- ((PrG * (1 + gamma)) + PrF + PrL) / (Tm + (sqrt(beta)/M));
  return(val);
}

W_mf_full  <- function(gamma, Tm, beta_val, M, a1, a2){
  PrG_val  <- PrGf(Tm = Tm, a1 = a1);
  PrF_val  <- PrFf(Tm = Tm, a1 = a1, a2 = a2);
  PrL_val  <- PrLf(Tm = Tm, a1 = a1, a2 = a2);
  W_mf_val <- W_mf(PrG = PrG_val, PrF = PrF_val, PrL = PrL_val, Tm = Tm, 
                   gamma = gamma, beta = beta_val, M = M);
}

W_m  <- function(mom = 1, mim = 1, mof = 1, mif = 1, M = 100, TF, TM, gamma, 
                 a1, a2, beta = 1){
  pG <- PrGf(Tm = TM, a1 = a1);
  pF <- PrFf(Tm = TM, a1 = a1, a2 = a2);
  pL <- PrLf(Tm = TM, a1 = a1, a2 = a2);
  bb <- beta;
  WW <- W_mf_full(gamma = gamma, Tm = TM, beta_val = bb, M = M, a1 = a1, 
                  a2 = a2);
  return(WW)
}

TMvals  <- seq(from = 0.001, to = 2, by = 0.001);
malefit <- W_m(mom = 1, mim = 1, mof = 1, mif = 1, M = 100, TF = 1, gamma = 0,
               a1 = 1, a2 = 2, beta = 1, TM = TMvals);
par(mar = c(5, 5, 1, 1));
plot(x = TMvals, y = malefit, type = "l", lwd = 2, cex.lab = 1.25,
     ylim = c(0, 3),
     xlab = expression(paste("Time out for males (", T[m], ")")),
     ylab = expression(paste("Male fitness (", W[m], ")")));
abline(h = 0, lty = "dotted", lwd = 0.8);

dWdT <- function(gamma, Tm, beta = 1, M = 100, a1 = 1){
  num <- M*M*(gamma*exp(Tm/a1) - gamma - 1);
  den <- (sqrt(beta) + Tm*M)^2
  val <- num/den
  return(val);
}

gavals  <- seq(from = 0.001, to = 2, by = 0.001);
malefit <- dWdT(gamma = gavals, Tm = 1, beta = 1, M = 100, a1 = 1);
par(mar = c(5, 5, 1, 1));
plot(x = gavals, y = malefit, type = "l", lwd = 2, cex.lab = 0.9,
     xlab = expression(paste("Inclusive fitness increment with gift (", gamma, ")")),
     ylab = expression(paste("Male fitness change with search time (",partialdiff,"W"[M],"/",partialdiff,"T"[M],")")));
abline(h = 0, lty = "dotted", lwd = 0.8);

Wfn <- function(TF = 1){
  return(0.5/TF);
}
WFG <- function(gamma, a1 = 1, a2 = 2, Tf = 1, TN = 1, M = 100, beta = 1, Tm){
  PrG <- 1 - exp(-(1/a1)*Tm);
  PrF <- (1 - exp(-(1/a2)*Tm))*exp(-(1/a1)*Tm);
  Wfg <- 0.5 * (1 + gamma * (PrG / (PrG + PrF)));
  prg <- 1 - exp(-TN * (M / sqrt(beta)) * (PrG + PrF));
  val <- prg * (Wfg / (Tf + TN));
  return(val);
}

gavals  <- seq(from = 0.001, to = 2, by = 0.001);
fmleL   <- rep(Wfn(TF = 1), times = length(gavals));
fmleG   <- WFG(gamma = gavals, Tm = 1, TN = 0.25)
plot(x = gavals, y = fmleG, type = "l", lwd = 2, cex.lab = 1, 
     ylim = c(0, 1.2),
     xlab = expression(paste("Offspring fitness increment with gift (", gamma, ")")),
     ylab = expression(paste("Female fitness (", W[f], ")")))
points(x = gavals, y = fmleL, type = "l", lwd = 2, col = "red");
abline(h = 0, lty = "dotted", lwd = 0.8);


gamma_star <- function(TN, M, beta, TF, a1, a2, TM){
  top1 <- exp(-(TM/a1)) * exp(-(TM/a2)) - 1;
  top2 <- exp(-(TM*(a2 + a1))/(a1*a2)) * TF + TN;
  bot1 <- TF;
  bot2 <- exp(-(TM*(a2 + a1))/(a1*a2)) - 1;
  bot3 <- exp(-(TM/a1)) - 1;
  topp <- top1 * top2;
  bott <- bot1 * bot2 * bot3;
  gst  <- -1*topp/bott;
  return(gst);
}

ts <- seq(from = 0, to = 4, by = 0.001);
yy <- gamma_star(TN = ts, M = 100, beta = 1, TF = 1, a1 = 1, a2 = 2, TM = 1);
par(mar = c(5, 5, 1, 1));
plot(x = ts, y = yy, type = "l", lwd = 2, cex.lab = 1, ylim = c(0, 8),
     xlab = expression(paste("Female additional search time (", "T"[N], ")")),
     ylab = expression(paste("Gain needed for fitness increase (", gamma, "*)")));
abline(h = 0, lty = "dotted", lwd = 0.8);

ts <- seq(from = 0.001, to = 4, by = 0.001);
yy <- gamma_star(TN = 1, M = 100, beta = 1, TF = 1, a1 = 1, a2 = 2, TM = ts);
plot(x = ts, y = yy, type = "l", lwd = 2, cex.lab = 1, ylim = c(0, 8),
     xlab = expression(paste("Male search time (", "T"[M], ")")),
     ylab = expression(paste("Gain needed for fitness increase (", gamma, "*)")));
abline(h = 0, lty = "dotted", lwd = 0.8);

#### RANDOM SIMULATIONS
T_Nv     <- runif(n = 16, min = 0, max = 10);
T_Mv     <- runif(n = 16, min = 0, max = 10);
T_Fv     <- runif(n = 16, min = 0, max = 10);
a1v      <- runif(n = 16, min = 0.5, max = 4);
a2v      <- a1v + runif(n = 40, min = 0, max = 4);
betav    <- runif(n = 16, min = 0, max = 1);
gammav   <- runif(n = 16, min = 0, max = 4);

par(mfrow = c(4, 4));
pdf("Male_dW_eg.pdf");
for(i in 1:16){
  gavals  <- seq(from = 0.001, to = 2, by = 0.001);
  malefit <- dWdT(gamma = gavals, Tm = T_Mv[i], beta = betav[i], 
                  M = 100, a1 = a1v[i]);
  plot(x = gavals, y = malefit, type = "l", lwd = 2, cex.lab = 0.9,
       xlab = "",
       ylab = "");
  abline(h = 0, lty = "dotted", lwd = 0.8);
}
dev.off();

par(mfrow = c(4, 4));
pdf("fem_W_gamma.pdf");
for(i in 1:16){
  gavals  <- seq(from = 0.001, to = 2, by = 0.001);
  fmleL   <- rep(Wfn(TF = T_Fv[i]), times = length(gavals));
  fmleG   <- WFG(gamma = gavals, Tm = T_Mv[i], TN = T_Nv[i], a1 = a1v[i],
                 a2 = a2v[i], beta = betav[i]);
  plot(x = gavals, y = fmleG, type = "l", lwd = 2, cex.lab = 1, 
       ylim = c(0, 1.2),
       xlab = expression(paste("Offspring fitness increment with gift (", gamma, ")")),
       ylab = expression(paste("Female fitness (", W[f], ")")))
  points(x = gavals, y = fmleL, type = "l", lwd = 2, col = "red");
  abline(h = 0, lty = "dotted", lwd = 0.8);
}
dev.off();

par(mfrow = c(4, 4));
pdf("fem_TN_gamma.pdf");
for(i in 1:16){
  ts <- seq(from = 0, to = 4, by = 0.001);
  yy <- gamma_star(TN = ts, M = 100, beta = betav[i], TF = T_Fv[i], a1 = a1v[i], 
                   a2 = a2v[i], TM = T_Mv[i]);
  plot(x = ts, y = yy, type = "l", lwd = 2, cex.lab = 1, ylim = c(0, 8),
       xlab = expression(paste("Female additional search time (", "T"[N], ")")),
       ylab = expression(paste("Gain needed for fitness increase (", gamma, "*)")));
  abline(h = 0, lty = "dotted", lwd = 0.8);
}
dev.off();


par(mfrow = c(4, 4));
pdf("male_TM_gammastar.pdf");
for(i in 1:16){
  ts <- seq(from = 0, to = 4, by = 0.001);
  yy <- gamma_star(TN = T_Nv[i], M = 100, beta = betav[i], TF = T_Fv[i], a1 = a1v[i], 
                   a2 = a2v[i], TM = ts);
  plot(x = ts, y = yy, type = "l", lwd = 2, cex.lab = 1, ylim = c(0, 8),
       xlab = expression(paste("Male search time (", "T"[M], ")")),
       ylab = expression(paste("Gain needed for fitness increase (", gamma, "*)")));
  abline(h = 0, lty = "dotted", lwd = 0.8);
}
dev.off();



