library(atsalibrary)

data(KvichakSockeye, package="atsalibrary")
data <- KvichakSockeye

# transform data
data$y <- log(data$recruits/data$spawners)
data <- data[!is.na(data$spawners),]

years <- data$brood_year
TT <- length(years)
dat <- matrix(data$y, nrow=1)

s <- data$spawners
s_z <- matrix((s - mean(s))/sqrt(var(s)), nrow = 1)
m <- dim(s_z)[1] + 1

# build MARSS model
# process
B <- diag(m)
U <- matrix(0, nrow=m, ncol=1)
Q <- matrix(list(0), m, m)
diag(Q) <- c("q.alpha", "q.beta")

# obs
Z <- array(NA, c(1, m, TT))
Z[1,1,] <- rep(1, TT)
Z[1,2,] <- s_z
A <- matrix(0)
R <- matrix("r")

# initital values & controls
inits <- list(x0 = matrix(c(0,0), nrow = m))
con_list <- list(maxit = 2146)
  # 2146 seems to be the minimum number of iterations at which all beta parameter varainces converge to zero

# model list
dlm.list <- list(B=B, U=U, Q=Q, Z=Z, A=A, R=R)

# fit model
dlm <- MARSS(dat, model= dlm.list, inits = inits, method = "kem", control = con_list)
dlm$states

## plot
alpha <- as.numeric(dlm$states[1,])
beta <- as.numeric(dlm$states[2,])
alpha.se <- as.numeric(dlm$states.se[1,])
beta.se <- as.numeric(dlm$states.se[2,])

# alpha
plot(alpha~data$brood_year,type='l', ylim=c(-3,3), xlim= c(1956, 2005))
lines(alpha+1.96*alpha.se~data$brood_year, lty="dashed")
lines(alpha-1.96*alpha.se~data$brood_year, lty="dashed")
# beta
plot(beta~data$brood_year,type='l', ylim=c(-0.5, 0.5),xlim= c(1956, 2005))
lines(beta+1.96*beta.se~data$brood_year, lty="dashed")
lines(beta-1.96*beta.se~data$brood_year, lty="dashed")

## covariate model build
# winter pdo
D <- matrix("pdo", 1, 1)
pdo_w <- as.data.frame(matrix(data$pdo_winter_t2, nrow=1, byrow=TRUE))
pdo_w <- data.matrix(pdo_w)

dlmPw.list <- list(B=B, U=U, Q=Q, Z=Z, A=A, R=R, D=D, d=pdo_w)

dlmPw <- MARSS(dat, model= dlmPw.list, inits = inits, method = "kem", control = con_list)
dlmPw$states

## plot
alphaPw <- as.numeric(dlmPw$states[1,])
betaPw <- as.numeric(dlmPw$states[2,])
alphaPw.se <- as.numeric(dlmPw$states.se[1,])
betaPw.se <- as.numeric(dlmPw$states.se[2,])

# alpha
plot(alphaPw~data$brood_year,type='l', ylim=c(-3,3), xlim= c(1956, 2005))
lines(alphaPw+1.96*alphaPw.se~data$brood_year, lty="dashed")
lines(alphaPw-1.96*alphaPw.se~data$brood_year, lty="dashed")
# beta
plot(betaPw~data$brood_year,type='l', ylim=c(-0.5, 0.5),xlim= c(1956, 2005))
lines(betaPw+1.96*betaPw.se~data$brood_year, lty="dashed")
lines(betaPw-1.96*betaPw.se~data$brood_year, lty="dashed")

# summer pdo
D <- matrix("pdo", 1, 1)
pdo_s <- as.data.frame(matrix(data$pdo_summer_t2, nrow=1, byrow=TRUE))
pdo_s <- data.matrix(pdo_s)

dlmPs.list <- list(B=B, U=U, Q=Q, Z=Z, A=A, R=R, D=D, d=pdo_s)

dlmPs <- MARSS(dat, model= dlmPs.list, inits = inits, method = "kem", control = con_list)
dlmPs$states

## plot
alphaPs <- as.numeric(dlmPs$states[1,])
betaPs <- as.numeric(dlmPs$states[2,])
alphaPs.se <- as.numeric(dlmPs$states.se[1,])
betaPs.se <- as.numeric(dlmPs$states.se[2,])

# alpha
plot(alphaPs~data$brood_year,type='l', ylim=c(-3,3), xlim= c(1956, 2005))
lines(alphaPs+1.96*alphaPs.se~data$brood_year, lty="dashed")
lines(alphaPs-1.96*alphaPs.se~data$brood_year, lty="dashed")
# beta
plot(betaPs~data$brood_year,type='l', ylim=c(-0.5, 0.5),xlim= c(1956, 2005))
lines(betaPs+1.96*betaPs.se~data$brood_year, lty="dashed")
lines(betaPs-1.96*betaPs.se~data$brood_year, lty="dashed")

# winter and summer pdo
D <- matrix(c("pdoW", "pdoS"), 2, 1)
pdo_b <- rbind(pdo_w, pdo_s)
pdo_b <- data.matrix(pdo_b)

dlmPb.list <- list(B=B, U=U, Q=Q, Z=Z, A=A, R=R, D=D, d=pdo_b)

dlmPb <- MARSS(dat, model= dlmPb.list, inits = inits, method = "kem", control = con_list)
dlmPb$states

## plot
alphaPb <- as.numeric(dlmPb$states[1,])
betaPb <- as.numeric(dlmPb$states[2,])
alphaPb.se <- as.numeric(dlmPb$states.se[1,])
betaPb.se <- as.numeric(dlmPb$states.se[2,])

# alpha
plot(alphaPb~data$brood_year,type='l', ylim=c(-3,3), xlim= c(1956, 2005))
lines(alphaPb+1.96*alphaPb.se~data$brood_year, lty="dashed")
lines(alphaPb-1.96*alphaPb.se~data$brood_year, lty="dashed")
# beta
plot(betaPb~data$brood_year,type='l', ylim=c(-0.5, 0.5),xlim= c(1956, 2005))
lines(betaPb+1.96*betaPb.se~data$brood_year, lty="dashed")
lines(betaPb-1.96*betaPb.se~data$brood_year, lty="dashed")

## AICc
# an empty list to collect outputs
out_list <- list()
# Add each model's name and AICc 
out_list[[1]] <- data.frame(Model = "no covariates", AICc = dlm$AICc)
out_list[[2]] <- data.frame(Model = "winter PDO", AICc = dlmPw$AICc)
out_list[[3]] <- data.frame(Model = "summer PDO", AICc = dlmPs$AICc)
out_list[[4]] <- data.frame(Model = "winter and summer PDO", AICc = dlmPb$AICc)
out_list

## big plots?
# alpha
plot(alpha~data$brood_year,type='l', ylim=c(-3,3), xlim= c(1956, 2005))
lines(alpha+1.96*alpha.se~data$brood_year, lty="dashed")
lines(alpha-1.96*alpha.se~data$brood_year, lty="dashed")

lines(alphaPw~data$brood_year, col = "red")
lines(alphaPw+1.96*alphaPw.se~data$brood_year, lty="dashed", col = "red")
lines(alphaPw-1.96*alphaPw.se~data$brood_year, lty="dashed", col = "red")

lines(alphaPs~data$brood_year, col = "blue")
lines(alphaPs+1.96*alphaPs.se~data$brood_year, lty="dashed", col = "blue")
lines(alphaPs-1.96*alphaPs.se~data$brood_year, lty="dashed", col = "blue")

lines(alphaPb~data$brood_year, col = "green")
lines(alphaPb+1.96*alphaPb.se~data$brood_year, lty="dashed", col = "green")
lines(alphaPb-1.96*alphaPb.se~data$brood_year, lty="dashed", col = "green")

# beta
plot(beta~data$brood_year,type='l', ylim=c(-0.5,0.5), xlim= c(1956, 2005))
lines(beta+1.96*beta.se~data$brood_year, lty="dashed")
lines(beta-1.96*beta.se~data$brood_year, lty="dashed")

lines(betaPw~data$brood_year, col = "red")
lines(betaPw+1.96*betaPw.se~data$brood_year, lty="dashed", col = "red")
lines(betaPw-1.96*betaPw.se~data$brood_year, lty="dashed", col = "red")

lines(betaPs~data$brood_year, col = "blue")
lines(betaPs+1.96*betaPs.se~data$brood_year, lty="dashed", col = "blue")
lines(betaPs-1.96*betaPs.se~data$brood_year, lty="dashed", col = "blue")

lines(betaPb~data$brood_year, col = "green")
lines(betaPb+1.96*betaPb.se~data$brood_year, lty="dashed", col = "green")
lines(betaPb-1.96*betaPb.se~data$brood_year, lty="dashed", col = "green")
