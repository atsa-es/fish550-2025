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

# initital values
inits <- list(x0 = matrix(c(0,0), nrow = m))

# model list
dlm.list <- list(B=B, U=U, Q=Q, Z=Z, A=A, R=R)

# fit model
dlm <- MARSS(dat, model= dlm.list, inits = inits)
dlm$states

# plot
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
lines(beta+2*beta.se~data$brood_year, lty="dashed")
lines(beta-2*beta.se~data$brood_year, lty="dashed")
