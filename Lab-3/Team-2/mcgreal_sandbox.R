## load MARSS
library(MARSS)
library(panelr)
library(tidyverse)
library(zoo)

## load the data (there are 3 datasets contained here)
data(lakeWAplankton, package = "MARSS")
all_dat <- as.data.frame(lakeWAplanktonTrans)

# transform date
all_dat$Date <- as.yearmon(paste(all_dat$Year, all_dat$Month), "%Y %m")

# plot data
for ( i in seq(3,length( all_dat )-1,1) ) plot(all_dat[,i], x=all_dat$Date, ylab=names(all_dat[i]),type="l")

# dropping years pre-1980 and critters with poor data
yr_frst <- 1985
data <- subset(all_dat, Year  >= yr_frst)
data <- data[-c(9, 18)]

# different lists of variables
phytoclimate <- c("Temp", 
                  "TP", 
                  "pH")
zooclimate <- c("Temp", 
                "pH")
phytoplankton <- c("Cryptomonas", 
                   "Diatoms", 
                   "Greens",
                   "Unicells", 
                   "Other.algae")
zooplankton <- c("Conochilus", 
                 "Cyclops", 
                 "Daphnia",
                 "Diaptomus", 
                 "Non.daphnid.cladocerans",
                 "Non.colonial.rotifers")
  # only including grazers

# get only the phytoplankton
dates <- data$Date
data_phyto <- t(data[, phytoplankton])
colnames(data_phyto) <- dates

# get phytoplankton climate covariates
data_phytoclimate <- t(data[, phytoclimate])
colnames(data_phytoclimate) <- dates

# monthly factor covariate matrix
month_cov <- matrix(0, 12, 120)
month.abb <- unique(data$Month)
monrow <- match(data$Month, month.abb)[1:120]
monrow
month_cov[cbind(monrow,1:120)] <- 1
month_cov[,1:24]

# stack up the old covariate matrix
phyto.c <- rbind(month_cov, data_phytoclimate)

# MARSS models for fitting phytoplankton data to cover gaps
# compare seasonal v. climate covariates

# build model - seasonal
phyto_month.list <- list(
  B = "identity",
  U = "zero",
  Q = "diagonal and unequal",
  Z = "identity",
  A = "zero",
  R = "diagonal and equal",
  c = month_cov,
  C= "unconstrained",
  x0 = "unequal",
  V0 = "zero",
  tinitx = 0
)

# build model - climate
phyto_climate.list <- list(
  B = "identity",
  U = "zero",
  Q = "diagonal and unequal",
  Z = "identity",
  A = "zero",
  R = "diagonal and equal",
  c = data_phytoclimate,
  C= "unconstrained",
  x0 = "unequal",
  V0 = "zero",
  tinitx = 0
)

# model control
con_list <- list(maxit = 3000, allow.degen = TRUE)

# modeling - seasonal
ptm <- proc.time()
if(!file.exists("Lab-3/Team-2/data/phyto_month.rds")){
  ssm.phyto_month <- MARSS(data_phyto, model = phyto_month.list, method = "kem", control = con_list)
  saveRDS(ssm.phyto_month, file="Lab-3/Team-2/data/phyto_month.rds")
}
proc.time()[3] - ptm
# load in ssm.phyto_month
ssm.phyto_month <- readRDS(file="Lab-3/Team-2/data/phyto_month.rds")

# modeling - climate
ptm <- proc.time()
if(!file.exists("Lab-3/Team-2/data/phyto_climate.rds")){
  ssm.phyto_climate <- MARSS(data_phyto, model = phyto_climate.list, method = "kem", control = con_list)
  saveRDS(ssm.phyto_climate, file="Lab-3/Team-2/data/phyto_climate.rds")
}
proc.time()[3] - ptm
# load in ssm.phyto_climate
ssm.phyto_climate <- readRDS(file="Lab-3/Team-2/data/phyto_climate.rds")

# model comparison
# an empty list to collect outputs
out_list <- list()

# Add each model's name and AICc 
out_list[[1]] <- data.frame(Model = "seasonal", AICc = ssm.phyto_month$AICc)
out_list[[2]] <- data.frame(Model = "climate", AICc = ssm.phyto_climate$AICc)

# Combine all into one dataframe
out <- do.call(rbind, out_list)
out
  # seasonal model fits better than climate

ssm.phyto <- ssm.phyto_month

# grabbing model estimates
states <- ssm.phyto$states
statesSE <- ssm.phyto$states.se

states <- as.data.frame(states)
statesSE <- as.data.frame(statesSE)

states_long <- states %>% 
  pivot_longer("V1":"V120", names_to="Date", values_to="fitted")
states_long <- states_long %>% 
  mutate(Date = rep(dates, 5))

states_long <- states_long %>% 
  mutate(state = c(rep(phytoplankton[1], 120),
                   rep(phytoplankton[2], 120),
                   rep(phytoplankton[3], 120),
                   rep(phytoplankton[4], 120),
                   rep(phytoplankton[5], 120)
  ))

statesSE_long <- statesSE %>% 
  pivot_longer("V1":"V120", names_to="Date", values_to="se")

states_long <- states_long %>% 
  mutate(se = statesSE_long$se)

states_long$lb <- states_long$fitted - states_long$se
states_long$ub <- states_long$fitted + states_long$se

# save long state estimates
save(states_long, file="Lab-3/Team-2/data/ssm.phyto_long.Rda")

plot <- ggplot(data = states_long, aes(x = Date, y = fitted, group = state)) +
  geom_line() +
  labs(x = "", 
       y="State Estimate",
       title='State-space estimates of phyotplankton abundance in Lake Washington') +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  theme_classic() +
  theme(legend.position="bottom") + 
  scale_x_discrete(breaks = c(1980, 1983, 1986, 1989, 1992, 1995))
plot

## alternative approach - phytoplankton as representative of one underlying state
# MARSS models for fitting phytoplankton data to cover gaps
# compare seasonal v. climate covariates
# build model - seasonal
phyto_month.list <- list(
  B = "identity",
  U = "zero",
  Q = "diagonal and unequal",
  Z = matrix(rep(1, nrow(data_phyto))),
  A = "zero",
  R = "diagonal and equal",
  c = month_cov,
  C= "unconstrained",
  x0 = "unequal",
  V0 = "zero",
  tinitx = 0
)

# build model - climate
phyto_climate.list <- list(
  B = "identity",
  U = "zero",
  Q = "diagonal and unequal",
  Z = matrix(rep(1, nrow(data_phyto))),
  A = "zero",
  R = "diagonal and equal",
  c = data_phytoclimate,
  C= "unconstrained",
  x0 = "unequal",
  V0 = "zero",
  tinitx = 0
)

# modeling - seasonal
ptm <- proc.time()
if(!file.exists("Lab-3/Team-2/data/phyto_month_1s.rds")){
  ssm.phyto_month <- MARSS(data_phyto, model = phyto_month.list, method = "kem", control = con_list)
  saveRDS(ssm.phyto_month, file="Lab-3/Team-2/data/phyto_month_1s.rds")
}
proc.time()[3] - ptm
# load in ssm.phyto_month
ssm.phyto_month <- readRDS(file="Lab-3/Team-2/data/phyto_month_1s.rds")

# modeling - climate
ptm <- proc.time()
if(!file.exists("Lab-3/Team-2/data/phyto_climate_1s.rds")){
  ssm.phyto_climate <- MARSS(data_phyto, model = phyto_climate.list, method = "kem", control = con_list)
  saveRDS(ssm.phyto_climate, file="Lab-3/Team-2/data/phyto_climate_1s.rds")
}
proc.time()[3] - ptm
# load in ssm.phyto_climate
ssm.phyto_climate <- readRDS(file="Lab-3/Team-2/data/phyto_climate_1s.rds")

# model comparison
# an empty list to collect outputs
out_list <- list()

# Add each model's name and AICc 
out_list[[1]] <- data.frame(Model = "seasonal", AICc = ssm.phyto_month$AICc)
out_list[[2]] <- data.frame(Model = "climate", AICc = ssm.phyto_climate$AICc)

# Combine all into one dataframe
out <- do.call(rbind, out_list)
out
# seasonal model fits better than climate

ssm.phyto <- ssm.phyto_month

# grabbing model estimates
states <- ssm.phyto$states
statesSE <- ssm.phyto$states.se

states <- as.data.frame(states)
statesSE <- as.data.frame(statesSE)

states_long <- states %>% 
  pivot_longer("V1":"V120", names_to="Date", values_to="fitted")
states_long <- states_long %>% 
  mutate(Date = dates)

states_long <- states_long %>% 
  mutate(state = c(rep(phytoplankton[1], 120)
  ))

statesSE_long <- statesSE %>% 
  pivot_longer("V1":"V120", names_to="Date", values_to="se")

states_long <- states_long %>% 
  mutate(se = statesSE_long$se)

states_long$lb <- states_long$fitted - states_long$se
states_long$ub <- states_long$fitted + states_long$se

# save long state estimates
save(states_long, file="Lab-3/Team-2/data/ssm.phyto_long_1s.Rda")

plot <- ggplot(data = states_long, aes(x = Date, y = fitted, group = state)) +
  geom_line() +
  labs(x = "", 
       y="State Estimate",
       title='State-space estimates of phyotplankton abundance in Lake Washington') +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  theme_classic() +
  theme(legend.position="bottom") + 
  scale_x_continuous(breaks = c(1980, 1983, 1986, 1989, 1992, 1995))
plot

# get only zooplankton
data_zoo <- t(data[, zooplankton])
colnames(data_zoo) <- dates

# get zooplankton climate covariates
data_zooclimate <- t(data[, zooclimate])

# stack up the old covariate matrix
zoo.d <- rbind(states_long$fitted, data_zooclimate)
rownames(zoo.d) <- c("Phyto", "Temp", "pH")
print(cor(t(zoo.d)))
  # not super highly correlated

# dynamic factor anaylsis - zooplankton

# preparing to loop
## empty lists
# loadings <- list()
aicc_list <- list()

# looping
for(i in 1:(nrow(data_zoo)-1)){
  
  ## loading matrices
  NN <- nrow(data_zoo)
  MM <- i
  ZZ <- matrix(paste0("z", seq(NN*MM)), NN, MM)
  ZZ[upper.tri(ZZ)] <- 0
  # loadings[[i]] <- assign(paste0("ZZ", i), ZZ)


  ## build model
  zoo.list <- list(
    B = "identity",
    U = "zero",
    Q = "identity",
    # Z = loadings[[i]],
    Z = ZZ,
    A = "zero",
    R = "diagonal and unequal",
    d = zoo.d,
    D = "unconstrained",
    x0 = "equal",
    V0 = "zero",
    tinitx = 0
  )

  ## modeling
  if(!file.exists(paste("Lab-3/Team-2/data/zoo_", i, ".rds", sep = ""))){
    ssm <- MARSS(data_zoo, model = zoo.list, method = "kem", control = con_list)
    saveRDS(ssm, file=paste("Lab-3/Team-2/data/zoo_", i, ".rds", sep = ""))
  }
  # load in ssm.phyto_climate
  ssm <- readRDS(file=paste("Lab-3/Team-2/data/zoo_", i, ".rds", sep = ""))
  
  ## model comparison
  # Add each model's name and AICc 
  aicc_list[[i]] <- data.frame(Model = paste("zoo_", i, sep = ""), AICc = ssm$AICc)
}

# Combine all into one dataframe
aicc <- do.call(rbind, aicc_list)
aicc
  # per AICc, the model with 4 underlying factors displays the best fit

ssm <- readRDS(file="Lab-3/Team-2/data/zoo_4.rds")

# checking different levels of covariates
d.phyto <- t(as.matrix(zoo.d[1,]))
d.temp <- t(as.matrix(zoo.d[2,]))
d.ph <- t(as.matrix(zoo.d[3,]))
d.phytemp <- as.matrix(zoo.d[1:2,])
d.phyph <- as.matrix(zoo.d[c(1,3),])
d.tempph <- as.matrix(zoo.d[2:3,])
d.all <- zoo.d

# # loadings matrix - 5 'states'
# Z4 <- matrix(paste0("z", seq(NN*4)), NN, 4)
# Z4[upper.tri(Z4)] <- 0

# lil lists
covaicc_list <- list()
cov_list <- list(d.phyto, d.temp, d.ph, d.phytemp, d.phyph, d.tempph, d.all)

# # looping
# for(i in 1:length(cov_list)){
#   ## build model
#   zoo.list <- list(
#     B = "identity",
#     U = "zero",
#     Q = "identity",
#     Z = Z5,
#     A = "zero",
#     R = "diagonal and unequal",
#     d = cov_list[[i]],
#     D = "unconstrained",
#     x0 = "equal",
#     V0 = "zero",
#     tinitx = 0
#   )
#   
#   ## modeling
#   ptm <- proc.time()
#   if(!file.exists(paste("Lab-3/Team-2/data/cov_", i, ".rds", sep = ""))){
#     ssm <- MARSS(data_zoo, model = zoo.list, method = "kem", control = con_list)
#     saveRDS(ssm, file=paste("Lab-3/Team-2/data/cov_", i, ".rds", sep = ""))
#   }
#   proc.time()[3] - ptm
#   # load in ssm.phyto_climate
#   ssm <- readRDS(file=paste("Lab-3/Team-2/data/cov_", i, ".rds", sep = ""))
#   
#   ## model comparison
#   # Add each model's name and AICc 
#   covaicc_list[[i]] <- data.frame(Model = paste("cov_", i, sep = ""), AICc = ssm$AICc)
# }

mod_list = list(m = 4, R = "diagonal and unequal")
for(i in 1:length(cov_list)){
  if(!file.exists(paste("Lab-3/Team-2/data/cov_", i, ".rds", sep = ""))){
    dfa <- MARSS(data_zoo, model = mod_list, form = "dfa", z.score = FALSE,
                    control = con_list, covariates = cov_list[[i]])
    saveRDS(dfa, file=paste("Lab-3/Team-2/data/cov_", i, ".rds", sep = ""))
  }
  # load in ssm.phyto_climate
  dfa <- readRDS(file=paste("Lab-3/Team-2/data/cov_", i, ".rds", sep = ""))
  covaicc_list[[i]] <- data.frame(Model = paste("cov_", i, sep = ""), AICc = dfa$AICc)
}

# Combine all into one dataframe
covaicc <- do.call(rbind, covaicc_list)
covaicc
  # per AICc, the full model (all covariates) with 4 underlying factors displays the best fit
ssm

##########################STUFF COMING FROM MARK'S CODE############################
# the ro-to
Z_est <- coef(ssm, type = "matrix")$Z
H_inv <- varimax(Z_est)$rotmat
Z_rot <- Z_est %*% H_inv
proc_rot <- solve(H_inv) %*% ssm$states
Z_est
Z_rot

# plotting estimated factors and loadings (?)
# getting into spaces where I don't intuitively understand exactly what's going on...

## plot labels
ylbl <- zooplankton
w_ts <- seq(dim(data_zoo)[2])
minZ <- 0
N_ts <- dim(data_zoo)[1]
clr <- c("brown2", "blue4", "darkgreen", "darkred", "purple", "gold", "orange3", "cyan3")

## set up plot area
layout(matrix(c(1:10), nrow(proc_rot), 2), widths = c(2,1))
par(mai = c(0.5, 0.5, 0.5, 0.1), omi = c(0, 0, 0, 0))

## plot the processes
for(i in 1:nrow(proc_rot)) {
  ylm <- c(-1, 1) * max(abs(proc_rot[i,]))
  ## set up plot area
  plot(w_ts,proc_rot[i,], type = "n", bty = "L",
       ylim = ylm, xlab = "", ylab = "", xaxt = "n")
  ## draw zero-line
  abline(h = 0, col = "gray")
  ## plot trend line
  lines(w_ts, proc_rot[i,], lwd = 2)
  lines(w_ts, proc_rot[i,], lwd = 2)
  ## add panel labels
  mtext(paste("State",i), side = 3, line = 0.5)
  axis(1, 12 * (0:dim(data_zoo)[2]) + 1, yr_frst + 0:dim(data_zoo)[2])
}

## plot the loadings
ylm <- c(-1, 1) * max(abs(Z_rot))
for(i in 1:nrow(proc_rot)) {
  plot(x = c(1:N_ts)[abs(Z_rot[,i])>minZ],
       y = as.vector(Z_rot[abs(Z_rot[,i])>minZ,i]),
       type = "h",
       lwd = 2, xlab = "", ylab = "", xaxt = "n", ylim = ylm,
       xlim = c(0.5, N_ts + 0.5), col = clr)
  for(j in 1:N_ts) {
    if(Z_rot[j,i] > minZ) {text(j, -0.03, ylbl[j], srt = 90, adj = 1, cex = 1.2, col = clr[j])}
    if(Z_rot[j,i] < -minZ) {text(j, 0.03, ylbl[j], srt = 90, adj = 0, cex = 1.2, col = clr[j])}
    abline(h = 0, lwd = 1.5, col = "gray")
  } 
  mtext(paste("Factor loadings on state", i), side = 3, line = 0.5)
}

# ccf plot
## set up plotting area
layout(1)
par(mai = c(0.9,0.9,0.1,0.1))
## plot CCF's
ccf(proc_rot[1,],proc_rot[2,], lag.max = 12, main="")

# plotting data and model fit
get_DFA_fits <- function(MLEobj, dd = NULL, alpha = 0.05) {
  ## empty list for results
  fits <- list()
  ## extra stuff for var() calcs
  Ey <- MARSS:::MARSShatyt(MLEobj)
  ## model params
  ZZ <- coef(MLEobj, type="matrix")$Z
  ## number of obs ts
  nn <- dim(Ey$ytT)[1]
  ## number of time steps
  TT <- dim(Ey$ytT)[2]
  ## get the inverse of the rotation matrix
  H_inv <- varimax(ZZ)$rotmat
  ## check for covars
  if(!is.null(dd)) {
    DD <- coef(MLEobj, type = "matrix")$D
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states + DD %*% dd
  } else {
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states
  }
  ## Var in model fits
  VtT <- MARSSkfss(MLEobj)$VtT
  VV <- NULL
  for(tt in 1:TT) {
    RZVZ <- coef(MLEobj, type = "matrix")$R - ZZ %*% VtT[,,tt] %*% t(ZZ)
    SS <- Ey$yxtT[,,tt] - Ey$ytT[,tt,drop = FALSE] %*% t(MLEobj$states[,tt,drop = FALSE])
    VV <- cbind(VV, diag(RZVZ + SS %*% t(ZZ) + ZZ %*% t(SS)))
  }
  SE <- sqrt(VV)
  ## upper & lower (1-alpha)% CI
  fits$up <- qnorm(1-alpha/2)*SE + fits$ex
  fits$lo <- qnorm(alpha/2)*SE + fits$ex
  return(fits)
}
## get model fits & CI's
mod_fit <- get_DFA_fits(ssm)

## set up plotting area
par(mfrow = c(N_ts, 1), mai = c(0.5, 0.7, 0.1, 0.1), omi = c(0, 0, 0, 0))

## plot the fits
for(i in 1:N_ts) {
  up <- mod_fit$up[i,]
  mn <- mod_fit$ex[i,]
  lo <- mod_fit$lo[i,]
  plot(w_ts, mn, type = "n", ylim = c(min(lo), max(up)),
       cex.lab = 1.2,
       xlab = "", ylab = ylbl[i], xaxt = "n")
  axis(1, 12 * (0:dim(data_zoo)[2]) + 1, yr_frst + 0:dim(data_zoo)[2])
  points(w_ts,data_zoo[i,], pch = 16, col = clr[i])
  lines(w_ts, up, col = "darkgray")
  lines(w_ts, mn, col = "black", lwd = 2)
  lines(w_ts, lo, col = "darkgray")
}
