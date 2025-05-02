## load MARSS
library(MARSS)
library(panelr)
library(tidyverse)
library(zoo)

## load the data (there are 3 datasets contained here)
data(lakeWAplankton, package = "MARSS")
all_dat <- as.data.frame(lakeWAplanktonTrans)

all_dat$Date <- as.yearmon(paste(all_dat$Year, all_dat$Month), "%Y %m")

for ( i in seq(3,length( all_dat )-1,1) ) plot(all_dat[,i], x=all_dat$Date, ylab=names(all_dat[i]),type="l")

# dropping years pre-1980 and critters with poor data
data <- subset(all_dat, Year  >= 1980)
data <- data[-c(9, 18)]

# different lists of variables
phytoclimate <- c("Temp", "TP", "pH")
zooclimate <- c("Temp", "pH")
phytoplankton <- c("Cryptomonas", "Diatoms", "Greens",
                   "Unicells", "Other.algae")
zooplankton <- c("Conochilus", "Cyclops", "Daphnia",
                  "Diaptomus", "Epischura", "Leptodora")

# get only the phytoplankton
dates <- data$Date
data_phyto <- t(data[, phytoplankton])
colnames(data_phyto) <- dates

# get phytoplankton climate covariates
data_phytoclimate <- t(data[, phytoclimate])
colnames(data_phytoclimate) <- dates

# monthly factor covariate matrix
month_cov <- matrix(0, 12, 180)
month.abb <- unique(data$Month)
monrow <- match(data$Month, month.abb)[1:180]
monrow
month_cov[cbind(monrow,1:180)] <- 1
month_cov[,1:24]

# stack up the old covariate matrix
phyto.c <- rbind(month_cov, data_phytoclimate)

# MARSS models for fitting phytoplankton data to cover gaps
# build model
phyto.list <- list(
  B = "identity",
  U = "zero",
  Q = "diagonal and unequal",
  Z = "identity",
  A = "zero",
  R = "diagonal and equal",
  c = phyto.c,
  C= "unconstrained",
  x0 = "unequal",
  V0 = "zero",
  tinitx = 0
)

# modeling
ptm <- proc.time()
if(!file.exists("Lab-3/Team-2/data/phyto.rds")){
  ssm.phyto <- MARSS(data_phyto, model = phyto.list, method = "kem")
  saveRDS(ssm.phyto, file="Lab-3/Team-2/data/phyto.rds")
}
proc.time()[3] - ptm
# load in ssm.phyto
ssm.phyto <- readRDS(file="Lab-3/Team-2/data/phyto.rds")

# grabbing model estimates
states <- ssm.phyto$states
statesSE <- ssm.phyto$states.se

states <- as.data.frame(states)
statesSE <- as.data.frame(statesSE)

states_long <- states %>% 
  pivot_longer("V1":"V180", names_to="Date", values_to="fitted")
states_long <- states_long %>% 
  mutate(Date = rep(dates, 5))

states_long <- states_long %>% 
  mutate(state = c(rep(phytoplankton[1], 180),
                   rep(phytoplankton[2], 180),
                   rep(phytoplankton[3], 180),
                   rep(phytoplankton[4], 180),
                   rep(phytoplankton[5], 180)
  ))

statesSE_long <- statesSE %>% 
  pivot_longer("V1":"V180", names_to="Date", values_to="se")

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

# get only zooplankton
data_zoo <- t(data[, zooplankton])
colnames(data_zoo) <- dates
