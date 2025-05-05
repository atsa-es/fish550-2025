
## load MARSS
library(MARSS)

## load the data (there are 3 datasets contained here)
data(lakeWAplankton, package = "MARSS")

## we want lakeWAplanktonTrans, which has been transformed
## so the 0s are replaced with NAs and the data z-scored
all_dat <- lakeWAplanktonTrans

## use only the 10 years from 1980-1989
yr_frst <- 1980
yr_last <- 1989
plank_dat <- all_dat[all_dat[, "Year"] >= yr_frst & 
                       all_dat[, "Year"] <= yr_last,]

## create vector of phytoplankton group names
phytoplankton <- c("Cryptomonas", "Diatoms", "Greens",
                   "Unicells", "Other.algae")

## get only the phytoplankton
dat_1980 <- plank_dat[, phytoplankton]




