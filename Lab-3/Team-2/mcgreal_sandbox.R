## load MARSS
library(MARSS)
library(reshape2)
library(tidyverse)

## load the data (there are 3 datasets contained here)
data(lakeWAplankton, package = "MARSS")
all_dat <- as.data.frame(lakeWAplanktonTrans)

all_dat$Date <- as.yearmon(paste(all_dat$Year, all_dat$Month), "%Y %m")

for ( i in seq(3,length( all_dat )-1,1) ) plot(all_dat[,i], x=all_dat$Date, ylab=names(all_dat[i]),type="l")

# dropping years pre-1980 and critters with poor data
data <- subset(all_dat, Year  >= 1980)
data <- data[-c(1, 2, 9, 18)]
