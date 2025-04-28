# Create data files
# First install devtools if you don't have it
install.packages("devtools")
library(devtools)
library(atsalibrary)

# Install from GitHub (replace with actual repo)
install_github("nwfsc-cb/rCAX")

library(rCAX)
install.packages("tidyverse")
library(tidyr)
library(dplyr)

columbia.river <- NULL
for(i in c(17, 20, 15, 11, 2)){
esuname <- rCAX:::caxesu[i]
a <- rcax_hli("NOSA", type="colnames")
tab <- rcax_hli("NOSA", flist = list(esu_dps = esuname))
# find the pops with no data and remove
tab <- tab %>% 
  subset((datastatus == "Final" | datastatus == "Reviewed") & bestvalue=="Yes")
if(i == 17 | i == 20) tab$value <- tab$tsaij else tab$value <- tab$tsaej
aa <- tab %>% 
  group_by(esapopname, run) %>% 
  summarize(n = sum(value!= "" & value!="0" & majorpopgroup != ""))
bad <- aa[which(aa$n==0),]
aa <- tab %>% 
  group_by(esapopname, run) %>% 
  summarize(n = any(duplicated(spawningyear)))
df <- tab %>% 
  subset(!(esapopname %in% bad$esapopname & run %in% bad$run)) %>%
  mutate(value = as.numeric(value))

# get the min and max years in data
years <- min(df$spawningyear[!is.na(df$value)]):max(df$spawningyear[!is.na(df$value)])
# fill out the missing years with NAs
df <- df %>%
  select(species, esu_dps, majorpopgroup, esapopname, commonpopname, spawningyear, value, run) %>% 
  group_by(species, esu_dps, majorpopgroup, esapopname, commonpopname, run) %>% 
  complete(spawningyear=years, fill=list(value=NA))

# Deal with pops with multiple data
if(any(aa$n)){
  cat(aa$esapopname[aa$n], "has duplicated years\n")
  df <- df %>% ungroup() %>%
    group_by(species, esu_dps, majorpopgroup, esapopname, run, spawningyear) %>%
    summarize(value = mean(value, na.rm = TRUE,
              commonpopname = commonpopname[1]))
}
if(i == 17 | i == 20) df$value_type <- "tsaij" else df$value_type <- "tsaej"
columbia.river <- bind_rows(columbia.river, df)
}
columbia.river <- columbia.river %>% subset(species != "") %>% ungroup()
save(columbia.river, file = "/Users/Emma/Desktop/Research_Repos/fish550-2025/Lab-2/Team-2/columbia-river.rda")
dir.create("/Users/Emma/Desktop/Research_Repos/fish550-2025/Lab-2/Team-2/", recursive = TRUE, showWarnings = FALSE)
dir.exists("/Users/Emma/Desktop/Research_Repos/fish550-2025/Lab-2/Team-2/")
unique(lower.columbia.river$esu_dps)
df <- lower.columbia.river %>% subset(species == "Steelhead" & run == "Winter")
ggplot(df, aes(x=spawningyear, y=log(value), color=majorpopgroup)) + 
  geom_point(size=0.2) + 
  theme(strip.text.x = element_text(size = 2)) +
  theme(axis.text.x = element_text(size = 5, angle = 90)) +
  facet_wrap(~esapopname)



#### Start here
setwd("C:/Users/Emma/Desktop/Research_Repos/fish550-2025/Lab-2/Data_Images")
setwd("~/Desktop/Research_Repos/fish550-2025/Lab-2/Data_Images")

#Load in the esa.salmon.rda
load("esa.salmon.rda")
unique(esa.salmon$esu_dps)
#Filter for just the "Salmon, Chinook (Snake River spring/summer-run ESU)"
esa.salmon <- esa.salmon %>% filter(esu_dps == "Salmon, Chinook (Snake River spring/summer-run ESU)")
unique(esa.salmon$majorpopgroup)
#Remove rows that have a "NaN" in the value column
esa.salmon <- esa.salmon %>% filter(value != "NaN")


#Setupa. matrix
# Create a matrix for each population, filling in missing years as NA
data_matrix <- esa.salmon %>%
  spread(key = spawningyear, value = value) %>%
  select(-esapopname) %>%
  as.matrix()

# Fill any missing values in the matrix (if any)
data_matrix[is.na(data_matrix)] <- NA

#Make all data in the matrix numeric
data_matrix <- apply(data_matrix, 2, function(x) as.numeric(as.character(x)))


#Lilac's code

esu <- unique(esa.salmon$esu_dps)
esu
esunum <- which(esu == "Salmon, Chinook (Snake River spring/summer-run ESU)")
esuname <- esu[esunum]

dat <- esa.salmon %>% 
  subset(esu_dps == esuname) %>% # get only this ESU
  mutate(log.spawner = log(value)) %>% # create a column called log.spawner
  select(esapopname, spawningyear, log.spawner) %>% # get just the columns that I need
  pivot_wider(names_from = "esapopname", values_from = "log.spawner") %>% 
  column_to_rownames(var = "spawningyear") %>% # make the years rownames
  as.matrix() %>% # turn into a matrix with year down the rows
  t() # make time across the columns
# MARSS complains if I don't do this
dat[is.na(dat)] <- NA


#How many unique run are there
unique(esa.salmon$run)
unique(esa.salmon$majorpopgroup)
unique(esa.salmon$esapopname)

#How many unique combinations of run and majorpopgroup are there?
unique_combinations1 <- unique(esa.salmon[, c("run", "majorpopgroup", "esapopname")])

#H4 
col1 <- c(rep(1, 16), rep(0, 12))         # 16 ones, 12 zeros
col2 <- c(rep(0, 16), rep(1, 8), rep(0, 4))  # 16 zeros, 8 ones, 4 zeros
col3 <- c(rep(0, 24), rep(1, 4))          # 24 zeros, 4 ones
my_matrix <- t(rbind(col1, col2, col3))

#Setting up the Z matrix for part 2 of the lab: Are the populations correlated?
#H1: There is no correlation between the populations
mod.list.0 <- list(B = "identity", 
                   U = matric ("u"),
                   Q = matrix("q"),
                   Z = matrix(1,28,1),
                   A = "scaling",
                   R = "diagonal and equal",
                   x0 = matrix("mu"),
                   tinitx = 0)
fit.0 <- MARSS(data, model = mod.list.0)

#H2: There is a correlation within the populations (28 different populations)
mod.list.1 <- list(B = "identity", 
                   U = "equal",
                   Q = "diagonal and equal",
                   Z = "identity",
                   A = "scaling",
                   R = "diagonal and equal",
                   x0 = matrix("mu"),
                   tinitx = 0)
fit.1 <- MARSS::MARSS(data, model = mod.list.1)

#H3: Same as H2 but the errors are temporally correlated
mod.list.2 <- mod.lisst.1
mod.list.2$Q <- "equalvarcov"

fit.2 <- MARSS:: MARSS(data, model = mod.list.2)

#H4: Spring vs. Summer vs. Spring/Summer with temporal correlation 3X28 for Z matrix

col1 <- c(rep(1, 16), rep(0, 12))         # 16 ones, 12 zeros
col2 <- c(rep(0, 16), rep(1, 8), rep(0, 4))  # 16 zeros, 8 ones, 4 zeros
col3 <- c(rep(0, 24), rep(1, 4))          # 24 zeros, 4 ones
ZMatrix4 <- t(rbind(col1, col2, col3))

mod.list.4 <- list(B = "identity",  
                   U = "equal",
                   Q = "equalvarcov",
                   Z = ZMatrix4,
                   A = "scaling",
                   R = "diagonal and equal", #Observation variance
                   x0 = "unequal",
                   tinitx = 0)

fit.4 <- MARSS::MARSS(data, model = mod.list.4)

#H5: Seasonal differences and major pop groups 9 x 28 for  Z with temporal correlation
col1 <- c(rep(1, 5), rep(0, 23))         # 5 ones, 23 zeros
col2 <- c(rep(0, 5), rep(1, 2), rep(0, 21))  # 5 zeros, 2 ones, 21 zeros
col3 <- c(rep(0, 7), rep(1, 5), rep(0, 16)) # 24 zeros, 4 ones
col4 <- c(rep(0, 12), rep(1, 4), rep(0, 12))
col5 <- c(rep(0, 16), rep(1, 1), rep(0, 11))
col6 <- c(rep(0, 17), rep(1, 4), rep(0, 7))
col7 <- c(rep(0, 21), rep(1, 3), rep(0, 4))
col8 <- c(rep(0, 24), rep(1, 3), rep(0, 1))
col9 <- c(rep(0, 27), rep(1, 1))

BMatrix4 <- t(rbind(col1, col2, col3, col4, col5, col6, col7, col8, col9))
ZMatrix4 <- BMatrix4

mod.list.6 <- list(B = "identity", 
                   U = "equal",
                   Q = "equalvarcov",
                   Z = Zmatrix6,
                   A = "scaling",
                   R = "diagonal and equal",
                   x0 = "unequal",
                   tinitx = 0)

fit.6 <- MARSS::MARSS(data, model = mod.list.6)

BMatrix3 <- t(rbind(col1, col2, col3))
ZMatrix3 <- BMatrix3


#Z matrix for H3 and H4
#Need to cut esa.salmon down to just the sna
#load("esa.salmon.rda")
#unique(esa.salmon$esu_dps)
#Filter for just the "Salmon, Chinook (Snake River spring/summer-run ESU)"
library(dplyr)
library(tidyverse)
esa.snake <- esa.salmon %>% filter(esu_dps == "Salmon, Chinook (Snake River spring/summer-run ESU)")
unique(esa.salmon$majorpopgroup)

#Pull out only the second through sixth columns
esa.snake <- esa.salmon %>% select(2:6)

#Make a new df of only the unique combinations of the majorpopgroup, run, and esapopname combinations
esa.snake.names <- esa.snake %>% select(majorpopgroup, run, esapopname)
#remove duplicate rows in the esa.snake.names df
esa.snake.names <- esa.snake.names %>% distinct()
#Organize the rows by alphabetical order of the esapopname column
esa.snake.names <- esa.snake.names %>% arrange(esapopname)

#Remove rows that have a "NaN" in the value column
esa.salmon <- esa.salmon %>% filter(value != "NaN")

#9x28 Z matrix
Zmatrix4 <- matrix(c(
  # Row 1
  1,0,0,
  # Row 2
  1,0,0,
  # Row 3
  0,1,0,
  #Row 4
  0,1,0,
  #Row 5
  1,0,0,
  # Row 6
  1,0,0,
  #Row 7
  0,1,0,
  #Row 8
  0,0,1,
  #Row 9
  1,0,0,
  #Row 10
  0,1,0,
  #Row 11
  1,0,0,
  #Row 12
  0,1,0,
  #Row 13
  1,0,0,
  #Row 14
  0,1,0,
  #Row 15
  1,0,0,
  #Row 16
  1,0,0,
  #Row 17
  1,0,0,
  #Row 18
  0,0,1,
  #Row 19
  0,1,0,
  #Row 20
  1,0,0,
  #Row 21
  0,0,1,
  #Row 22
  0,0,1,
  #Row 23
  1,0,0,
  #Row 24
  1,0,0,
  #Row 25
  0,1,0,
  #Row 26
  1,0,0,
  #Row 27
  1,0,0,
  #Row 28
  1,0,0
), nrow = 28, byrow = TRUE)

