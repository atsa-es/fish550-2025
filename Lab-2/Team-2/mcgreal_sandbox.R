library(atsalibrary)
library(MARSS)
library(panelr)
library(readr)
library(tidyverse)
library(vcvComp)
library(zoo)

# DATA ----
# pull in data
load("Lab-2/Data_Images/esa-salmon.rda")

# check out the data
esu <- unique(esa.salmon$esu_dps)
head(esa.salmon)
esu

# plotting esu
plotesu <- function(esuname){
  df <- esa.salmon %>% subset(esu_dps %in% esuname)
  ggplot(df, aes(x=spawningyear, y=log(value), color=majorpopgroup)) + 
    geom_point(size=0.2, na.rm = TRUE) + 
    theme(strip.text.x = element_text(size = 3)) +
    theme(axis.text.x = element_text(size = 5, angle = 90)) +
    facet_wrap(~esapopname) +
    ggtitle(paste0(esuname, collapse="\n"))
}
plotesu(esu[2])

# subset esu data
esu2 <- subset(esa.salmon, esu_dps == "Salmon, Chinook (Snake River spring/summer-run ESU)")
esu2$popname <- substr(esu2$esapopname, 53, nchar(esu2$esapopname)) 

# log transform and set data wide
esu2$logvalue <- log(esu2$value + 1)
esu2 <- esu2 %>% mutate_all(~ifelse(is.nan(.), NA, .))

esu2_widen <- esu2[-c(1:6, 8)]
w_esu2 <- panel_data(esu2_widen, id = popname, wave = spawningyear)
w_esu2 <- widen_panel(w_esu2, separator = "_")

# grab years, population names, and n
years <- names(w_esu2)
years <- years[-1]
years <- substring(years, first=10, last=13)

pops <- w_esu2$popname

n <- nrow(w_esu2)

# convert counts to matrix
dat <- data.matrix(w_esu2[2:ncol(w_esu2)])
dat

# check how much data is there per majorpopgroup?
esu2$data_available <- ifelse(is.na(esu2$value), 0, 1)
majpop_avaliable <- esu2[-c(1, 2, 4:10)]
aggregate(data_available ~ majorpopgroup, majpop_avaliable, sum)

# QUESTION 1 ----
## unique populations ----
# specify matrices for MARSS models
b.model <- "identity"
u.model <- matrix(paste0("u", seq(n)))
q.model <- "diagonal and equal"
z.model <- "identity"
a.model <- "zero"
r.model <- "diagonal and equal" 
x0.model <- "unequal"
v0.model <- "zero"

model.list <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x0.model, V0 = v0.model, tinitx = 0)

# modeling
ptm <- proc.time()
if(!file.exists("Lab-2/Team-2/ss1.all.rds")){
  ss1.all <- MARSS(dat, model = model.list, method = "kem")
  saveRDS(ss1.all, file="Lab-2/Team-2/ss1.all.rds")
}
proc.time()[3] - ptm

# load in ss1.all
ss1.all <- readRDS(file="Lab-2/Team-2/ss1.all.rds")

# grabbing data for figures
states <- ss1.all$states
statesSE <- ss1.all$states.se

states <- as.data.frame(states)
statesSE <- as.data.frame(statesSE)

states_long <- states %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="fitted")
states_long <- states_long %>% 
  mutate(Year = rep(years, 28))

states_long <- states_long %>% 
  mutate(state = c(rep(pops[1], 76),
                   rep(pops[2], 76),
                   rep(pops[3], 76),
                   rep(pops[4], 76),
                   rep(pops[5], 76),
                   rep(pops[6], 76),
                   rep(pops[7], 76),
                   rep(pops[8], 76),
                   rep(pops[9], 76),
                   rep(pops[10], 76),
                   rep(pops[11], 76),
                   rep(pops[12], 76),
                   rep(pops[13], 76),
                   rep(pops[14], 76),
                   rep(pops[15], 76),
                   rep(pops[16], 76),
                   rep(pops[17], 76),
                   rep(pops[18], 76),
                   rep(pops[19], 76),
                   rep(pops[20], 76),
                   rep(pops[21], 76),
                   rep(pops[22], 76),
                   rep(pops[23], 76),
                   rep(pops[24], 76),
                   rep(pops[25], 76),
                   rep(pops[26], 76),
                   rep(pops[27], 76),
                   rep(pops[28], 76)
                   ))

statesSE_long <- statesSE %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="se")

states_long <- states_long %>% 
  mutate(se = statesSE_long$se)

states_long$lb <- states_long$fitted - states_long$se
states_long$ub <- states_long$fitted + states_long$se

# save long state estimates
save(states_long, file="Lab-2/Team-2/ss1.allstates_long.Rda")

plot <- ggplot(data = states_long, aes(x = Year, y = fitted, group = state)) +
  geom_line() +
  labs(x = "", 
       y="State Estimate",
       title='Spring Summer Chinook Abundance, Upper Columbia/Snake River',
       subtitle="28 individual populations, equal process variance") +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  theme_classic() +
  theme(legend.position="bottom") + 
  scale_x_discrete(breaks = c(1950, 1964, 1978, 1992, 2006, 2020))
plot

## working on z matrices ----
esu2_key <- esu2[-c(1,2, 4, 5, 8, 10)]
esu2_key <- aggregate(spawningyear ~ majorpopgroup + popname + run, data = esu2_key, FUN = mean)
esu2_key <- esu2_key[-c(4)]

# using esu key to build z matrices
esu2_key$poprun <- paste0(as.character(esu2_key$majorpopgroup),"_", as.character(esu2_key$run))

esu2_key$r_spring <- ifelse(esu2_key$run == "Spring", 1, 0)
esu2_key$r_sprsum <- ifelse(esu2_key$run == "Spring/summer", 1, 0)
esu2_key$r_summer <- ifelse(esu2_key$run == "Summer", 1, 0)
  #run

esu2_key$p_ls <- ifelse(esu2_key$majorpopgroup == "Lower Snake", 1, 0)
esu2_key$p_mf <- ifelse(esu2_key$majorpopgroup == "Middle Fork Salmon", 1, 0)
esu2_key$p_gr <- ifelse(esu2_key$majorpopgroup == "Grande Ronde/Imnaha", 1, 0)
esu2_key$p_us <- ifelse(esu2_key$majorpopgroup == "Upper Salmon", 1, 0)
esu2_key$p_sf <- ifelse(esu2_key$majorpopgroup == "South Fork Salmon", 1, 0)
  #major population

esu2_key$b_sprls <- ifelse(esu2_key$poprun == "Lower Snake_Spring", 1, 0)
esu2_key$b_sprmf <- ifelse(esu2_key$poprun == "Middle Fork Salmon_Spring", 1, 0)
esu2_key$b_ssrmf <- ifelse(esu2_key$poprun == "Middle Fork Salmon_Spring/summer", 1, 0)
esu2_key$b_sprgr <- ifelse(esu2_key$poprun == "Grande Ronde/Imnaha_Spring", 1, 0)
esu2_key$b_ssrus <- ifelse(esu2_key$poprun == "Upper Salmon_Spring/summer", 1, 0)
esu2_key$b_sumsf <- ifelse(esu2_key$poprun == "South Fork Salmon_Summer", 1, 0)
esu2_key$b_ssrgr <- ifelse(esu2_key$poprun == "Grande Ronde/Imnaha_Spring/summer", 1, 0)
esu2_key$b_sprus <- ifelse(esu2_key$poprun == "Upper Salmon_Spring", 1, 0)
esu2_key$b_sumus <- ifelse(esu2_key$poprun == "Upper Salmon_Summer", 1, 0)
  #major pop/run

zmat_r <- esu2_key[-c(1, 3, 4, 8:21)]
zmat_p <- esu2_key[-c(1, 3:7, 13:21)]
zmat_b <- esu2_key[-c(1, 3:12)]

# convert counts to matrix
zmat_r <- zmat_r[order(zmat_r$popname),,drop=FALSE]
z_run <- data.matrix(zmat_r[2:ncol(zmat_r)])

zmat_p <- zmat_p[order(zmat_p$popname),,drop=FALSE]
z_mpop <- data.matrix(zmat_p[2:ncol(zmat_p)])

zmat_b <- zmat_b[order(zmat_b$popname),,drop=FALSE]
z_mpoprun <- data.matrix(zmat_b[2:ncol(zmat_b)])

## run  ----
# specify matrices for MARSS models
b.model <- "identity"
u.model <- matrix(paste0("u", seq(ncol(z_run))))
q.model <- "diagonal and equal"
z.model <- z_run
a.model <- "zero"
r.model <- "diagonal and equal" 
x0.model <- "unequal"
v0.model <- "zero"

model.list <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x0.model, V0 = v0.model, tinitx = 0)

# modeling
ptm <- proc.time()
if(!file.exists("Lab-2/Team-2/ss1.run.rds")){
  ss1.run <- MARSS(dat, model = model.list, method = "kem")
  saveRDS(ss1.run, file="Lab-2/Team-2/ss1.run.rds")
}
proc.time()[3] - ptm

# load in ss1
ss1.run <- readRDS(file="Lab-2/Team-2/ss1.run.rds")

# grabbing data for figures
states <- ss1.run$states
statesSE <- ss1.run$states.se

states <- as.data.frame(states)
statesSE <- as.data.frame(statesSE)

states_long <- states %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="fitted")
states_long <- states_long %>% 
  mutate(Year = rep(years, ncol(z_run)))

states_long <- states_long %>% 
  mutate(state = c(rep("Spring", 76),
                   rep("Spring/summer", 76),
                   rep("Summer", 76)
  ))

statesSE_long <- statesSE %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="se")

states_long <- states_long %>% 
  mutate(se = statesSE_long$se)

states_long$lb <- states_long$fitted - states_long$se
states_long$ub <- states_long$fitted + states_long$se

# save long state estimates
save(states_long, file="Lab-2/Team-2/ss1.runstates_long.Rda")

plot <- ggplot(data = states_long, aes(x = Year, y = fitted, group = state)) +
  geom_line() +
  labs(x = "", 
       y="State Estimate",
       title='Spring Summer Chinook Abundance, Upper Columbia/Snake River',
       subtitle="3 runs, equal process variance") +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  theme_classic() +
  theme(legend.position="bottom") +
  scale_x_discrete(breaks = c(1950, 1964, 1978, 1992, 2006, 2020))
plot

cov <- matrix(c(states_long$fitted), 76, ncol = ncol(z_run))
ss1.run.cor <- cor(cov)
unique(states_long$state)
ss1.run.cor

## pop ----
# specify matrices for MARSS models
b.model <- "identity"
u.model <- matrix(paste0("u", seq(ncol(z_mpop))))
q.model <- "diagonal and equal"
z.model <- z_mpop
a.model <- "zero"
r.model <- "diagonal and equal" 
x0.model <- "unequal"
v0.model <- "zero"

model.list <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x0.model, V0 = v0.model, tinitx = 0)

# modeling
ptm <- proc.time()
if(!file.exists("Lab-2/Team-2/ss1.pop.rds")){
  ss1.pop <- MARSS(dat, model = model.list, method = "kem")
  saveRDS(ss1.pop, file="Lab-2/Team-2/ss1.pop.rds")
}
proc.time()[3] - ptm

# load in ss1
ss1.pop <- readRDS(file="Lab-2/Team-2/ss1.pop.rds")

# grabbing data for figures
states <- ss1.pop$states
statesSE <- ss1.pop$states.se

states <- as.data.frame(states)
statesSE <- as.data.frame(statesSE)

states_long <- states %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="fitted")
states_long <- states_long %>% 
  mutate(Year = rep(years, ncol(z_mpop)))

states_long <- states_long %>% 
  mutate(state = c(rep("lower snake", 76),
                   rep("middle fork snake", 76),
                   rep("gradne ronde", 76),
                   rep("upper salmon", 76),
                   rep("south fork salmon", 76)
  ))

statesSE_long <- statesSE %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="se")

states_long <- states_long %>% 
  mutate(se = statesSE_long$se)

states_long$lb <- states_long$fitted - states_long$se
states_long$ub <- states_long$fitted + states_long$se

# save long state estimates
save(states_long, file="Lab-2/Team-2/ss1.popstates_long.Rda")

plot <- ggplot(data = states_long, aes(x = Year, y = fitted, group = state)) +
  geom_line() +
  labs(x = "", 
       y="State Estimate",
       title='Spring Summer Chinook Abundance, Upper Columbia/Snake River',
       subtitle="5 major populations, equal process variance") +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  theme_classic() +
  theme(legend.position="bottom") +
  scale_x_discrete(breaks = c(1950, 1964, 1978, 1992, 2006, 2020))
plot

cov <- matrix(c(states_long$fitted), 76, ncol = ncol(z_mpop))
ss1.pop.cor <- cor(cov)
unique(states_long$state)
ss1.pop.cor

## run/pop  ----
# specify matrices for MARSS models
b.model <- "identity"
u.model <- matrix(paste0("u", seq(ncol(z_mpoprun))))
q.model <- "diagonal and equal"
z.model <- z_mpoprun
a.model <- "zero"
r.model <- "diagonal and equal" 
x0.model <- "unequal"
v0.model <- "zero"

model.list <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x0.model, V0 = v0.model, tinitx = 0)

# modeling
ptm <- proc.time()
if(!file.exists("Lab-2/Team-2/ss1.poprun.rds")){
  ss1.poprun <- MARSS(dat, model = model.list, method = "kem")
  saveRDS(ss1.poprun, file="Lab-2/Team-2/ss1.poprun.rds")
}
proc.time()[3] - ptm

# load in ss1
ss1.poprun <- readRDS(file="Lab-2/Team-2/ss1.poprun.rds")

# grabbing data for figures
states <- ss1.poprun$states
statesSE <- ss1.poprun$states.se

states <- as.data.frame(states)
statesSE <- as.data.frame(statesSE)

states_long <- states %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="fitted")
states_long <- states_long %>% 
  mutate(Year = rep(years, ncol(z_mpoprun)))

states_long <- states_long %>% 
  mutate(state = c(rep("Lower Snake, Spring", 76),
                   rep("Middle Fork Salmon, Spring", 76),
                   rep("Middle Fork Salmon, Spring/summer", 76),
                   rep("Grande Ronde/Imnaha, Spring", 76),
                   rep("Upper Salmon, Spring/summer", 76),
                   rep("South Fork Salmon, Summer", 76),
                   rep("Grande Ronde/Imnaha, Spring/summer", 76),
                   rep("Upper Salmon, Spring", 76),
                   rep("Upper Salmon, Summer", 76)
  ))

statesSE_long <- statesSE %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="se")

states_long <- states_long %>% 
  mutate(se = statesSE_long$se)

states_long$lb <- states_long$fitted - states_long$se
states_long$ub <- states_long$fitted + states_long$se

# save long state estimates
save(states_long, file="Lab-2/Team-2/ss1.poprunstates_long.Rda")

plot <- ggplot(data = states_long, aes(x = Year, y = fitted, group = state)) +
  geom_line() +
  labs(x = "", 
       y="State Estimate",
       title='Spring Summer Chinook Abundance, Upper Columbia/Snake River',
       subtitle="9 groups - 3 runs and 5 major populations, equal process variance") +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  theme_classic() +
  theme(legend.position="bottom") +
  scale_x_discrete(breaks = c(1950, 1964, 1978, 1992, 2006, 2020))
plot

cov <- matrix(c(states_long$fitted), 76, ncol = ncol(z_mpoprun))
ss1.poprun.cor <- cor(cov)
unique(states_long$state)
ss1.poprun.cor

# QUESTION 2  ----

## run ----
# model build
b.model <- "identity"
u.model <- matrix(paste0("u", seq(ncol(z_run))))
q.model <- "diagonal and unequal"
z.model <- z_run
a.model <- "zero"
r.model <- "diagonal and equal" 
x0.model <- "unequal"
v0.model <- "zero"

model.list <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x0.model, V0 = v0.model, tinitx = 0)

# modeling
ptm <- proc.time()
if(!file.exists("Lab-2/Team-2/ss2.run.rds")){
  ss2.run <- MARSS(dat, model = model.list, method = "kem")
  saveRDS(ss2.run, file="Lab-2/Team-2/ss2.run.rds")
}
proc.time()[3] - ptm

# load in ss2.run
ss2.run <- readRDS(file="Lab-2/Team-2/ss2.run.rds")

# grabbing data for figures
states <- ss2.run$states
statesSE <- ss2.run$states.se

states <- as.data.frame(states)
statesSE <- as.data.frame(statesSE)

states_long <- states %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="fitted")
states_long <- states_long %>% 
  mutate(Year = rep(years, ncol(z_run)))

states_long <- states_long %>% 
  mutate(state = c(rep("Spring", 76),
                   rep("Spring/summer", 76),
                   rep("Summer", 76)
  ))

statesSE_long <- statesSE %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="se")

states_long <- states_long %>% 
  mutate(se = statesSE_long$se)

states_long$lb <- states_long$fitted - states_long$se
states_long$ub <- states_long$fitted + states_long$se

# save long state estimates
save(states_long, file="Lab-2/Team-2/ss2.runstates_long.Rda")

plot <- ggplot(data = states_long, aes(x = Year, y = fitted, group = state)) +
  geom_line() +
  labs(x = "", 
       y="State Estimate",
       title='Spring Summer Chinook Abundance, Upper Columbia/Snake River',
       subtitle="3 runs, unequal process variance") +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  theme_classic() +
  theme(legend.position="bottom") +
  scale_x_discrete(breaks = c(1950, 1964, 1978, 1992, 2006, 2020))
plot

cov <- matrix(c(states_long$fitted), 76, ncol = ncol(z_run))
ss2.run.cor <- cor(cov)
unique(states_long$state)
ss2.run.cor

## pop ----
b.model <- "identity"
u.model <- matrix(paste0("u", seq(ncol(z_mpop))))
q.model <- "diagonal and unequal"
z.model <- z_mpop
a.model <- "zero"
r.model <- "diagonal and equal" 
x0.model <- "unequal"
v0.model <- "zero"

model.list <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x0.model, V0 = v0.model, tinitx = 0)

# modeling
ptm <- proc.time()
if(!file.exists("Lab-2/Team-2/ss2.pop.rds")){
  ss2.pop <- MARSS(dat, model = model.list, method = "kem")
  saveRDS(ss2.pop, file="Lab-2/Team-2/ss2.pop.rds")
}
proc.time()[3] - ptm

# load in ss2.pop
ss2.pop <- readRDS(file="Lab-2/Team-2/ss2.pop.rds")

# grabbing data for figures
states <- ss2.pop$states
statesSE <- ss2.pop$states.se

states <- as.data.frame(states)
statesSE <- as.data.frame(statesSE)

states_long <- states %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="fitted")
states_long <- states_long %>% 
  mutate(Year = rep(years, ncol(z_mpop)))

states_long <- states_long %>% 
  mutate(state = c(rep("lower snake", 76),
                   rep("middle fork snake", 76),
                   rep("gradne ronde", 76),
                   rep("upper salmon", 76),
                   rep("south fork salmon", 76)
  ))

statesSE_long <- statesSE %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="se")

states_long <- states_long %>% 
  mutate(se = statesSE_long$se)

states_long$lb <- states_long$fitted - states_long$se
states_long$ub <- states_long$fitted + states_long$se

# save long state estimates
save(states_long, file="Lab-2/Team-2/ss2.popstates_long.Rda")

plot <- ggplot(data = states_long, aes(x = Year, y = fitted, group = state)) +
  geom_line() +
  labs(x = "", 
       y="State Estimate",
       title='Spring Summer Chinook Abundance, Upper Columbia/Snake River',
       subtitle="5 populations, unequal process variance") +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  theme_classic() +
  theme(legend.position="bottom") +
  scale_x_discrete(breaks = c(1950, 1964, 1978, 1992, 2006, 2020))
plot

cov <- matrix(c(states_long$fitted), 76, ncol = ncol(z_mpop))
ss2.pop.cor <- cor(cov)
unique(states_long$state)
ss2.pop.cor

## run/pop ----
b.model <- "identity"
u.model <- matrix(paste0("u", seq(ncol(z_mpoprun))))
q.model <- "diagonal and unequal"
z.model <- z_mpoprun
a.model <- "zero"
r.model <- "diagonal and equal" 
x0.model <- "unequal"
v0.model <- "zero"

model.list <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x0.model, V0 = v0.model, tinitx = 0)

# modeling
ptm <- proc.time()
if(!file.exists("Lab-2/Team-2/ss2.poprun.rds")){
  ss2.poprun <- MARSS(dat, model = model.list, method = "kem")
  saveRDS(ss2.poprun, file="Lab-2/Team-2/ss2.poprun.rds")
}
proc.time()[3] - ptm

# load in ss2.poprun
ss2.poprun <- readRDS(file="Lab-2/Team-2/ss2.poprun.rds")

# grabbing data for figures
states <- ss2.poprun$states
statesSE <- ss2.poprun$states.se

states <- as.data.frame(states)
statesSE <- as.data.frame(statesSE)

states_long <- states %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="fitted")
states_long <- states_long %>% 
  mutate(Year = rep(years, ncol(z_mpoprun)))

states_long <- states_long %>% 
  mutate(state = c(rep("Lower Snake, Spring", 76),
                   rep("Middle Fork Salmon, Spring", 76),
                   rep("Middle Fork Salmon, Spring/summer", 76),
                   rep("Grande Ronde/Imnaha, Spring", 76),
                   rep("Upper Salmon, Spring/summer", 76),
                   rep("South Fork Salmon, Summer", 76),
                   rep("Grande Ronde/Imnaha, Spring/summer", 76),
                   rep("Upper Salmon, Spring", 76),
                   rep("Upper Salmon, Summer", 76)
  ))

statesSE_long <- statesSE %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="se")

states_long <- states_long %>% 
  mutate(se = statesSE_long$se)

states_long$lb <- states_long$fitted - states_long$se
states_long$ub <- states_long$fitted + states_long$se

# save long state estimates
save(states_long, file="Lab-2/Team-2/ss2.poprunstates_long.Rda")

plot <- ggplot(data = states_long, aes(x = Year, y = fitted, group = state)) +
  geom_line() +
  labs(x = "", 
       y="State Estimate",
       title='Spring Summer Chinook Abundance, Upper Columbia/Snake River',
       subtitle="9 groups - 3 runs and 5 major populations, unequal process variance") +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  theme_classic() +
  theme(legend.position="bottom") +
  scale_x_discrete(breaks = c(1950, 1964, 1978, 1992, 2006, 2020))
plot

cov <- matrix(c(states_long$fitted), 76, ncol = ncol(z_mpoprun))
ss2.poprun.cor <- cor(cov)
unique(states_long$state)
ss2.poprun.cor

# QUESTION 3 ----
## get pdo data ----
# read in data from table
if(!file.exists("Lab-2/Team-2/pdo_raw.Rda")){
  pdo <- read.table("https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat",
                    sep = "",
                    header = TRUE,
                    skip = 1)
  save(pdo, file="Lab-2/Team-2/pdo_raw.Rda")
} 

if(file.exists("Lab-2/Team-2/pdo_raw.Rda")) {
  load("Lab-2/Team-2/pdo_raw.Rda")
}

#annual average
# annual average - is this smart?
annual <- rowMeans(pdo[ ,2:13])
annual
pdo$annual <- annual

# drop month vars and years before 1960
pdo <- pdo[-c(2:13)]
pdo <- pdo %>% filter(Year >= 1946)
pdo <- pdo %>% filter(Year < 2025)

# create lagged average (taking average of three prior years)
pdo$pdo_03ya <- zoo::rollmean(pdo$annual ,k=3, align="right", fill=NA)
pdo$spawningyear <- pdo$Year + 1
head(pdo)
  # convinced?
pdo <- pdo[-c(1,2)]
pdo <- pdo %>% filter(spawningyear >= 1949)
pdo <- pdo %>% filter(spawningyear < 2025)

# set wide
pdo_yrs <- pdo$spawningyear
pdo_idx <- pdo$pdo_03ya

Wpdo <- as.data.frame(matrix(pdo_idx, nrow=1, byrow=TRUE))
names(Wpdo) <- pdo_yrs
Wpdo <- data.matrix(Wpdo)

## run ----
# model build
b.model <- "identity"
u.model <- matrix(paste0("u", seq(ncol(z_run))))
c.model <- matrix("pdo", ncol(z_run), 1)
q.model <- "diagonal and unequal"
z.model <- z_run
a.model <- "zero"
r.model <- "diagonal and equal" 
x0.model <- "unequal"
v0.model <- "zero"

model.list <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  C = c.model, c = Wpdo, 
  x0 = x0.model, V0 = v0.model, tinitx = 0)

# modeling
ptm <- proc.time()
if(!file.exists("Lab-2/Team-2/ss3.run.rds")){
  ss3.run <- MARSS(dat, model = model.list, method = "kem")
  saveRDS(ss3.run, file="Lab-2/Team-2/ss3.run.rds")
}
proc.time()[3] - ptm

# load in ss3.run
ss3.run <- readRDS(file="Lab-2/Team-2/ss3.run.rds")

# grabbing data for figures
states <- ss3.run$states
statesSE <- ss3.run$states.se

states <- as.data.frame(states)
statesSE <- as.data.frame(statesSE)

states_long <- states %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="fitted")
states_long <- states_long %>% 
  mutate(Year = rep(years, ncol(z_run)))

states_long <- states_long %>% 
  mutate(state = c(rep("Spring", 76),
                   rep("Spring/summer", 76),
                   rep("Summer", 76)
  ))

statesSE_long <- statesSE %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="se")

states_long <- states_long %>% 
  mutate(se = statesSE_long$se)

states_long$lb <- states_long$fitted - states_long$se
states_long$ub <- states_long$fitted + states_long$se

# save long state estimates
save(states_long, file="Lab-2/Team-2/ss3.runstates_long.Rda")

plot <- ggplot(data = states_long, aes(x = Year, y = fitted, group = state)) +
  geom_line() +
  labs(x = "", 
       y="State Estimate",
       title='Spring Summer Chinook Abundance, Upper Columbia/Snake River',
       subtitle="3 runs, unequal process variance, PDO covariate") +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  theme_classic() +
  theme(legend.position="bottom") +
  scale_x_discrete(breaks = c(1950, 1964, 1978, 1992, 2006, 2020))
plot

cov <- matrix(c(states_long$fitted), 76, ncol = ncol(z_run))
ss3.run.cor <- cor(cov)
unique(states_long$state)
ss3.run.cor

## pop ----
# model build
b.model <- "identity"
u.model <- matrix(paste0("u", seq(ncol(z_mpop))))
c.model <- matrix("pdo", ncol(z_mpop), 1)
q.model <- "diagonal and unequal"
z.model <- z_mpop
a.model <- "zero"
r.model <- "diagonal and equal" 
x0.model <- "unequal"
v0.model <- "zero"

model.list <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  C = c.model, c = Wpdo, 
  x0 = x0.model, V0 = v0.model, tinitx = 0)

# modeling
ptm <- proc.time()
if(!file.exists("Lab-2/Team-2/ss3.pop.rds")){
  ss3.pop <- MARSS(dat, model = model.list, method = "kem")
  saveRDS(ss3.pop, file="Lab-2/Team-2/ss3.pop.rds")
}
proc.time()[3] - ptm

# load in ss3.pop
ss3.pop <- readRDS(file="Lab-2/Team-2/ss3.pop.rds")

# grabbing data for figures
states <- ss3.pop$states
statesSE <- ss3.pop$states.se

states <- as.data.frame(states)
statesSE <- as.data.frame(statesSE)

states_long <- states %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="fitted")
states_long <- states_long %>% 
  mutate(Year = rep(years, ncol(z_mpop)))

states_long <- states_long %>% 
  mutate(state = c(rep("lower snake", 76),
                   rep("middle fork snake", 76),
                   rep("gradne ronde", 76),
                   rep("upper salmon", 76),
                   rep("south fork salmon", 76)
  ))

statesSE_long <- statesSE %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="se")

states_long <- states_long %>% 
  mutate(se = statesSE_long$se)

states_long$lb <- states_long$fitted - states_long$se
states_long$ub <- states_long$fitted + states_long$se

# save long state estimates
save(states_long, file="Lab-2/Team-2/ss3.popstates_long.Rda")

plot <- ggplot(data = states_long, aes(x = Year, y = fitted, group = state)) +
  geom_line() +
  labs(x = "", 
       y="State Estimate",
       title='Spring Summer Chinook Abundance, Upper Columbia/Snake River',
       subtitle="5 populations, unequal process variance, PDO covariate") +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  theme_classic() +
  theme(legend.position="bottom") +
  scale_x_discrete(breaks = c(1950, 1964, 1978, 1992, 2006, 2020))
plot

cov <- matrix(c(states_long$fitted), 76, ncol = ncol(z_mpop))
ss3.pop.cor <- cor(cov)
unique(states_long$state)
ss3.pop.cor

## run/pop ----
# model build
b.model <- "identity"
u.model <- matrix(paste0("u", seq(ncol(z_mpoprun))))
c.model <- matrix("pdo", ncol(z_mpoprun), 1)
q.model <- "diagonal and unequal"
z.model <- z_mpoprun
a.model <- "zero"
r.model <- "diagonal and equal" 
x0.model <- "unequal"
v0.model <- "zero"

model.list <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  C = c.model, c = Wpdo, 
  x0 = x0.model, V0 = v0.model, tinitx = 0)

# modeling
ptm <- proc.time()
if(!file.exists("Lab-2/Team-2/ss3.poprun.rds")){
  ss3.poprun <- MARSS(dat, model = model.list, method = "kem")
  saveRDS(ss3.poprun, file="Lab-2/Team-2/ss3.poprun.rds")
}
proc.time()[3] - ptm

# load in ss3.poprun
ss3.poprun <- readRDS(file="Lab-2/Team-2/ss3.poprun.rds")

# grabbing data for figures
states <- ss3.poprun$states
statesSE <- ss3.poprun$states.se

states <- as.data.frame(states)
statesSE <- as.data.frame(statesSE)

states_long <- states %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="fitted")
states_long <- states_long %>% 
  mutate(Year = rep(years, ncol(z_mpoprun)))

states_long <- states_long %>% 
  mutate(state = c(rep("Lower Snake, Spring", 76),
                   rep("Middle Fork Salmon, Spring", 76),
                   rep("Middle Fork Salmon, Spring/summer", 76),
                   rep("Grande Ronde/Imnaha, Spring", 76),
                   rep("Upper Salmon, Spring/summer", 76),
                   rep("South Fork Salmon, Summer", 76),
                   rep("Grande Ronde/Imnaha, Spring/summer", 76),
                   rep("Upper Salmon, Spring", 76),
                   rep("Upper Salmon, Summer", 76)
  ))

statesSE_long <- statesSE %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="se")

states_long <- states_long %>% 
  mutate(se = statesSE_long$se)

states_long$lb <- states_long$fitted - states_long$se
states_long$ub <- states_long$fitted + states_long$se

# save long state estimates
save(states_long, file="Lab-2/Team-2/ss3.poprunstates_long.Rda")

plot <- ggplot(data = states_long, aes(x = Year, y = fitted, group = state)) +
  geom_line() +
  labs(x = "", 
       y="State Estimate",
       title='Spring Summer Chinook Abundance, Upper Columbia/Snake River',
       subtitle="9 groups - 3 runs and 5 major populations, unequal process variance, PDO covariate") +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  theme_classic() +
  theme(legend.position="bottom") +
  scale_x_discrete(breaks = c(1950, 1964, 1978, 1992, 2006, 2020))
plot

cov <- matrix(c(states_long$fitted), 76, ncol = ncol(z_mpoprun))
ss3.poprun.cor <- cor(cov)
unique(states_long$state)
ss3.poprun.cor