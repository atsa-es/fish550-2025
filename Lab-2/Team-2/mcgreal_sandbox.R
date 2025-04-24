library(atsalibrary)
library(MARSS)
library(panelr)
library(tidyverse)

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

## QUESTION 1
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
if(!file.exists("Lab-2/Team-2/ss1.rds")){
  ss1 <- MARSS(dat, model = model.list, method = "kem")
  saveRDS(ss1, file="Lab-2/Team-2/ss1.rds")
}
proc.time()[3] - ptm

# load in ss1
ss1 <- readRDS(file="Lab-2/Team-2/ss1.rds")

# grabbing data for figures
states <- ss1$states
statesSE <- ss1$states.se

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
save(states_long, file="Lab-2/Team-2/ss1states_long.Rda")

plot <- ggplot(data = states_long, aes(x = Year, y = fitted, group = state)) +
  geom_line() +
  labs(x = "", 
       y="State Estimate",
       title='Spring Summer Chinook Abundance, Upper Columbia/Snake River') +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  theme_classic() +
  theme(legend.position="bottom") + 
  scale_x_discrete(breaks = c(1950, 1964, 1978, 1992, 2006, 2020))
plot

# working on z matrix -> major pop group
esu2_key <- esu2[-c(1,2, 4:6, 8, 10)]
esu2_key <- aggregate(spawningyear ~ majorpopgroup + popname, data = esu2_key, FUN = mean)
esu2_key <- esu2_key[-c(3)]

# specify matrices for MARSS models
b.model <- "identity"
u.model <- matrix(paste0("u", seq(5)))
q.model <- "diagonal and equal"
z.model <- matrix(c(1,0,0,0,0,
                    0,1,0,0,0,
                    0,1,0,0,0,
                    0,1,0,0,0,
                    0,0,1,0,0,
                    0,1,0,0,0,
                    0,0,0,1,0,
                    0,0,0,0,1,
                    0,0,1,0,0,
                    0,0,1,0,0,
                    0,0,0,1,0,
                    0,1,0,0,0,
                    0,1,0,0,0,
                    0,1,0,0,0,
                    0,1,0,0,0,
                    0,0,1,0,0,
                    0,0,0,1,0,
                    0,0,0,1,0,
                    0,0,0,1,0,
                    0,0,0,1,0,
                    0,0,0,0,1,
                    0,0,0,0,1,
                    0,1,0,0,0,
                    1,0,0,0,0,
                    0,0,0,1,0,
                    0,0,1,0,0,
                    0,0,1,0,0,
                    0,0,0,1,0), 
                  nrow=28, ncol=5)
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
if(!file.exists("Lab-2/Team-2/ss1.1.rds")){
  ss1.1 <- MARSS(dat, model = model.list, method = "kem")
  saveRDS(ss1.1, file="Lab-2/Team-2/ss1.1.rds")
}
proc.time()[3] - ptm

# load in ss1
ss1.1 <- readRDS(file="Lab-2/Team-2/ss1.1.rds")

# grabbing data for figures
states <- ss1.1$states
statesSE <- ss1.1$states.se

states <- as.data.frame(states)
statesSE <- as.data.frame(statesSE)

states_long <- states %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="fitted")
states_long <- states_long %>% 
  mutate(Year = rep(years, 5))

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
save(states_long, file="Lab-2/Team-2/ss1.1states_long.Rda")

plot <- ggplot(data = states_long, aes(x = Year, y = fitted, group = state)) +
  geom_line() +
  labs(x = "", 
       y="State Estimate",
       title='Spring Summer Chinook Abundance, Upper Columbia/Snake River') +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  theme_classic() +
  theme(legend.position="bottom") +
  scale_x_discrete(breaks = c(1950, 1964, 1978, 1992, 2006, 2020))
plot

## QUESTION 2
b.model <- "identity"
u.model <- matrix(paste0("u", seq(5)))
q.model <- "diagonal and unequal"
z.model <- matrix(c(1,0,0,0,0,
                    0,1,0,0,0,
                    0,1,0,0,0,
                    0,1,0,0,0,
                    0,0,1,0,0,
                    0,1,0,0,0,
                    0,0,0,1,0,
                    0,0,0,0,1,
                    0,0,1,0,0,
                    0,0,1,0,0,
                    0,0,0,1,0,
                    0,1,0,0,0,
                    0,1,0,0,0,
                    0,1,0,0,0,
                    0,1,0,0,0,
                    0,0,1,0,0,
                    0,0,0,1,0,
                    0,0,0,1,0,
                    0,0,0,1,0,
                    0,0,0,1,0,
                    0,0,0,0,1,
                    0,0,0,0,1,
                    0,1,0,0,0,
                    1,0,0,0,0,
                    0,0,0,1,0,
                    0,0,1,0,0,
                    0,0,1,0,0,
                    0,0,0,1,0), 
                  nrow=28, ncol=5)
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
if(!file.exists("Lab-2/Team-2/ss2.rds")){
  ss2 <- MARSS(dat, model = model.list, method = "kem")
  saveRDS(ss2, file="Lab-2/Team-2/ss2.rds")
}
proc.time()[3] - ptm

# load in ss1
ss2 <- readRDS(file="Lab-2/Team-2/ss2.rds")

# grabbing data for figures
states <- ss2$states
statesSE <- ss2$states.se

states <- as.data.frame(states)
statesSE <- as.data.frame(statesSE)

states_long <- states %>% 
  pivot_longer("V1":"V76", names_to="Year", values_to="fitted")
states_long <- states_long %>% 
  mutate(Year = rep(years, 5))

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
save(states_long, file="Lab-2/Team-2/ss2states_long.Rda")

plot <- ggplot(data = states_long, aes(x = Year, y = fitted, group = state)) +
  geom_line() +
  labs(x = "", 
       y="State Estimate",
       title='Spring Summer Chinook Abundance, Upper Columbia/Snake River') +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  theme_classic() +
  theme(legend.position="bottom") +
  scale_x_discrete(breaks = c(1950, 1964, 1978, 1992, 2006, 2020))
plot

# check how much data is there per majorpopgroup?
esu2$data_available <- ifelse(is.na(esu2$value), 0, 1)
majpop_avaliable <- esu2[-c(1, 2, 4:10)]
aggregate(data_available ~ majorpopgroup, majpop_avaliable, sum)
