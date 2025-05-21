library(data.table)
library(hmmTMB)

stoplight <- read.csv(here::here("Lab-4", "stoplight.csv"))

# Select only the local biological 
data <- stoplight %>%
  filter(Type == "Local Biological")

data <- data[-c(2)]
years <- colnames(data[2:length(data)])
species <- c(data$Ecosystem.Indicators)
data <- data[-c(1)]
data <- transpose(data)
colnames(data) <- species
cor(data)
data$year <- years
data$year <- substring(data$year, 2)

# specify markov model

set.seed(1)

# 2 state model
hidden2 <- MarkovChain$new(data = data, n_states = 2)
dists2 <- list(Copepod_richness = "norm",
              N_copepod = "norm",
              S_copepod = "norm",
              Biological_transition = "norm",
              Nearshore_Ichthyoplankton = "norm",
              Ichthy_community_index = "norm",
              Chinook_salmon_juv = "norm",
              Coho_salmon_juv = "norm")
par02 <- list(Copepod_richness = list(mean=c(mean(data$Copepod_richness)+sd(data$Copepod_richness),
                                             mean(data$Copepod_richness)-sd(data$Copepod_richness)), 
                                      sd=c(sd(data$Copepod_richness),
                                           sd(data$Copepod_richness))),
             N_copepod = list(mean=c(mean(data$N_copepod)+sd(data$N_copepod),
                                     mean(data$N_copepod)-sd(data$N_copepod)), 
                              sd=c(sd(data$N_copepod),
                                   sd(data$N_copepod))),
             S_copepod = list(mean=c(mean(data$S_copepod)+sd(data$S_copepod),
                                     mean(data$S_copepod)-sd(data$S_copepod)), 
                              sd=c(sd(data$S_copepod),
                                   sd(data$S_copepod))),
             Biological_transition = list(mean=c(mean(data$Biological_transition)+sd(data$Biological_transition),
                                                 mean(data$Biological_transition)-sd(data$Biological_transition)), 
                                          sd=c(sd(data$Biological_transition),
                                               sd(data$Biological_transition))),
             Nearshore_Ichthyoplankton = list(mean=c(mean(data$Nearshore_Ichthyoplankton)+sd(data$Nearshore_Ichthyoplankton),
                                                     mean(data$Nearshore_Ichthyoplankton)-sd(data$Nearshore_Ichthyoplankton)), 
                                              sd=c(sd(data$Nearshore_Ichthyoplankton),
                                                   sd(data$Nearshore_Ichthyoplankton))),
             Ichthy_community_index = list(mean=c(mean(data$Ichthy_community_index)+sd(data$Ichthy_community_index),
                                                  mean(data$Ichthy_community_index)-sd(data$Ichthy_community_index)), 
                                           sd=c(sd(data$Ichthy_community_index),
                                                sd(data$Ichthy_community_index))),
             Chinook_salmon_juv = list(mean=c(mean(data$Chinook_salmon_juv)+sd(data$Chinook_salmon_juv),
                                              mean(data$Chinook_salmon_juv)-sd(data$Chinook_salmon_juv)), 
                                       sd=c(sd(data$Chinook_salmon_juv),
                                            sd(data$Chinook_salmon_juv))),
             Coho_salmon_juv = list(mean=c(mean(data$Coho_salmon_juv)+sd(data$Coho_salmon_juv),
                                           mean(data$Coho_salmon_juv)-sd(data$Coho_salmon_juv)), 
                                    sd=c(sd(data$Coho_salmon_juv),
                                         sd(data$Coho_salmon_juv)))             
             )
obs_model2 <- Observation$new(data = data,
                             dists = dists2,
                             n_states = 2,
                             par = par02)
hmm2 <- HMM$new(obs = obs_model2,
               hid = hidden2)
hmm2$fit(silent=FALSE)
hmm2

data$est_state2 <- factor(paste0("State", hmm2$viterbi()))

# 3 state model
hidden3 <- MarkovChain$new(data = data, n_states = 3)
dists3 <- list(Copepod_richness = "norm",
              N_copepod = "norm",
              S_copepod = "norm",
              Biological_transition = "norm",
              Nearshore_Ichthyoplankton = "norm",
              Ichthy_community_index = "norm",
              Chinook_salmon_juv = "norm",
              Coho_salmon_juv = "norm")
par03 <- list(Copepod_richness = list(mean=c(mean(data$Copepod_richness)+1.5*sd(data$Copepod_richness),
                                             mean(data$Copepod_richness),
                                             mean(data$Copepod_richness)-1.5*sd(data$Copepod_richness)),
                                      sd=c(sd(data$Copepod_richness),
                                           sd(data$Copepod_richness),
                                           sd(data$Copepod_richness))),
             N_copepod = list(mean=c(mean(data$N_copepod)+1.5*sd(data$N_copepod),
                                     mean(data$N_copepod),
                                     mean(data$N_copepod)-1.5*sd(data$N_copepod)), 
                              sd=c(sd(data$N_copepod),
                                   sd(data$N_copepod),
                                   sd(data$N_copepod))),
             S_copepod = list(mean=c(mean(data$S_copepod)+1.5*sd(data$S_copepod),
                                     mean(data$S_copepod),
                                     mean(data$S_copepod)-1.5*sd(data$S_copepod)), 
                              sd=c(sd(data$S_copepod),
                                   sd(data$S_copepod),
                                   sd(data$S_copepod))),
             Biological_transition = list(mean=c(mean(data$Biological_transition)+1.5*sd(data$Biological_transition),
                                                 mean(data$Biological_transition),
                                                 mean(data$Biological_transition)-1.5*sd(data$Biological_transition)), 
                                          sd=c(sd(data$Biological_transition),
                                               sd(data$Biological_transition),
                                               sd(data$Biological_transition))),
             Nearshore_Ichthyoplankton = list(mean=c(mean(data$Nearshore_Ichthyoplankton)+1.5*sd(data$Nearshore_Ichthyoplankton),
                                                     mean(data$Nearshore_Ichthyoplankton),
                                                     mean(data$Nearshore_Ichthyoplankton)-1.5*sd(data$Nearshore_Ichthyoplankton)), 
                                              sd=c(sd(data$Nearshore_Ichthyoplankton),
                                                   sd(data$Nearshore_Ichthyoplankton),
                                                   sd(data$Nearshore_Ichthyoplankton))),
             Ichthy_community_index = list(mean=c(mean(data$Ichthy_community_index)+1.5*sd(data$Ichthy_community_index),
                                                  mean(data$Ichthy_community_index),
                                                  mean(data$Ichthy_community_index)-1.5*sd(data$Ichthy_community_index)), 
                                           sd=c(sd(data$Ichthy_community_index),
                                                sd(data$Ichthy_community_index),
                                                sd(data$Ichthy_community_index))),
             Chinook_salmon_juv = list(mean=c(mean(data$Chinook_salmon_juv)+1.5*sd(data$Chinook_salmon_juv),
                                              mean(data$Chinook_salmon_juv),
                                              mean(data$Chinook_salmon_juv)-1.5*sd(data$Chinook_salmon_juv)), 
                                       sd=c(sd(data$Chinook_salmon_juv),
                                            sd(data$Chinook_salmon_juv),
                                            sd(data$Chinook_salmon_juv))),
             Coho_salmon_juv = list(mean=c(mean(data$Coho_salmon_juv)+1.5*sd(data$Coho_salmon_juv),
                                           mean(data$Coho_salmon_juv),
                                           mean(data$Coho_salmon_juv)-1.5*sd(data$Coho_salmon_juv)), 
                                    sd=c(sd(data$Coho_salmon_juv),
                                         sd(data$Coho_salmon_juv),
                                         sd(data$Coho_salmon_juv)))             
             )
obs_model3 <- Observation$new(data = data,
                             dists = dists3,
                             n_states = 3,
                             par = par03)
hmm3 <- HMM$new(obs = obs_model3,
               hid = hidden3)
hmm3$fit(silent=FALSE)
hmm3

data$est_state3 <- factor(paste0("State", hmm3$viterbi()))

ggplot(data, aes(x=year, y=est_state2, col = est_state2)) + 
  geom_point() + 
  ylab("2 state") + 
  xlab("")+
  theme_classic()

ggplot(data, aes(x=year, est_state3, col = est_state3)) + 
  geom_point() + 
  ylab("3 state") + 
  xlab("")+
  theme_classic()