# libaries
library(dplyr)
library(here)
library(hmmTMB)

# read pdo data
pdo <- rsoi::download_pdo()

# shift years so that winters line up
pdo$Year[which(pdo$Month%in%c("Oct","Nov","Dec"))] <- pdo$Year[which(pdo$Month%in%c("Oct","Nov","Dec"))] + 1
pdo <- dplyr::group_by(pdo, Year) %>%
  dplyr::summarize(winter_pdo = mean(PDO[which(Month %in% c("Oct","Nov","Dec","Jan","Feb"))])) %>% 
  dplyr::select(winter_pdo, Year)
# The first year will be missing Oct-Dec
pdo <- pdo[-1,]

# code from the lab .rmd^
##############################################################################################################

# plot
ggplot(pdo, aes(Year, winter_pdo)) + 
  geom_point() + ylab("PDO") + theme_bw()

## fit HMM
# hidden state model
hidden <- MarkovChain$new(data = pdo, n_states=2)
  # begin by defining two states - warm and cool

# observation model
# options for other families in vignette
dists <- list(winter_pdo = "norm") 
# named list of starting values -- varies by family
par0 <- list(winter_pdo = list(mean = c((mean(pdo$winter_pdo) - sd(pdo$winter_pdo)),(mean(pdo$winter_pdo) + sd(pdo$winter_pdo))), sd = c(0.985, 0.985)))
# create observation model
obs_model <- Observation$new(data = pdo,
                             dists = dists,
                             n_states = 2,
                             par = par0)

# construct HMM
hmm <- HMM$new(obs = obs_model,
               hid = hidden)

hmm$fit(silent=TRUE)
hmm

pdo$est_state <- factor(paste0("State", hmm$viterbi()))

ggplot(pdo, aes(Year, winter_pdo, col = est_state)) + 
  geom_point() + ylab("PDO state?") + theme_bw()

# try different seeds
best = 1.0e10
best_model = NA
iter = 20
for(i in 1:iter){
  set.seed(i)
  sig <- runif(1, 0.01, (1.5*sd(pdo$winter_pdo)))
  
  hidden <- MarkovChain$new(data = pdo, n_states=2)  
  
  dists <- list(winter_pdo = "norm") 
  
  par0 <- list(winter_pdo = list(mean = c((mean(pdo$winter_pdo) - sig), (mean(pdo$winter_pdo) + sig)), sd = c(sig, sig)))
  
  obs_model <- Observation$new(data = pdo,
                               dists = dists,
                               n_states = 2,
                               par = par0)
  
  hmm <- HMM$new(obs = obs_model,
                 hid = hidden)
  
  hmm$fit(silent=TRUE)
  
  if(hmm$AIC_conditional() < best) {
    best_model = hmm
    best = hmm$AIC_conditional()
  }
}

## PART 2 ###############################################################################################

stopl <- read.csv(here("Lab-4", "stoplight.csv"))
