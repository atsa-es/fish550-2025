# libaries
library(dplyr)
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

##############################################################################################################

## fit HMM
# hidden state model
hidden <- MarkovChain$new(data = pdo, n_states=2)
  # begin by defining two states - warm and cool

# observation model
# options for other families in vignette
dists <- list(winter_pdo = "norm") 
# named list of starting values -- varies by family
par0 <- list(winter_pdo = list(mean = c(0.64,0.32), sd = c(0.985, 0.985)))
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
