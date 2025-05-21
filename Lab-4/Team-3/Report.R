knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
options(dplyr.summarise.inform = FALSE)

library(ggplot2)
library(dplyr)
library(tidyr)
library(hmmTMB)

#install.packages("rsoi")
pdo <- rsoi::download_pdo()
stoplight <- read.csv(here::here("Lab-4", "stoplight.csv"))

# Select only the local biological 
biological <- stoplight %>%
  filter(Type == "Local Biological")


### Adjust the data for PDO 
pdo$Year[which(pdo$Month%in%c("Oct","Nov","Dec"))] <- pdo$Year[which(pdo$Month%in%c("Oct","Nov","Dec"))] + 1
pdo <- dplyr::group_by(pdo, Year) %>%
  dplyr::summarize(winter_pdo = mean(PDO[which(Month %in% c("Oct","Nov","Dec","Jan","Feb"))])) %>% 
  dplyr::select(winter_pdo, Year)
# The first year will be missing Oct-Dec
pdo <- pdo[-1,]


plot <- ggplot(pdo, aes(x = Year, y = winter_pdo)) +
  geom_line() +
  labs(title = "Winter PDO vs Year", x = "Year", y = "Winter PDO") +
  theme_classic()

plot


# Normalize each row to range from 0 to 1
biological_normalized <- biological
biological_normalized[, -(1:2)] <- t(apply(biological[, -(1:2)], 1, function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}))

# log transform the results 
biological_normalized[, -(1:2)] <- log(pmax(biological_normalized[, -(1:2)], 1e-6))
# Check the results
print("Normalized data (first few rows):")
print(biological_normalized[1:3, 1:6])


# Pivot the data to long format for plotting
biological_long <- biological_normalized %>%
  pivot_longer(cols = -(1:2), names_to = "Year", values_to = "Value")

# Create the plot faceted by Ecosystem.Indicators
ggplot(biological_long, aes(x = Year, y = Value, group = 1)) +
  geom_line(color = "blue", alpha = 0.7) +
  geom_point(color = "darkblue", size = 0.8) +
  facet_wrap(~ Ecosystem.Indicators, scales = "free_x") +
  labs(title = "Normalized Biological Indicators (0-1 Scale)",
       x = "Year", 
       y = "Normalized Value",
       subtitle = "Each indicator normalized to range from 0 to 1") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10),
        plot.title = element_text(size = 14))




library(hmmTMB)

pdo <- data.frame(
  ID = 1,
  time = pdo$Year,  # Use actual years as time
  winter_pdo = pdo$winter_pdo
)

# Set up hidden states (2 states for different PDO regimes)
hidden <- MarkovChain$new(data = pdo, n_states = 2)

# Use normal distribution for continuous PDO values
dists <- list(winter_pdo = "norm")

# Initial parameters for normal distribution (mean and sd for each state)
par0 <- list(winter_pdo = list(mean = c(-0.5, 0.5),    # Different means for each state
                               sd = c(0.8, 0.8)))      # Standard deviations

# Create observation model
obs_model <- Observation$new(data = pdo,
                             dists = dists,
                             n_states = 2,
                             par = par0)

# Fit the HMM
hmm <- HMM$new(obs = obs_model, hid = hidden)
hmm$fit(silent = TRUE)


hmm

hmm$par()

pdo$est_state <- factor(paste0("State", hmm$viterbi()))

ggplot(pdo, aes(time, winter_pdo, col = est_state)) + 
  geom_point(size = 2) + # This line will connect all the points
  ylab("Warm or Cold?") + 
  xlab("") + 
  theme_classic()

# Placeholder for storing models and log-likelihoods
models <- vector("list", 20)
log_liks <- numeric(20)

# Run 20 iterations
for (i in 1:20) {
  
  # Optionally randomize initial parameters slightly
  par0 <- list(winter_pdo = list(
    mean = rnorm(2, mean = c(-0.5, 0.5), sd = 0.2),  # jittered means
    sd = runif(2, min = 0.5, max = 1.2)              # jittered sds
  ))
  
  # Set up models
  hidden <- MarkovChain$new(data = pdo, n_states = 2)
  obs_model <- Observation$new(data = pdo, dists = list(winter_pdo = "norm"),
                               n_states = 2, par = par0)
  
  # Fit the model
  hmm <- HMM$new(obs = obs_model, hid = hidden)
  hmm$fit(silent = TRUE)
  
  # Store the model and log-likelihood
  models[[i]] <- hmm
  log_liks[i] <- logLik(hmm)
}

# Create a data frame for plotting
loglik_df <- data.frame(Iteration = 1:20, LogLikelihood = log_liks)

# Plot log-likelihoods
ggplot(loglik_df, aes(x = Iteration, y = LogLikelihood)) +
  geom_point() +
  theme_classic() +
  labs(title = "Log-Likelihood Across HMM Fits", y = "Log-Likelihood", x = "Iteration")

# Identify best model
best_index <- which.max(log_liks)
best_model <- models[[best_index]]
cat("Best model is at iteration", best_index, "with log-likelihood", log_liks[best_index], "\n")


# Extract the state probabilities 
state_probs<-best_model$state_probs()

# plot each of these 
library(ggplot2)

ggplot(pdo, aes(x = time)) +
  geom_line(aes(y = state_probs[,1], col = "State 1")) +
  geom_point(aes(y = state_probs[,1], col = "State 1")) +
  geom_line(aes(y = state_probs[,2], col = "State 2")) +
  geom_point(aes(y = state_probs[,2], col = "State 2")) +
  labs(title = "Posterior Probability of States", x = "Year", y = "Probability") +
  theme_classic() +
  scale_color_manual(values = c("State 1" = "blue", "State 2" = "red")) +
  theme(legend.title = element_blank())

trans_mat <- as.data.frame(best_model$par()$tpm)

#Can also get it this way but can't get df
#best_model$print_tpm()

calculate_stationary_distribution <- function(P) { # must be a square matrix

  eigen_analysis <- eigen(t(P)) # Transpose the matrix
  eigenvalues <- eigen_analysis$values
  eigenvectors <- eigen_analysis$vectors

  # Find the eigenvalue closest to 1
  target_eigenvalue_index <- which.min(abs(eigenvalues - 1))
  target_eigenvector <- eigenvectors[, target_eigenvalue_index]

  # Normalize the eigenvector to sum to 1
  stationary_distribution <- target_eigenvector / sum(target_eigenvector)

  # Check that probabilities are real and positive
  if(any(Im(stationary_distribution) != 0) || any(Re(stationary_distribution) < 0)){
    n_states <- nrow(P)
    initial_distribution <- rep(1/n_states, n_states) # Start with equal probabilities
    for (i in 1:1000) {  # Iterate until convergence (adjust as needed)
      initial_distribution <- initial_distribution %*% P
    }
    stationary_distribution <- as.vector(initial_distribution)
  } else {
    stationary_distribution <- Re(stationary_distribution)
  }
  return(stationary_distribution)
}

# Calculate the stationary distribution
long_run_probs <- calculate_stationary_distribution(trans_mat)

# Print the probs
cat("Long-run probability of being in State 1:", long_run_probs[1], "\n")
cat("Long-run probability of being in State 2:", long_run_probs[2], "\n")

cat("The PDO is predicted to be in State 1 approximately", round(long_run_probs[1]*100,2), "% of the time, and in State 2 approximately", round(long_run_probs[2]*100,2), "% of the time.\n")


biological_reshaped <- biological_normalized %>%

  select(-Type) %>%
  # Convert to long format first
  pivot_longer(cols = -Ecosystem.Indicators, names_to = "Year", values_to = "Value") %>%
  mutate(time = as.numeric(gsub("^X", "", Year))) %>%
  mutate(ID = 1) %>%
  # Convert to wide format with Ecosystem.Indicators as column names
  pivot_wider(names_from = Ecosystem.Indicators, values_from = Value)

# Print the first few rows to check
print("Reshaped data (first few rows):")
print(head(biological_reshaped))



set.seed(666)

#For all models
# Define distributions for each indicator
dists <- list(
 Copepod_richness = "norm", 
 N_copepod = "norm", 
 S_copepod = "norm",
 Biological_transition = "norm", 
 Nearshore_Ichthyoplankton = "norm",
 Ichthy_community_index = "norm",
 Chinook_salmon_juv = "norm",
 Coho_salmon_juv = "norm"
)

# Define initial parameters for each indicator
# Need to match the names in dists, not par0
# par0 <- list(
#   Copepod_richness = list(mean = c(0.3, 0.7), sd = c(0.2, 0.2)),
#   N_copepod = list(mean = c(0.3, 0.7), sd = c(0.2, 0.2)),
#   S_copepod = list(mean = c(0.3, 0.7), sd = c(0.2, 0.2)),
#   Biological_transition = list(mean = c(0.3, 0.7), sd = c(0.2, 0.2)),
#   Nearshore_Ichthyoplankton = list(mean = c(0.3, 0.7), sd = c(0.2, 0.2)),
#   Ichthy_community_index = list(mean = c(0.3, 0.7), sd = c(0.2, 0.2)),
#   Chinook_salmon_juv = list(mean = c(0.3, 0.7), sd = c(0.2, 0.2)),
#   Coho_salmon_juv = list(mean = c(0.3, 0.7), sd = c(0.2, 0.2))
# )

par0 <- list(
  Copepod_richness = list(mean = log(c(0.3, 0.7)), sd = c(0.2, 0.2)),
  N_copepod = list(mean = log(c(0.3, 0.7)), sd = c(0.2, 0.2)),
  S_copepod = list(mean = log(c(0.3, 0.7)), sd = c(0.2, 0.2)),
  Biological_transition = list(mean = log(c(0.3, 0.7)), sd = c(0.2, 0.2)),
  Nearshore_Ichthyoplankton = list(mean = log(c(0.3, 0.7)), sd = c(0.2, 0.2)),
  Ichthy_community_index = list(mean = log(c(0.3, 0.7)), sd = c(0.2, 0.2)),
  Chinook_salmon_juv = list(mean = log(c(0.3, 0.7)), sd = c(0.2, 0.2)),
  Coho_salmon_juv = list(mean = log(c(0.3, 0.7)), sd = c(0.2, 0.2))
)


#########################################################################
# 2 state 

# Create hidden Markov chain
hidden2 <- MarkovChain$new(data = biological_reshaped, n_states = 2)


# Create observation model using the correct data
# Should use biological_normalized,
obs_model2 <- Observation$new(data = biological_reshaped,
                            dists = dists,
                            n_states = 2,
                            par = par0)

# Create HMM
hmm2 <- HMM$new(obs = obs_model2, 
               hid = hidden2)

# Fit the model
hmm2$fit(silent = TRUE)
 
# Print results
print("Model fitting completed")
print(hmm2)
 
 
# Collect the state estimates 

biological_reshaped$est_state <- factor(paste0("State", hmm$viterbi()))
 
hmm2_series <- biological_reshaped

ggplot(hmm2_series, aes(Year, as.numeric(est_state), col = est_state)) + 
   geom_line(aes(group = 1), color = "black") + 
   geom_point(size = 3) +
   scale_y_continuous(breaks = c(1, 2), labels = c("State 1", "State 2")) +
   labs(title = "HMM State Changes Over Time",
        x = "Year", 
        y = "State",
        color = "Est. State") + 
   theme_classic()

#########################################################################



# 3 state HMM model
set.seed(123)

# First, ensure data is prepared correctly (exclude Year from model)
data_for_hmm <- biological_reshaped %>% select(-Year)

# Create hidden Markov chain
hidden3 <- MarkovChain$new(data = data_for_hmm, n_states = 3)

# Define distributions for each indicator
dists <- list(
  Copepod_richness = "norm", 
  N_copepod = "norm", 
  S_copepod = "norm",
  Biological_transition = "norm", 
  Nearshore_Ichthyoplankton = "norm",
  Ichthy_community_index = "norm",
  Chinook_salmon_juv = "norm",
  Coho_salmon_juv = "norm"
)

# Define initial parameters for 3 states
# State 1: Low values (mean ≈ 0.2)
# State 2: Medium values (mean ≈ 0.5)
# State 3: High values (mean ≈ 0.8)
par0 <- list(
  Copepod_richness = list(mean = c(0.2, 0.5, 0.8), sd = c(0.15, 0.15, 0.15)),
  N_copepod = list(mean = c(0.2, 0.5, 0.8), sd = c(0.15, 0.15, 0.15)),
  S_copepod = list(mean = c(0.2, 0.5, 0.8), sd = c(0.15, 0.15, 0.15)),
  Biological_transition = list(mean = c(0.2, 0.5, 0.8), sd = c(0.15, 0.15, 0.15)),
  Nearshore_Ichthyoplankton = list(mean = c(0.2, 0.5, 0.8), sd = c(0.15, 0.15, 0.15)),
  Ichthy_community_index = list(mean = c(0.2, 0.5, 0.8), sd = c(0.15, 0.15, 0.15)),
  Chinook_salmon_juv = list(mean = c(0.2, 0.5, 0.8), sd = c(0.15, 0.15, 0.15)),
  Coho_salmon_juv = list(mean = c(0.2, 0.5, 0.8), sd = c(0.15, 0.15, 0.15))
)

# Create observation model
obs_model3 <- Observation$new(data = data_for_hmm,
                            dists = dists,
                            n_states = 3,
                            par = par0)

# Create HMM
hmm <- HMM$new(obs = obs_model3, 
               hid = hidden3)

# Fit the model
print("Fitting 3-state HMM model...")
hmm$fit(silent = TRUE)

# Collect the state estimates

biological_reshaped$est_state <- factor(paste0("State", hmm$viterbi()))
hmm3_series <- biological_reshaped

ggplot(hmm3_series, aes(Year, as.numeric(est_state), col = est_state)) +
   geom_line(aes(group = 1), color = "black") +
   geom_point(size = 3) +
   scale_y_continuous(breaks = c(1, 2), labels = c("State 1", "State 2")) +
   labs(title = "HMM State Changes Over Time",
        x = "Year",
        y = "State",
        color = "Est. State") +
   theme_classic()



#For initial hmm
pseudo <- best_model$pseudores()

ggplot(as.data.frame(pseudo), aes(sample = pdo$winter_pdo)) +
  geom_qq() +
  geom_qq_line(col="red") +
  ggtitle("QQ Plot")

best_model$AIC_marginal()

best_model$AIC_conditional()


#For biological data

# hmm1$AIC_marginal()
# 
# hmm2$AIC_marginal()
# 
# hmm3$AIC_marginal()








pdo$est_state <- factor(paste0("State", best_model$viterbi()))

ggplot(pdo, aes(time, winter_pdo, col = est_state)) + 
  geom_line(aes(group = 1), color = "black") + 
  geom_point(size = 2) + # This line will connect all the points
  ylab("Rained?") + 
  theme_classic()


trans_mat

