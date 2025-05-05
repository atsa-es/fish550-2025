# Plot time series of densities for the different taxa and environmental covariates.
# 
# Select an appropriate time period of at least 5 years (60 months) and fit different forms of DFA models with and without covariates.
# 
# Evaluate the model fits relative to the observed data for your best (or top 3) models.

library(MARSS)
library(ggplot2)
library(dplyr)
library(ggpubr)

data(lakeWAplankton, package = "MARSS")

data = lakeWAplanktonTrans

zooplankton = c("Conochilus", "Cyclops", "Daphnia", "Diaptomus", "Epischura",
                "Leptodora", "Non.daphnid.cladocerans", "Non.colonial.rotifers") # I'm excluding 'Neomysis' since they dont have lots of data

env_covars = c("Temp", "TP", "pH")

time_vars = c("Year", "Month")

combined = unique(c(zooplankton, env_covars, time_vars))

# filter data
data = lakeWAplanktonTrans[, colnames(lakeWAplanktonTrans) %in% combined, drop = FALSE]

# ensure filtering is correct
colnames(data)

# select zooplankton only and de-mean data
zoop_data = data[, colnames(data) %in% zooplankton, drop = FALSE]
y_mean    = apply(zoop_data, 2, mean, na.rm = TRUE)
zoop_data = zoop_data - y_mean

# select last 10 years only
zoop_data = zoop_data[120:dim(zoop_data)[2],]

# transpose so time is columns 
zoop_data = t(zoop_data)
N_ts      = nrow(zoop_data)
TT        = ncol(zoop_data)

## plotting data

cnt = 1
par(mfrow = c(4, 2), mai = c(0.5, 0.7, 0.15, 0.5), omi = c(0, 0, 0, 0))

for (i in zooplankton) {
  plot(zoop_data[cnt, ], type = "l", xlab = "Time", ylab = i,
       main = paste("Time series of", i), ylim = c(-2, 2))
  cnt = cnt + 1
}

#                 ###### Model 1: 3 hidden states ######
# 
# ## 1. Observation model
# Z_vals = list("z11",  0  ,  0  ,
#               "z21","z22",  0  ,
#               "z31","z32","z33",
#               "z41","z42","z43",
#               "z51","z52","z53",
#               "z61","z62","z63",
#               "z71","z72","z73",
#               "z81","z82","z83")
# ZZ = matrix(Z_vals, nrow = N_ts, ncol = 3, byrow = TRUE)
# aa = "zero"
# DD = "zero"                 # no covariates
# dd = "zero" 
# RR = "diagonal and unequal" # observation error variance-covariance matrix
# 
# ## 2. Process model
# 
# mm = 3           # number of hidden states
# BB = "identity"  # diag(mm)
# uu = "zero"      # process bias (i.e. growth rate)
# CC = "zero"      # no covariates
# cc = "zero"  
# QQ = "identity"  # process error variance-covariance matrix
# 
# mod_list = list(Z = ZZ, A = aa, D = DD, d = dd, R = RR,
#                  B = BB, U = uu, C = CC, c = cc, Q = QQ)
# 
# init_list = list(x0 = matrix(rep(0, mm), mm, 1))
# 
# con_list = list(maxit = 10000, allow.degen = TRUE)
# dfa_1 = MARSS(y = zoop_data, model = mod_list, inits = init_list, control = con_list)

###### Model 1: 1 Hidden State ######

## 1. Observation model
Z_vals_1 = list(  "z11"  ,  
                  "z21",
                  "z31",
                  "z41",
                  "z51",
                  "z61",
                  "z71",
                  "z81" )

ZZ_1 = matrix(Z_vals_1, nrow = N_ts, ncol = 1, byrow = TRUE)
aa_1 = "zero"
DD_1 = "zero"                 # no covariates
dd_1 = "zero"
RR_1 = "diagonal and unequal" # observation error variance-covariance matrix

## 2. Process model
mm_1 = 1           # number of hidden states
BB_1 = "identity"  # diag(mm)
uu_1 = "zero"      # process bias (i.e. growth rate)
CC_1 = "zero"      # no covariates
cc_1 = "zero"
QQ_1 = "identity"  # process error variance-covariance matrix

mod_list_1 = list(Z = ZZ_1, A = aa_1, D = DD_1, d = dd_1, R = RR_1,
                  B = BB_1, U = uu_1, C = CC_1, c = cc_1, Q = QQ_1)

init_list_1 = list(x0 = 0)

con_list_1   = list(maxit = 1000, allow.degen = TRUE)
dfa_1_hidden = MARSS(y = zoop_data, model = mod_list_1, inits = init_list_1, control = con_list_1)

## Plot fits against data
model_1_fits    = dfa_1_hidden$ytT
model_1_fits_se = dfa_1_hidden$ytT.se

par(mfrow = c(4, 2), mai = c(0.5, 0.7, 0.15, 0.5), omi = c(0, 0, 0, 0))
cnt = 1
for (i in zooplankton) {
  plot(zoop_data[cnt, ], type = "p", xlab = "Time", ylab = i,
       main = paste("Time series of", i), ylim = c(-2, 2))
  lines(model_1_fits[cnt, ], col = "red")
  lines(model_1_fits[cnt, ] + model_1_fits_se[cnt, ], col = "blue", lty = 2)
  lines(model_1_fits[cnt, ] - model_1_fits_se[cnt, ], col = "blue", lty = 2)
  cnt = cnt + 1
}

## Plot states and loadings
Z_est_1 = coef(dfa_1_hidden, type = "matrix")$Z
H_inv_1 = varimax(Z_est_1)

states_1 = as.vector(dfa_1_hidden$states) 
states_1 = data.frame(1:TT, states_1); names(states_1) = c("Time", "State")

states_plot_1 = states_1 %>%
  ggplot(aes(x = Time, y = State)) +
  geom_line() +
  theme_minimal()

loadings_1 = as.vector(H_inv_1[,1])
loadings_1 = data.frame(loadings_1, zooplankton)

loadings_plot_1 = loadings_1 %>%
  ggplot(aes(x = zooplankton, y = loadings_1)) +
  geom_bar(stat = "identity", fill = "white", color = "black") +
  theme_minimal() +
  coord_flip() +
  ylab("") +
  xlab("Loadings")

ggarrange(states_plot_1, loadings_plot_1)

###### Model 2: 2 Hidden States ######

## 1. Observation model
Z_vals_2 = list(  0  , 0  ,
                "z21", 0  ,
                "z31","z32",
                "z41","z42",
                "z51","z52",
                "z61","z62",
                "z71","z72",
                "z81","z82")
ZZ_2 = matrix(Z_vals_2, nrow = N_ts, ncol = 2, byrow = TRUE)
aa_2 = "zero"
DD_2 = "zero"                 # no covariates
dd_2 = "zero"
RR_2 = "diagonal and unequal" # observation error variance-covariance matrix

## 2. Process model
mm_2 = 2           # number of hidden states
BB_2 = "identity"  # diag(mm)
uu_2 = "zero"      # process bias (i.e. growth rate)
CC_2 = "zero"      # no covariates
cc_2 = "zero"
QQ_2 = "identity"  # process error variance-covariance matrix

mod_list_2 = list(Z = ZZ_2, A = aa_2, D = DD_2, d = dd_2, R = RR_2,
                  B = BB_2, U = uu_2, C = CC_2, c = cc_2, Q = QQ_2)

init_list_2 = list(x0 = matrix(rep(0, mm_2), mm_2, 1))

con_list_2 = list(maxit = 1000, allow.degen = TRUE)
dfa_2_hidden = MARSS(y = zoop_data, model = mod_list_2, inits = init_list_2, control = con_list_2)

## Plot fits against data
model_2_fits    = dfa_2_hidden$ytT
model_2_fits_se = dfa_2_hidden$ytT.se

par(mfrow = c(4, 2), mai = c(0.5, 0.7, 0.15, 0.5), omi = c(0, 0, 0, 0))
cnt = 1
for (i in zooplankton) {
  plot(zoop_data[cnt, ], type = "p", xlab = "Time", ylab = i,
       main = paste("Time series of", i), ylim = c(-2, 2))
  lines(model_2_fits[cnt, ], col = "red")
  lines(model_2_fits[cnt, ] + model_2_fits_se[cnt, ], col = "blue", lty = 2)
  lines(model_2_fits[cnt, ] - model_2_fits_se[cnt, ], col = "blue", lty = 2)
  cnt = cnt + 1
}

## Plot states and loadings
Z_est_2 = coef(dfa_2_hidden, type = "matrix")$Z
H_inv_2 = varimax(Z_est_2)$rotmat

states_2 = as.data.frame(t(dfa_2_hidden$states))
states_2 = data.frame(1:TT, states_2); names(states_2) = c("Time", "State 1", "State 2")

states_plot_2 = states_2 %>%
  ggplot(aes(x = Time)) +
  geom_line(aes(y = `State 1`, color = "State 1")) +
  geom_line(aes(y = `State 2`, color = "State 2")) +
  theme_minimal() +
  ylab("States") +
  scale_color_manual(values = c("blue", "red")) +
  theme(legend.title = element_blank(),
        legend.position = "top")

loadings_2 = as.data.frame(Z_est_2)
loadings_2 = data.frame(loadings_2, zooplankton)
loadings_plot_2 = loadings_2 %>%
  ggplot(aes(x = zooplankton)) +
  geom_bar(aes(y = X1), stat = "identity", fill = "blue", color = "black") +
  geom_bar(aes(y = X2), stat = "identity", fill = "red", color = "black") +
  theme_minimal() +
  coord_flip() +
  ylab("") +
  xlab("Loadings")

ggarrange(states_plot_2, loadings_plot_2)

###### Model 3: 3 Hidden States ######

## 1. Observation model
Z_vals_3 = list("z11",  0  ,  0  ,
                "z21","z22",  0  ,
                "z31","z32","z33",
                "z41","z42","z43",
                "z51","z52","z53",
                "z61","z62","z63",
                "z71","z72","z73",
                "z81","z82","z83")
ZZ_3 = matrix(Z_vals_3, nrow = N_ts, ncol = 3, byrow = TRUE)
aa_3 = "zero"
DD_3 = "zero"                 # no covariates
dd_3 = "zero"
RR_3 = "diagonal and unequal" # observation error variance-covariance matrix

## 2. Process model
mm_3 = 3           # number of hidden states
BB_3 = "identity"  # diag(mm)
uu_3 = "zero"      # process bias (i.e. growth rate)
CC_3 = "zero"      # no covariates
cc_3 = "zero"
QQ_3 = "identity"  # process error variance-covariance matrix

mod_list_3 = list(Z = ZZ_3, A = aa_3, D = DD_3, d = dd_3, R = RR_3,
                  B = BB_3, U = uu_3, C = CC_3, c = cc_3, Q = QQ_3)

init_list_3 = list(x0 = matrix(rep(0, mm_3), mm_3, 1))

con_list_3 = list(maxit = 1000, allow.degen = TRUE)
dfa_3_hidden = MARSS(y = zoop_data, model = mod_list_3, inits = init_list_3, control = con_list_3)

## Plot fits against data
model_3_fits    = dfa_3_hidden$ytT
model_3_fits_se = dfa_3_hidden$ytT.se
par(mfrow = c(4, 2), mai = c(0.5, 0.7, 0.15, 0.5), omi = c(0, 0, 0, 0))
cnt = 1
for (i in zooplankton) {
  plot(zoop_data[cnt, ], type = "p", xlab = "Time", ylab = i,
       main = paste("Time series of", i), ylim = c(-2, 2))
  lines(model_3_fits[cnt, ], col = "red")
  lines(model_3_fits[cnt, ] + model_3_fits_se[cnt, ], col = "blue", lty = 2)
  lines(model_3_fits[cnt, ] - model_3_fits_se[cnt, ], col = "blue", lty = 2)
  cnt = cnt + 1
}

## Plot states and loadings
Z_est_3 = coef(dfa_3_hidden, type = "matrix")$Z
H_inv_3 = varimax(Z_est_3)$rotmat

states_3 = as.data.frame(t(dfa_3_hidden$states))
states_3 = data.frame(1:TT, states_3); names(states_3) = c("Time", "State 1", "State 2", "State 3")
states_plot_3 = states_3 %>%
  ggplot(aes(x = Time)) +
  geom_line(aes(y = `State 1`, color = "State 1")) +
  geom_line(aes(y = `State 2`, color = "State 2")) +
  geom_line(aes(y = `State 3`, color = "State 3")) +
  theme_minimal() +
  ylab("States") +
  scale_color_manual(values = c("blue", "red", "green4")) +
  theme(legend.title = element_blank(),
        legend.position = "top")

loadings_3 = as.data.frame(Z_est_3)
loadings_3 = data.frame(loadings_3, zooplankton)
loadings_plot_3 = loadings_3 %>%
  ggplot(aes(x = zooplankton)) +
  geom_bar(aes(y = X1), stat = "identity", fill = "blue", color = "black") +
  geom_bar(aes(y = X2), stat = "identity", fill = "red", color = "black") +
  geom_bar(aes(y = X3), stat = "identity", fill = "green4", color = "black") +
  theme_minimal() +
  coord_flip() +
  ylab("") +
  xlab("Loadings")

ggarrange(states_plot_3, loadings_plot_3)

### Make a model comparison table
model_comparison = data.frame(
  Model = c("Model 1", "Model 2", "Model 3"),
  AIC = c(dfa_1_hidden$AIC, dfa_2_hidden$AIC, dfa_3_hidden$AIC)
)
model_comparison = model_comparison %>%
  mutate(Delta_AIC = AIC - min(AIC),
         Weight = exp(-0.5 * Delta_AIC) / sum(exp(-0.5 * Delta_AIC)))

model_comparison








