library(atsalibrary)
library(forecast)
library(zoo)
library(tidyverse)
library(ggpubr)

# Formatting data --------------------------------------------------------------
chinook.month = chinook.month %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"),
                        format = "%Y-%b-%d"))
 
chinook.month$Month2 = as.numeric(as.factor(chinook.month$Month))

# Plotting data ----------------------------------------------------------------

## monthly data
chinook.month %>%
  ggplot(aes(x = Date, y = metric.tons)) +
  geom_line() +
  facet_wrap(~ State, ncol = 1) +
  labs(title = "Monthly Chinook Salmon Landings",
       x = "Date",
       y = "Log(Metric Tons)") +
  theme_minimal()

## yearly data
chinook.year %>%
  filter(State %in% c("California", "Oregon","Washington")) %>%
  ggplot(aes(x = Year, y = metric.tons)) +
  geom_line() +
  facet_wrap(~ State, ncol = 1, scales = "free_y") +
  labs(title = "Yearly Chinook Salmon Landings",
       x = "Year",
       y = "Log(Metric Tons)") +
  theme_minimal()

# Fitting ARIMA models ---------------------------------------------------------

# Question to explore: comparing the accuracy of forecasts between different lengths of testing data

## Monthly data ##
month_ts = ts(chinook.month$log.metric.tons, start = c(1980, 1), frequency = 12)

# Create test data with different ts lenghts
ts_lengths = c(5,10,15,20,25)
month_train = list()
month_test  = list()
for (i in 1:length(ts_lengths)) {
  
  month_train[[i]] = window(month_ts,
                            start = c(1980, 1),
                            end   = c(1980 + ts_lengths[i], 12))
 
  month_test[[i]]  = window(month_ts,
                            start = c(1980 + ts_lengths[i] + 1, 1),
                            end = c(2019, 12))
   
}


# Fit ARIMA models to all time series 
train_models = list()
for (i in 1:length(month_train)) train_models[[i]] = auto.arima(month_train[[i]])

# Compare forecast against test data
fr = list()
for (i in 1:length(month_train)) fr[[i]] = forecast(train_models[[i]], h = length(month_test[[i]])*2)

plots = list()
for (i in 1:length(month_train)) {
  plots[[i]] = autoplot(fr[[i]]) + 
    geom_point(aes(x=x, y=y), size = 0.5, data=fortify(month_test[[i]])) +
    labs(title = paste("Number of years =", ts_lengths[i]), x = "Year", y = "Log(Metric Tons)") +
    theme_minimal()
}

ggarrange(plotlist = plots, ncol = 2, nrow = 3)

# Calculate accuracy of forecasts by comparing predictions and observations
n_years_test = list()
for (i in 1:length(month_train)) n_years_test[[i]] = nrow(month_test[[i]])


### Write up ###

## Question outline ##
## Methods ##
## Results ##






