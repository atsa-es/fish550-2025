# load packages
library(atsalibrary)
library(ggplot2)
library(patchwork)
library(tidyverse)

## STUFF FROM .rmd
# create date column (first of each month)
chinook.month <- chinook.month %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d"))

# plot with facets by state
ggplot(chinook.month, aes(x = Date, y = log.metric.tons)) +
  geom_line() +
  facet_wrap(~ State, ncol = 1) +  # 3 panels vertically
  labs(title = "Monthly Chinook Salmon Landings",
       x = "Date",
       y = "Log(Metric Tons)") +
  theme_minimal()

# monthly data summed across states
chinook.month.wc <- chinook.month %>%
  group_by(Year, Month) %>%
  summarise(
    metric.tons = sum(metric.tons, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d"),
    log.metric.tons = log(metric.tons)
  ) %>%
  arrange(Date)

# annual data
chinook.year <- chinook.year

chinook.year.filtered <- chinook.year %>%
  filter(!State %in% c("Michigan", "Pennsylvania"))

# plot data by state
ggplot(chinook.year, aes(x = Year, y = log.metric.tons)) +
  geom_line() +
  facet_wrap(~ State, ncol = 2, scales = "free_y") +
  labs(
    title = "Chinook Landings by State",
    x = "Year",
    y = "Log(Metric Tons)"
  ) +
  theme_minimal()

ggplot(chinook.year.filtered, aes(x = Year, y = log.metric.tons)) +
  geom_line() +
  facet_wrap(~ State, ncol = 2, scales = "free_y") +
  labs(
    title = "Chinook Landings by State",
    x = "Year",
    y = "Log(Metric Tons)"
  ) +
  theme_minimal()

## SANDBOX Price analysis
# check out data
head(chinook.month)

# back out price (US$/ton)
chinook.month$price <- chinook.month$value.usd/chinook.month$metric.tons
head(chinook.month)

# plot with price by state
landings.monthly <- ggplot(chinook.month, aes(x = Date, y = log.metric.tons)) +
  geom_line() +
  facet_wrap(~ State, nrow = 1) +  # 3 panels vertically
  labs(title = "Monthly Chinook Salmon Landings",
       x = "Date",
       y = "Log(m. tons)") +
  theme_minimal()
landings.monthly

price.monthly <- ggplot(chinook.month, aes(x = Date, y = price)) +
  geom_line() +
  facet_wrap(~ State, nrow = 1) +  # 3 panels horizontal
  labs(title = "Monthly Chinook Salmon Landings",
       x = "Date",
       y = "US$/Metric Ton") +
  theme_minimal()
price.monthly
  # clear periodicity in price
  # WA Jan 2016 = infinity, kinda silly

chinook.month[sapply(chinook.month, is.infinite)] <- NA
chinook.month <- chinook.month %>% mutate_all(~ifelse(is.nan(.), NA, .))
price.monthly <- ggplot(chinook.month, aes(x = Date, y = price)) +
  geom_line() +
  facet_wrap(~ State, nrow = 1) +  # 3 panels horizontal
  labs(title = "Monthly Chinook Salmon Price",
       x = "Date",
       y = "US$/m. ton") +
  theme_minimal()
price.monthly
landings.monthly

landings.monthly/price.monthly

# plan:   forecast value
#         forecast price and landings
#             (interact price and landings)
#         is forecasting price*landings more efficient than forecasting value?

