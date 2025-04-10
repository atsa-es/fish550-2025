# load packages
library(atsalibrary)
library(ggplot2)
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

## SANDBOX ANALYSIS