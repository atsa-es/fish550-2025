# load packages
library(atsalibrary)
# library(ggfortify)
library(ggplot2)
library(patchwork)
library(tidyverse)
library(zoo)

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

value.monthly <- ggplot(chinook.month, aes(x = Date, y = value.usd)) +
  geom_line() +
  facet_wrap(~ State, nrow = 1) +  # 3 panels horizontal
  labs(title = "Monthly Chinook Salmon Value",
       x = "Date",
       y = "US$") +
  theme_minimal()
price.monthly
landings.monthly
value.monthly

landings.monthly/price.monthly/value.monthly

# plan:   forecast value
#         forecast price and landings
#             (interact price and landings)
#         is forecasting price*landings more efficient than forecasting value?

# set time series
CAchinook.month <- subset(chinook.month, State == "CA")
ORchinook.month <- subset(chinook.month, State == "OR")
WAchinook.month <- subset(chinook.month, State == "WA")

#CA
CAlands.ts <- ts(CAchinook.month$log.metric.tons, start=c(1990,1),
                 frequency = 12)
CAlands.train <- window(CAlands.ts, c(1990, 1), c(2020, 12))
CAlands.test <- window(CAlands.ts, c(2021, 1), c(2022, 12))

CAprice.ts <- ts(CAchinook.month$price, start=c(1990,1),
                 frequency = 12)
CAprice.train <- window(CAprice.ts, c(1990, 1), c(2020, 12))
CAprice.test <- window(CAprice.ts, c(2021, 1), c(2022, 12))

CAvalue.ts <- ts(CAchinook.month$value.usd, start=c(1990,1),
                 frequency = 12)
CAvalue.train <- window(CAvalue.ts, c(1990, 1), c(2020, 12))
CAvalue.test <- window(CAvalue.ts, c(2021, 1), c(2022, 12))

#OR
ORlands.ts <- ts(ORchinook.month$log.metric.tons, start=c(1990,1),
                 frequency = 12)
ORlands.train <- window(ORlands.ts, c(1990, 1), c(2020, 12))
ORlands.test <- window(ORlands.ts, c(2021, 1), c(2022, 12))

ORprice.ts <- ts(ORchinook.month$price, start=c(1990,1),
                 frequency = 12)
ORprice.train <- window(ORprice.ts, c(1990, 1), c(2020, 12))
ORprice.test <- window(ORprice.ts, c(2021, 1), c(2022, 12))

ORvalue.ts <- ts(ORchinook.month$value.usd, start=c(1990,1),
                 frequency = 12)
ORvalue.train <- window(ORvalue.ts, c(1990, 1), c(2020, 12))
ORvalue.test <- window(ORvalue.ts, c(2021, 1), c(2022, 12))

#WA
WAlands.ts <- ts(WAchinook.month$log.metric.tons, start=c(1990,1),
                 frequency = 12)
WAlands.train <- window(WAlands.ts, c(1990, 1), c(2020, 12))
WAlands.test <- window(WAlands.ts, c(2021, 1), c(2022, 12))

WAprice.ts <- ts(WAchinook.month$price, start=c(1990,1),
                 frequency = 12)
WAprice.train <- window(WAprice.ts, c(1990, 1), c(2020, 12))
WAprice.test <- window(WAprice.ts, c(2021, 1), c(2022, 12))

WAvalue.ts <- ts(WAchinook.month$value.usd, start=c(1990,1),
                 frequency = 12)
WAvalue.train <- window(WAvalue.ts, c(1990, 1), c(2020, 12))
WAvalue.test <- window(WAvalue.ts, c(2021, 1), c(2022, 12))

# set up forecasts
#CA
CAlands.fit <- forecast::auto.arima(CAlands.train)
CAprice.fit <- forecast::auto.arima(CAprice.train)
CAvalue.fit <- forecast::auto.arima(CAvalue.train)

CAlands.fit
CAprice.fit
CAvalue.fit

#OR
ORlands.fit <- forecast::auto.arima(ORlands.train)
ORprice.fit <- forecast::auto.arima(ORprice.train)
ORvalue.fit <- forecast::auto.arima(ORvalue.train)

ORlands.fit
ORprice.fit
ORvalue.fit

#WA
WAlands.fit <- forecast::auto.arima(WAlands.train)
WAprice.fit <- forecast::auto.arima(WAprice.train)
WAvalue.fit <- forecast::auto.arima(WAvalue.train)

WAlands.fit
WAprice.fit
WAvalue.fit

# project forecasts
#CA
CAlands.fc <- forecast::forecast(CAlands.fit, h=24)
plot(CAlands.fc)
points(CAlands.test)

CAprice.fc <- forecast::forecast(CAprice.fit, h=24)
plot(CAprice.fc)
points(CAprice.test)

CAvalue.fc <- forecast::forecast(CAvalue.fit, h=24)
plot(CAvalue.fc)
points(CAvalue.test)

#OR
ORlands.fc <- forecast::forecast(ORlands.fit, h=24)
plot(ORlands.fc)
points(ORlands.test)

ORprice.fc <- forecast::forecast(ORprice.fit, h=24)
plot(ORprice.fc)
points(ORprice.test)

ORvalue.fc <- forecast::forecast(ORvalue.fit, h=24)
plot(ORvalue.fc)
points(ORvalue.test)

#WA
WAlands.fc <- forecast::forecast(WAlands.fit, h=24)
plot(WAlands.fc)
points(WAlands.test)

WAprice.fc <- forecast::forecast(WAprice.fit, h=24)
plot(WAprice.fc)
points(WAprice.test)

WAvalue.fc <- forecast::forecast(WAvalue.fit, h=24)
plot(WAvalue.fc)
points(WAvalue.test)

#comparing forecasts
#CA
CAlands.compoundfc <- exp(CAlands.fc$mean)
CAvalue.compoundfc <- CAlands.compoundfc*CAprice.fc$mean
CAvalue.compoundfc
CAvalue.fc$mean
CAvalue.test

#OR
ORlands.compoundfc <- exp(ORlands.fc$mean)
ORvalue.compoundfc <- ORlands.compoundfc*ORprice.fc$mean
ORvalue.compoundfc
ORvalue.fc$mean
ORvalue.test

#WA
WAlands.compoundfc <- exp(WAlands.fc$mean)
WAvalue.compoundfc <- WAlands.compoundfc*WAprice.fc$mean
WAvalue.compoundfc
WAvalue.fc$mean
WAvalue.test

# plotting
#CA
plot(CAvalue.compoundfc)
points(CAvalue.test)

plot(CAvalue.fc$mean)
points(CAvalue.test)

#OR
plot(ORvalue.compoundfc)
points(ORvalue.test)

plot(ORvalue.fc$mean)
points(ORvalue.test)

#WA
plot(WAvalue.compoundfc)
points(WAvalue.test)

plot(WAvalue.fc$mean)
points(WAvalue.test)

## NEW SNADBOX (pt. 2)
# plotting this bs with ggplot
class(WAvalue.fc$mean)
df <- data.frame(date=as.Date(as.yearmon(time(WAvalue.fc$mean))), Y=as.matrix(WAvalue.fc$mean))
test <- data.frame(date=as.Date(as.yearmon(time(WAvalue.test))), Y=as.matrix(WAvalue.test))
ggplot(data=df, aes(x=date, y=Y)) + 
  geom_line() +
  geom_point(data=test, aes(x=date, y=Y)) +
  theme_classic()

# making interacted forecasts with CI
time = as.Date(as.yearmon(time(WAlands.fc$mean)))
landed <- c(exp(WAlands.fc$mean))
landed.lb80 <- c(WAlands.fc$lower[,1])
landed.lb95 <- c(WAlands.fc$lower[,2])
landed.ub80 <- c(WAlands.fc$upper[,1])
landed.ub95 <- c(WAlands.fc$upper[,2])
prices <- c(WAprice.fc$mean)
prices.lb80 <- c(WAprice.fc$lower[,1])
prices.lb95 <- c(WAprice.fc$lower[,2])
prices.ub80 <- c(WAprice.fc$upper[,1])
prices.ub95 <- c(WAprice.fc$upper[,2])

df <- data.frame(time, 
                 landed, landed.lb80, landed.lb95, landed.ub80, landed.ub95,
                 prices, prices.lb80, prices.lb95, prices.ub80, prices.ub95)

df$landedCI80 <- df$landed.ub80 - df$landed.lb80
df$landedCI95 <- df$landed.ub95 - df$landed.lb95
df$pricesCI80 <- df$prices.ub80 - df$prices.lb80
df$pricesCI95 <- df$prices.ub95 - df$prices.lb95

df$landedSE80 <- df$landedCI80/2/1.282
df$landedSE95 <- df$landedCI95/2/1.96
df$pricesSE80 <- df$pricesCI80/2/1.282
df$pricesSE95 <- df$pricesCI95/2/1.96

df$compound <- df$prices*exp(df$landed)
df$Vcompound <- (exp(df$landed)^2*df$pricesSE95^2) + 
                (df$prices^2*df$landedSE95^2) +
                (2*df$prices*df$landed*cov(df$prices, df$landed)) +
                (df$landedSE95^2*df$pricesSE95^2) +
                cov(df$prices, df$landed)^2
df$SEcompound <- sqrt(df$Vcompound)
df$compound.lb95 <- df$compound - 1.96*df$SEcompound
df$compound.ub95 <- df$compound + 1.96*df$SEcompound
df$compound.lb80 <- df$compound - 1.282*df$SEcompound
df$compound.ub80 <- df$compound + 1.282*df$SEcompound

ggplot(data = df, aes(x=time, y=compound)) +
  geom_ribbon(aes(ymin = compound.lb95, ymax = compound.ub95), fill = "skyblue1", alpha = 0.5) +
  geom_line() +
  theme_classic() 
