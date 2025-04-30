## Packages
library(atsalibrary)
library(dplyr)
library(ggplot2)
library(zoo)

#Filter and plot data
data(chinook)
chinook.year<-chinook.year %>%
  filter(!State %in% c("Michigan", "Pennsylvania"))
ggplot(chinook.year, aes(x = Year, y = log.metric.tons, color = State)) +
  geom_line() +
  labs(
    title = "Chinook Landings by State",
    x = "Year",
    y = "Log(Metric Tons)"
  ) +
  theme_minimal()

#### Regional forecasting accuracy for the Pacific Northwest ####
## Methods: Fit separate ARIMA models to the California, Oregon, and Washington landings data (volume)
# Will also fit an ARIMA model for the entire region
## Will test forecasting accuracy of fit models for different states
# E.g. Forecasting Oregon with Washington model, vice versa
# E.g. Forecasting WA data with regional model
# Will also check to see if this model is suitable for forecasting Alaska catches

## Subset test and training data for each region
# Washington
dat <- chinook.year %>%
  filter(State == "Washington") %>%
  mutate(lnreturns = log.metric.tons,
         year = Year) %>%
  select(year, lnreturns)
wats <- ts(dat$lnreturns, start=dat$year[1])
ts.plot(wats, main=" Washington Chinook Landings", ylab="Log Metric Tons", xlab="Year")
watrain <- window(wats, dat$year[1], dat$year[1]+55)
watest <- window(wats, dat$year[1]+56, dat$year[1]+66)
testlength<-length(watest)
# Oregon
dat <- chinook.year %>%
  filter(State == "Oregon") %>%
  mutate(lnreturns = log.metric.tons,
         year = Year) %>%
  select(year, lnreturns)
orts <- ts(dat$lnreturns, start=dat$year[1])
ts.plot(orts, main="Oregon Chinook Landings", ylab="Log Metric Tons", xlab="Year")
ortrain <- window(orts, dat$year[1], dat$year[1]+56)
ortest <- window(orts, dat$year[1]+56, dat$year[1]+66)
# California
dat <- chinook.year %>%
  filter(State == "California") %>%
  mutate(lnreturns = log.metric.tons,
         year = Year) %>%
  select(year, lnreturns)
cats <- ts(dat$lnreturns, start=dat$year[1])
ts.plot(cats, main="California Chinook Landings", ylab="Log Metric Tons", xlab="Year")
catrain <- window(cats, dat$year[1], dat$year[1]+55)
catest <- window(cats, dat$year[1]+56, dat$year[1]+66)
# PNW Region
chinook.pnw.total <- chinook.year %>%
  filter(!State == "Alaska")%>%
  mutate(metric.tons = exp(log.metric.tons)) %>%  # undo the log
  group_by(Year) %>%
  summarise(
    total.metric.tons = sum(metric.tons, na.rm = TRUE),
    log.total.metric.tons = log(total.metric.tons)
  )
dat <- chinook.pnw.total %>%
  mutate(lnreturns = log.total.metric.tons,
         year = Year) %>%
  select(year, lnreturns)
pnwts <- ts(dat$lnreturns, start=dat$year[1])
ts.plot(pnwts, main="PNW Chinook Landings", ylab="Log Metric Tons", xlab="Year")
pnwtrain <- window(pnwts, dat$year[1], dat$year[1]+55)
pnwtest <- window(pnwts, dat$year[1]+56, dat$year[1]+66)
# Alaska
dat <- chinook.year %>%
  filter(State=="Alaska") %>%
  mutate(lnreturns = log.metric.tons,
         year = Year) %>%
  select(year, lnreturns)
akts <- ts(dat$lnreturns, start=dat$year[1])
ts.plot(akts, main="Alaska Chinook Landings", ylab="Log Metric Tons", xlab="Year")
aktrain <- window(akts, dat$year[1], dat$year[1]+55)
aktest <- window(akts, dat$year[1]+56, dat$year[1]+66)

#Differencing and stationarity 
### All chinook data
chinook.total <- chinook.year %>%
  filter(State%in%c("California", "Alaska", "Oregon", "Washington")) %>%
  mutate(metric.tons = exp(log.metric.tons)) %>%  # undo the log
  group_by(Year) %>%
  summarise(
    total.metric.tons = sum(metric.tons, na.rm = TRUE),
    log.total.metric.tons = log(total.metric.tons)
  )
dat <- chinook.total %>%
  mutate(lnreturns = log.total.metric.tons,
         year = Year) %>%
  select(year, lnreturns)
totalts<-ts(dat$lnreturns, start=dat$year[1])
totaltrain <- window(totalts, dat$year[1], dat$year[1]+55)
totaltest <- window(totalts, dat$year[1]+56, dat$year[1]+66)

## Differencing and stationarity
ndiffs(aktrain)
ndiffs(catrain)
ndiffs(ortrain)
ndiffs(watrain)
ndiffs(totaltrain)
ndiffs(pnwtrain)
## ACF plots
par(mfrow=c(2,2))
acf(catrain, main= "California ACF")
acf(aktrain, main="Alaska ACF")
acf(watrain, main="Washington ACF")
acf(ortrain,main="Oregon ACF")

#PACF
pacf(catrain, main= "California ACF")
pacf(aktrain, main="Alaska ACF")
pacf(watrain, main="Washington ACF")
pacf(ortrain,main="Oregon ACF")
### Fitting ARIMA and Holt Models for each location
# Alaska
auto.arima(aktrain) # returned (1,0 0)
akts.arima.0<-arima(aktrain, order=c(0,0,0))
akts.arima.1<-arima(aktrain, order=c(0,1,0))
akts.arima.2<-arima(aktrain, order=c(1,0,0)) #auto-arima model
akts.arima.3<-arima(aktrain, order=c(1,1,0))
akts.arima.4<-arima(aktrain, order=c(1,1,1))
akts.holt<-ets(aktrain, model="AAN")

# California
auto.arima(catrain) # returned (0,1,0)
cats.arima.0<-arima(catrain, order=c(0,0,0))
cats.arima.1<-arima(catrain, order=c(0,1,0))#auto-arima model
cats.arima.2<-arima(catrain, order=c(1,0,0))
cats.arima.3<-arima(catrain, order=c(1,1,0))
cats.arima.4<-arima(catrain, order=c(1,1,1))
cats.holt<-ets(catrain, model="AAN")

# Oregon 
auto.arima(ortrain) # returned (0,1,0)
orts.arima.0<-arima(ortrain, order=c(0,0,0))
orts.arima.1<-arima(ortrain, order=c(0,1,0))#auto-arima model
orts.arima.2<-arima(ortrain, order=c(1,0,0)) 
orts.arima.3<-arima(ortrain, order=c(1,1,0))
orts.arima.4<-arima(ortrain, order=c(1,1,1))
orts.holt<-ets(ortrain, model="AAN")

# Washington
auto.arima(watrain) # returned (0,1,0)
wats.arima.0<-arima(watrain, order=c(0,0,0))
wats.arima.1<-arima(watrain, order=c(0,1,0))#auto-arima model
wats.arima.2<-arima(watrain, order=c(1,0,0)) 
wats.arima.3<-arima(watrain, order=c(1,1,0))
wats.arima.4<-arima(watrain, order=c(1,1,1))
wats.holt<-ets(watrain, model="AAN")

# PNW
auto.arima(pnwtrain) # returned (0,1,0)
pnwts.arima.0<-arima(pnwtrain, order=c(0,0,0))
pnwts.arima.1<-arima(pnwtrain, order=c(0,1,0)) #auto-arima model
pnwts.arima.2<-arima(pnwtrain, order=c(1,0,0)) 
pnwts.arima.3<-arima(pnwtrain, order=c(1,1,0))
pnwts.arima.4<-arima(pnwtrain, order=c(1,1,1))
pnwts.holt<-ets(pnwtrain, model="AAN")

# Total
# Total
auto.arima(totaltrain)
totalts.arima.0<-arima(totaltrain, order=c(0,0,0))
totalts.arima.1<-arima(totaltrain, order=c(0,1,0)) #auto-arima model
totalts.arima.2<-arima(totaltrain, order=c(1,0,0)) 
totalts.arima.3<-arima(totaltrain, order=c(1,1,0))
totalts.arima.4<-arima(totaltrain, order=c(1,1,1))
totalts.holt<-ets(totaltrain, model="AAN")

## Forecasting for each model at each location
# Alaska
ak.forecast.0<-forecast(akts.arima.0, h=testlength)
ak.forecast.1<-forecast(akts.arima.1, h=testlength)
ak.forecast.2<-forecast(akts.arima.2, h=testlength)
ak.forecast.3<-forecast(akts.arima.3, h=testlength)
ak.forecast.4<-forecast(akts.arima.4, h=testlength)
ak.forecast.5<-forecast(akts.holt, h=testlength) #Holt model

# California
ca.forecast.0<-forecast(cats.arima.0, h=testlength)
ca.forecast.1<-forecast(cats.arima.1, h=testlength)
ca.forecast.2<-forecast(cats.arima.2, h=testlength)
ca.forecast.3<-forecast(cats.arima.3, h=testlength)
ca.forecast.4<-forecast(cats.arima.4, h=testlength)
ca.forecast.5<-forecast(cats.holt, h=testlength)

# Oregon
or.forecast.0<-forecast(orts.arima.0, h=testlength)
or.forecast.1<-forecast(orts.arima.1, h=testlength)
or.forecast.2<-forecast(orts.arima.2, h=testlength)
or.forecast.3<-forecast(orts.arima.3, h=testlength)
or.forecast.4<-forecast(orts.arima.4, h=testlength)
or.forecast.5<-forecast(orts.holt, h=testlength)

#Washington
wa.forecast.0<-forecast(wats.arima.0, h=testlength)
wa.forecast.1<-forecast(wats.arima.1, h=testlength)
wa.forecast.2<-forecast(wats.arima.2, h=testlength)
wa.forecast.3<-forecast(wats.arima.3, h=testlength)
wa.forecast.4<-forecast(wats.arima.4, h=testlength)
wa.forecast.5<-forecast(wats.holt, h=testlength)

# PNW
pnw.forecast.0<-forecast(pnwts.arima.0, h=testlength)
pnw.forecast.1<-forecast(pnwts.arima.1, h=testlength)
pnw.forecast.2<-forecast(pnwts.arima.2, h=testlength)
pnw.forecast.3<-forecast(pnwts.arima.3, h=testlength)
pnw.forecast.4<-forecast(pnwts.arima.4, h=testlength)
pnw.forecast.5<-forecast(pnwts.holt, h=testlength)

# Total
total.forecast.0<-forecast(totalts.arima.0, h=testlength)
total.forecast.1<-forecast(totalts.arima.1, h=testlength)
total.forecast.2<-forecast(totalts.arima.2, h=testlength)
total.forecast.3<-forecast(totalts.arima.3, h=testlength)
total.forecast.4<-forecast(totalts.arima.4, h=testlength)
total.forecast.5<-forecast(totalts.holt, h=testlength)

### Validating forecasts
# Alaska
aktest.0<-accuracy(ak.forecast.0, aktest)
aktest.1<-accuracy(ak.forecast.1, aktest)
aktest.2<-accuracy(ak.forecast.2, aktest)
aktest.3<-accuracy(ak.forecast.3, aktest)
aktest.4<-accuracy(ak.forecast.4, aktest)
aktest.5<-accuracy(ak.forecast.5, aktest)
ak.forecast.summary<-matrix(data=c(aktest.0[2, "RMSE"],aktest.1[2,"RMSE"], aktest.2[2,"RMSE"],
                                   aktest.3[2,"RMSE"], aktest.4[2, "RMSE"],
                                   aktest.5[2, "RMSE"]), nrow=1, ncol=6)
colnames(ak.forecast.summary)<-c("0,0,0", "0,1,0", "1,0,0", "1,1,0", "1,1,1", "Holt")
rownames(ak.forecast.summary)<-c("RMSE")
print(ak.forecast.summary) #The Holt model had lowest RMSE


# California
catest.0<-accuracy(ca.forecast.0, catest)
catest.1<-accuracy(ca.forecast.1, catest)
catest.2<-accuracy(ca.forecast.2, catest)
catest.3<-accuracy(ca.forecast.3, catest)
catest.4<-accuracy(ca.forecast.4, catest)
catest.5<-accuracy(ca.forecast.5, catest)
ca.forecast.summary<-matrix(data=c(catest.0[2,"RMSE"],catest.1[2,"RMSE"], catest.2[2,"RMSE"],
                                   catest.3[2,"RMSE"], catest.4[2, "RMSE"],
                                   catest.5[2, "RMSE"]), nrow=1, ncol=6)
colnames(ca.forecast.summary)<-c("0,0,0","0,1,0", "1,0,0", "1,1,0", "1,1,1", "Holt")
rownames(ca.forecast.summary)<-c("RMSE")
print(ca.forecast.summary) #The Holt had the most accurate forecast

# Oregon
ortest.0<-accuracy(or.forecast.0, ortest)
ortest.1<-accuracy(or.forecast.1, ortest)
ortest.2<-accuracy(or.forecast.2, ortest)
ortest.3<-accuracy(or.forecast.3, ortest)
ortest.4<-accuracy(or.forecast.4, ortest)
ortest.5<-accuracy(or.forecast.5, ortest)

or.forecast.summary<-matrix(data=c(ortest.0[2,"RMSE"], ortest.1[2,"RMSE"], ortest.2[2,"RMSE"],
                                   ortest.3[2,"RMSE"], ortest.4[2, "RMSE"],
                                   ortest.5[2, "RMSE"]), nrow=1, ncol=6)
colnames(or.forecast.summary)<-c("0,0,0","0,1,0", "1,0,0", "1,1,0", "1,1,1", "Holt")
rownames(or.forecast.summary)<-c("RMSE")
print(or.forecast.summary) #The ARIMA(1,0,0) model had the best forecast

# Washington
watest.0<-accuracy(wa.forecast.0, watest)
watest.1<-accuracy(wa.forecast.1, watest)
watest.2<-accuracy(wa.forecast.2, watest)
watest.3<-accuracy(wa.forecast.3, watest)
watest.4<-accuracy(wa.forecast.4, watest)
watest.5<-accuracy(wa.forecast.5, watest)
wa.forecast.summary<-matrix(data=c(watest.0[2,"RMSE"], watest.1[2,"RMSE"], watest.2[2,"RMSE"],
                                   watest.3[2,"RMSE"], watest.4[2, "RMSE"],
                                   watest.5[2, "RMSE"]), nrow=1, ncol=6)
colnames(wa.forecast.summary)<-c("0,0,0","0,1,0", "1,0,0", "1,1,0", "1,1,1", "Holt")
rownames(wa.forecast.summary)<-c("RMSE")
print(wa.forecast.summary) #The ARIMA(1,0,0) model had the best forecast

# PNW
pnwtest.0<-accuracy(pnw.forecast.0, pnwtest)
pnwtest.1<-accuracy(pnw.forecast.1, pnwtest)
pnwtest.2<-accuracy(pnw.forecast.2, pnwtest)
pnwtest.3<-accuracy(pnw.forecast.3, pnwtest)
pnwtest.4<-accuracy(pnw.forecast.4, pnwtest)
pnwtest.5<-accuracy(pnw.forecast.5, pnwtest)
pnw.forecast.summary<-matrix(data=c(pnwtest.0[2,"RMSE"], pnwtest.1[2,"RMSE"], pnwtest.2[2,"RMSE"],
                                   pnwtest.3[2,"RMSE"], pnwtest.4[2, "RMSE"],
                                   pnwtest.5[2, "RMSE"]), nrow=1, ncol=6)
colnames(pnw.forecast.summary)<-c("0,0,0","0,1,0", "1,0,0", "1,1,0", "1,1,1", "Holt")
rownames(pnw.forecast.summary)<-c("RMSE")
print(pnw.forecast.summary) #The Holt model had lowest RMSE

# Total
totaltest.0<-accuracy(total.forecast.0, totaltest)
totaltest.1<-accuracy(total.forecast.1, totaltest)
totaltest.2<-accuracy(total.forecast.2, totaltest)
totaltest.3<-accuracy(total.forecast.3, totaltest)
totaltest.4<-accuracy(total.forecast.4, totaltest)
totaltest.5<-accuracy(total.forecast.5, totaltest)
total.forecast.summary<-matrix(data=c(totaltest.0[2,"RMSE"], totaltest.1[2,"RMSE"], totaltest.2[2,"RMSE"],
                                   totaltest.3[2,"RMSE"], totaltest.4[2, "RMSE"],
                                   totaltest.5[2, "RMSE"]), nrow=1, ncol=6)
colnames(total.forecast.summary)<-c("0,0,0","0,1,0", "1,0,0", "1,1,0", "1,1,1", "Holt")
rownames(total.forecast.summary)<-c("RMSE")
print(total.forecast.summary) # 110 and 010 performed about equally

# Plotting best forecast from each region
# Alaska
par(mfrow=c(3,2))
plot(ak.forecast.5, main="Alaksa Chinook Landings", 
     ylab="Log Volume Biometric Tons", xlab="Year")
points(aktest, pch=20)
checkresiduals(akts.holt)
# California
plot(ca.forecast.5, main="California Chinook Landings", 
     ylab="Log Volume Biometric Tons", xlab="Year")
points(catest, pch=20)
checkresiduals(cats.holt)
# Oregon
plot(or.forecast.2, main="Oregon Chinook Landings", 
     ylab="Log Volume Biometric Tons", xlab="Year")
points(ortest, pch=20)
checkresiduals(orts.arima.2)
# Washington
plot(wa.forecast.2, main="Washington Chinook Landings", 
     ylab="Log Volume Biometric Tons", xlab="Year")
points(watest, pch=20)
checkresiduals(wats.arima.2)
par(mfrow=c(1,2))
# PNW
plot(pnw.forecast.5, main="PNW Chinook Landings", 
     ylab="Log Volume Biometric Tons", xlab="Year")
points(pnwtest, pch=20)
checkresiduals(pnwts.holt)
#Total
plot(total.forecast.3, main="Total Chinook Landings", ylab="Log Volume Biometric Tons", xlab="Year")
points(totaltest, pch=20)
checkresiduals(totalts.arima.3)

### Comparing forecasting accuracy across model and regions
forecastrmse<-matrix(nrow=6, ncol=6)
rownames(forecastrmse)<-c("Alaska Forecast", "California Forecast", 
                         "Oregon Forecast", "Washington Forecast", "PNW Forecast", "Total Forecast")
colnames(forecastrmse)<-c("Alaska Model", "California Model", 
                         "Oregon Model", "Washington Model", "PNW Model", "Total Model")
forecastrmse[1,1]<-accuracy(ak.forecast.5, aktest)[2, "RMSE"]
forecastrmse[1,2]<-accuracy(ca.forecast.5, aktest)[2, "RMSE"]
forecastrmse[1,3]<-accuracy(or.forecast.2, aktest)[2, "RMSE"]
forecastrmse[1,4]<-accuracy(wa.forecast.2, aktest)[2, "RMSE"]
forecastrmse[1,5]<-accuracy(pnw.forecast.5, aktest)[2, "RMSE"]
forecastrmse[1,6]<-accuracy(total.forecast.3, aktest)[2, "RMSE"]
forecastrmse[2,1]<-accuracy(ak.forecast.5, catest)[2, "RMSE"]
forecastrmse[2,2]<-accuracy(ca.forecast.5, catest)[2, "RMSE"]
forecastrmse[2,3]<-accuracy(or.forecast.2, catest)[2, "RMSE"]
forecastrmse[2,4]<-accuracy(wa.forecast.2, catest)[2, "RMSE"]
forecastrmse[2,5]<-accuracy(pnw.forecast.5, catest)[2, "RMSE"]
forecastrmse[2,6]<-accuracy(total.forecast.3, catest)[2, "RMSE"]
forecastrmse[3,1]<-accuracy(ak.forecast.5, ortest)[2, "RMSE"]
forecastrmse[3,2]<-accuracy(ca.forecast.5, ortest)[2, "RMSE"]
forecastrmse[3,3]<-accuracy(or.forecast.2, ortest)[2, "RMSE"]
forecastrmse[3,4]<-accuracy(wa.forecast.2, ortest)[2, "RMSE"]
forecastrmse[3,5]<-accuracy(pnw.forecast.5,ortest)[2, "RMSE"]
forecastrmse[3,6]<-accuracy(total.forecast.3, ortest)[2, "RMSE"]
forecastrmse[4,1]<-accuracy(ak.forecast.5, watest)[2, "RMSE"]
forecastrmse[4,2]<-accuracy(ca.forecast.5, watest)[2, "RMSE"]
forecastrmse[4,3]<-accuracy(or.forecast.2, watest)[2, "RMSE"]
forecastrmse[4,4]<-accuracy(wa.forecast.2, watest)[2, "RMSE"]
forecastrmse[4,5]<-accuracy(pnw.forecast.5,watest)[2, "RMSE"]
forecastrmse[4,6]<-accuracy(total.forecast.3, watest)[2, "RMSE"]
forecastrmse[5,1]<-accuracy(ak.forecast.5, pnwtest)[2, "RMSE"]
forecastrmse[5,2]<-accuracy(ca.forecast.5, pnwtest)[2, "RMSE"]
forecastrmse[5,3]<-accuracy(or.forecast.2, pnwtest)[2, "RMSE"]
forecastrmse[5,4]<-accuracy(wa.forecast.2, pnwtest)[2, "RMSE"]
forecastrmse[5,5]<-accuracy(pnw.forecast.5,pnwtest)[2, "RMSE"]
forecastrmse[5,6]<-accuracy(total.forecast.3, pnwtest)[2, "RMSE"]
forecastrmse[6,1]<-accuracy(ak.forecast.5, totaltest)[2, "RMSE"]
forecastrmse[6,2]<-accuracy(ca.forecast.5, totaltest)[2, "RMSE"]
forecastrmse[6,3]<-accuracy(or.forecast.2, totaltest)[2, "RMSE"]
forecastrmse[6,4]<-accuracy(wa.forecast.2, totaltest)[2, "RMSE"]
forecastrmse[6,5]<-accuracy(pnw.forecast.5,totaltest)[2, "RMSE"]
forecastrmse[6,6]<-accuracy(total.forecast.3, totaltest)[2, "RMSE"]
print(forecastrmse)




