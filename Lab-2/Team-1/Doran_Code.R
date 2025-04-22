## Packages
library(MARSS)
library(ggplot2)
library(dplyr)
library(forecast)
library(tidyverse)
library(zoo)
library(corrplot)

### Data exploration
esu <- unique(esa.salmon$esu_dps)
esunum <- which(esu == esu[7])
esuname <- esu[esunum]

#Data visualization
plotesu <- function(esuname){
  df <- esa.salmon %>% subset(esu_dps %in% esuname)
  ggplot(df, aes(x=spawningyear, y=log(value), color=majorpopgroup)) + 
    geom_point(size=0.2, na.rm = TRUE) + 
    theme(strip.text.x = element_text(size = 3)) +
    theme(axis.text.x = element_text(size = 5, angle = 90)) +
    facet_wrap(~esapopname) +
    ggtitle(paste0(esuname, collapse="\n"))
}

#Plot of our ESU sub-pops
plotesu(esu[7])

## Create data frames for our ESU
esa_filtered <- esa.salmon %>%
  filter(esu_dps == esuname, esapopname != "") %>%
  mutate(log.spawner = log(value)) %>%
  mutate(log.spawner = replace(log.spawner, is.infinite(log.spawner), NA))


#Lookup data frame for major pop groups
pop.groups <- esa_filtered %>%
  select(esapopname, majorpopgroup) %>%
  distinct()%>%
  as.matrix()

# Time series matrix for MARSS
coho<- esa_filtered %>%
  select(esapopname, spawningyear, log.spawner) %>%
  pivot_wider(
    names_from = "esapopname",
    values_from = "log.spawner",
    values_fn = mean
  ) %>%
  column_to_rownames(var = "spawningyear") %>%
  as.matrix() %>%
  t()
coho[is.na(coho)] <- NA
head(coho)

#clean up row names- our 26 sub-pops
tmp <- rownames(coho)
tmp <- stringr::str_replace(tmp, "Steelhead [(]Upper Columbia River DPS[)]", "")
tmp <- stringr::str_replace(tmp, "River - summer", "")
tmp <- stringr::str_trim(tmp)
rownames(coho) <- tmp
rownames(pop.groups)<-tmp
#subset colnames in case we need them later
cohocolnames<-colnames(coho)

#### Question 1####
#Part 1: Filling in NA values
#use marss, save to object and get y-hat
# `R="diagonal and equal"` and `A="scaling"
mod.list<-list(
  R="diagonal and equal",
  A="scaling"
)
obj<-MARSS(coho, mod.list)
cohofit<-obj$states
rownames(cohofit)<-tmp
colnames(cohofit)<-cohocolnames
head(cohofit)
cohofit-coho
## Part 2: Estimates of decrease from historical abundance
#random walk model, Z is a diagonal matrix
cohoU<-coef(obj, type="matrix")$U
hist(cohoU, main="Coho Population Trends", xlab="U coefficient")
colnames(cohoU)<-c("U")

#Which populations are declining?
decliningpop<-subset(cohoU, cohoU<0)
#Salmon, coho (Oregon Coast ESU) Floras Creek/New River - fall
#And Sixes River- fall are the only groups that are declining

meantrend<-mean(cohoU)
print(meantrend)
#So on average, this ESU group is experiencing slight increases from historical
#abundances (average U=0.0359 when fitting biased random walk models to ts data

#### Question 2####
#Fit MARSS models with different population groupings assumptions and compare AIC
#Our data set assumes five groups (see pop.groups look-up matrix)
#Compare to models that assume:
#each group is its own subpop
#one big subpop
#geographical?

#### Question 3####
