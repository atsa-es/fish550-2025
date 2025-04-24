# Create data files
# First install devtools if you don't have it
install.packages("devtools")
library(devtools)
library(atsalibrary)

# Install from GitHub (replace with actual repo)
install_github("nwfsc-cb/rCAX")

library(rCAX)
install.packages("tidyverse")
library(tidyr)
library(dplyr)

columbia.river <- NULL
for(i in c(17, 20, 15, 11, 2)){
esuname <- rCAX:::caxesu[i]
a <- rcax_hli("NOSA", type="colnames")
tab <- rcax_hli("NOSA", flist = list(esu_dps = esuname))
# find the pops with no data and remove
tab <- tab %>% 
  subset((datastatus == "Final" | datastatus == "Reviewed") & bestvalue=="Yes")
if(i == 17 | i == 20) tab$value <- tab$tsaij else tab$value <- tab$tsaej
aa <- tab %>% 
  group_by(esapopname, run) %>% 
  summarize(n = sum(value!= "" & value!="0" & majorpopgroup != ""))
bad <- aa[which(aa$n==0),]
aa <- tab %>% 
  group_by(esapopname, run) %>% 
  summarize(n = any(duplicated(spawningyear)))
df <- tab %>% 
  subset(!(esapopname %in% bad$esapopname & run %in% bad$run)) %>%
  mutate(value = as.numeric(value))

# get the min and max years in data
years <- min(df$spawningyear[!is.na(df$value)]):max(df$spawningyear[!is.na(df$value)])
# fill out the missing years with NAs
df <- df %>%
  select(species, esu_dps, majorpopgroup, esapopname, commonpopname, spawningyear, value, run) %>% 
  group_by(species, esu_dps, majorpopgroup, esapopname, commonpopname, run) %>% 
  complete(spawningyear=years, fill=list(value=NA))

# Deal with pops with multiple data
if(any(aa$n)){
  cat(aa$esapopname[aa$n], "has duplicated years\n")
  df <- df %>% ungroup() %>%
    group_by(species, esu_dps, majorpopgroup, esapopname, run, spawningyear) %>%
    summarize(value = mean(value, na.rm = TRUE,
              commonpopname = commonpopname[1]))
}
if(i == 17 | i == 20) df$value_type <- "tsaij" else df$value_type <- "tsaej"
columbia.river <- bind_rows(columbia.river, df)
}
columbia.river <- columbia.river %>% subset(species != "") %>% ungroup()
save(columbia.river, file = "/Users/Emma/Desktop/Research_Repos/fish550-2025/Lab-2/Team-2/columbia-river.rda")
dir.create("/Users/Emma/Desktop/Research_Repos/fish550-2025/Lab-2/Team-2/", recursive = TRUE, showWarnings = FALSE)
dir.exists("/Users/Emma/Desktop/Research_Repos/fish550-2025/Lab-2/Team-2/")
unique(lower.columbia.river$esu_dps)
df <- lower.columbia.river %>% subset(species == "Steelhead" & run == "Winter")
ggplot(df, aes(x=spawningyear, y=log(value), color=majorpopgroup)) + 
  geom_point(size=0.2) + 
  theme(strip.text.x = element_text(size = 2)) +
  theme(axis.text.x = element_text(size = 5, angle = 90)) +
  facet_wrap(~esapopname)


unique(esa.salmon$esu_dps)
#Filter for just the "Salmon, Chinook (Snake River spring/summer-run ESU)"
esa.salmon <- esa.salmon %>% filter(esu_dps == "Salmon, Chinook (Snake River spring/summer-run ESU)")
unique(esa.salmon$majorpopgroup)
#Remove rows that have a "NaN" in the value column
esa.salmon <- esa.salmon %>% filter(value != "NaN")


#Setupa. matrix
# Create a matrix for each population, filling in missing years as NA
data_matrix <- esa.salmon %>%
  spread(key = spawningyear, value = value) %>%
  select(-esapopname) %>%
  as.matrix()

# Fill any missing values in the matrix (if any)
data_matrix[is.na(data_matrix)] <- NA

#Make all data in the matrix numeric
data_matrix <- apply(data_matrix, 2, function(x) as.numeric(as.character(x)))

#Fit the MARSS Model
fit <- MARSS(data_matrix,
             model = list(
               R = "diagonal and equal",
               A = "scaling"
             ))
state_estimates <- fit$states
autoplot(fit, plot.type = "fitted.ytT")     # Observed vs fitted
autoplot(fit, plot.type = "residuals") # Check model fit


