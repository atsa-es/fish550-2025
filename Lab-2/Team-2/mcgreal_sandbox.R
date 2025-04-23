library(atsalibrary)
library(MARSS)
library(panelr)
library(tidyverse)

# pull in data
load("Lab-2/Data_Images/esa-salmon.rda")

# check out the data
esu <- unique(esa.salmon$esu_dps)
head(esa.salmon)
esu

# plotting esu
plotesu <- function(esuname){
  df <- esa.salmon %>% subset(esu_dps %in% esuname)
  ggplot(df, aes(x=spawningyear, y=log(value), color=majorpopgroup)) + 
    geom_point(size=0.2, na.rm = TRUE) + 
    theme(strip.text.x = element_text(size = 3)) +
    theme(axis.text.x = element_text(size = 5, angle = 90)) +
    facet_wrap(~esapopname) +
    ggtitle(paste0(esuname, collapse="\n"))
}
plotesu(esu[2])

# subset esu data
esu2 <- subset(esa.salmon, esu_dps == "Salmon, Chinook (Snake River spring/summer-run ESU)")
esu2$popname <- substr(esu2$esapopname, 53, nchar(esu2$esapopname)) 

# log transform and set data wide
esu2$logvalue <- log(esu2$value)
esu2 <- esu2 %>% mutate_all(~ifelse(is.nan(.), NA, .))

esu2_widen <- esu2[-c(1:6, 8)]
w_esu2 <- panel_data(esu2_widen, id = popname, wave = spawningyear)
w_esu2 <- widen_panel(w_esu2, separator = "_")
