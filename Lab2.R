
here::here()
library(tidyverse)
library(dplyr)
options(dplyr.summarise.inform = FALSE)

load("esa-salmon.rda")

unique(columbia.river$species)

df <- columbia.river %>% subset(species == "Steelhead")

ggplot(df, aes(x=spawningyear, y=log(value), color=run)) + 
  geom_point(size=0.2, na.rm = TRUE) +
  theme(strip.text.x = element_text(size = 3)) +
  theme(axis.text.x = element_text(size = 5, angle = 90)) + 
  facet_wrap(~esapopname)


summary(df)

load(here::here("Lab-2", "Data_Images", "esa-salmon.rda"))
library(tidyr)
# # Create estimates of spawner abundance for all missing years 
# and provide estimates of the decline from the historical abundance.
# # 
# # Evaluate support for the major population groups. 
# Are the populations in the groups more correlated than outside the groups?
# #   
# # Evaluate the evidence of correlation of salmon returns with climate indices. 
# We will talk about how to do this on the Tuesday after lab.

library(dplyr)
# Our group was assigned only Steelhead. 
SteelheadAll<- esa.salmon %>%
  filter(species=="Steelhead")


# Plot all Summer Steelhead faceted by ESA group and log transformed value 
library(ggplot2)

Stl_plot<- SteelheadAll %>%
  mutate(log_value=log(value)) %>%
  ggplot(aes(x=spawningyear, y=log_value, color = run)) +
  geom_point() +
  facet_wrap(~esapopname) +
  theme_bw() +
  labs(title="Summer Steelhead by ESA group", x="Year", y="Log Abundance")


print(Stl_plot)



# Step 1: Start with your full dataset
Stl <- SteelheadAll

# Step 2: Filter for Summer if you want
# Stl <- Stl %>% filter(run == "Summer") # optional

# Step 3: Create region and run labels BEFORE pivoting
Stl <- Stl %>%
  mutate(
    region = case_when(
      grepl("Lower", esapopname) ~ "Lower",
      grepl("Middle", esapopname) ~ "Middle",
      grepl("Upper", esapopname) ~ "Upper",
      TRUE ~ "Unknown"
    ),
    run_timing = case_when(
      grepl("summer", esapopname, ignore.case = TRUE) ~ "Summer",
      grepl("winter", esapopname, ignore.case = TRUE) ~ "Winter",
      TRUE ~ "Unknown"
    )
  )

# Step 4: Clean up the names (simplify for pivoting)
Stl$esapopname <- gsub("Steelhead", "", Stl$esapopname)
Stl$esapopname <- gsub("- summer", "", Stl$esapopname)
Stl$esapopname <- gsub(" - winter", "", Stl$esapopname)


# Step 5: Store lookup table for region/run AFTER cleaning names
meta_info <- Stl %>%
  select(esapopname, region, run_timing) %>%
  distinct()

# Step 6: Pivot wider to population x year
Stl_wide <- Stl %>%
  select(spawningyear, esapopname, value) %>%
  pivot_wider(names_from = esapopname, values_from = value)

# Step 7: Save years and transpose
Years <- Stl_wide$spawningyear
Stl_mat <- Stl_wide %>%
  select(-spawningyear) %>%
  as.matrix() %>%
  t()

# Step 8: Assign years as column names and reorder by year
colnames(Stl_mat) <- Years
Stl_mat <- Stl_mat[, order(as.numeric(colnames(Stl_mat)))]
#Make sure all NaN are NA 
Stl_mat[is.nan(Stl_mat)] <- NA

# Step 9: Create vectors for regions and run timings
region_vec <- meta_info$region[match(rownames(Stl_mat), meta_info$esapopname)]
run_vec <- meta_info$run_timing[match(rownames(Stl_mat), meta_info$esapopname)]

# Now you can subset like:
# Stl_mat[region_vec == "Lower", ]
# Stl_mat[run_vec == "Summer", ]

###################################
#Cliamte Covariate Analysis
###################################

load("np_climate.RData")
load("esa-salmon.rda")

years <- which(np_climate_seasonal$year %in% 2000:2024)
spring_pdo <- np_climate_seasonal$gmsst.fall[years]
cmat <- matrix(spring_pdo, nrow=1)
matrix(na.approx(cmat), nrow=1)
cmat

esu <- unique(esa.salmon$esu_dps)
esunum <- which(esu == "Steelhead (Upper Columbia River DPS)")
esuname <- esu[esunum]

dat <- esa.salmon %>% 
  subset(esu_dps == esuname) %>% # get only this ESU
  mutate(log.spawner = log(value)) %>% # create a column called log.spawner
  dplyr::select(esapopname, spawningyear, log.spawner) %>% # get just the columns that I need
  pivot_wider(names_from = "esapopname", values_from = "log.spawner") %>% 
  column_to_rownames(var = "spawningyear") %>% # make the years rownames
  as.matrix() %>% # turn into a matrix with year down the rows
  t() # make time across the columns
# MARSS complains if I don't do this


dat[is.na(dat)] <- NA
# Clean up rownames
tmp <- rownames(dat)
tmp <- stringr::str_replace(tmp, "Steelhead [(]Upper Columbia River DPS[)]", "")
tmp <- stringr::str_replace(tmp, "River - summer", "")
tmp <- stringr::str_trim(tmp)
rownames(dat) <- tmp


library(atsalibrary)
data("np_climate")
years <- which(np_climate_seasonal$year %in% colnames(dat))
spring_pdo <- np_climate_seasonal$pdo.spring[years]
cmat <- matrix(spring_pdo, nrow=1)
# No NAs allowed. Interpolate if needed
any(is.na(cmat))

names(np_climate_seasonal)

seasons <- c("spring","summer", "fall","winter")
clim_var <- c("pna","epo","oni", "censo","whwp") #"wp","meiv2","pacwarm","gmsst","ipotpi","pdo" )

# Create all combinations
combo_df <- expand.grid(season = seasons, variable = clim_var)

# Combine with "." separator
combo_names <- paste( combo_df$variable,combo_df$season, sep = ".")


all_combos <- NULL
for (i in 1:length(seasons)){

    temp <- seasons[i]
    matches <- combo_names[grepl(temp, combo_names)]
    
    season_combos <- list() 
    
    for(m in 1:length(matches)) {
    combos <- combn(matches, m, simplify = FALSE)
    season_combos[[paste0("size_", m)]] <- combos 
    }
    
    all_combos[[temp]] <- season_combos
    
}






years <- which(np_climate_seasonal$year %in% colnames(dat))


all_cmat <- list()
AIC_results <- list()
dropped_fits <- list()  #for failed/unconverged models

# Loop over each combination size group
for (i in 1:length(seasons)) {
  season <- seasons[i]
  season_combos <- all_combos[[i]]
  
  for (j in seq_along(season_combos)) {
    
    combo_group_name <- names(season_combos)[j]     # e.g., "size_1"
    combo_group <- season_combos[[j]]               # list of combinations of that size
    
    combo_matrices <- list()  # Reset for each group
    
    # Loop over each combination within the group
    for (v in seq_along(combo_group)) {
      
      var_names <- combo_group[[v]]  # vector of variable names in this combo
      print(paste("Processing:", paste(var_names, collapse = "+")))  # Debug info
      
      # Extract climate data rows and build matrix
      var_matrix <- do.call(rbind, lapply(var_names, function(var) {
        covs <- np_climate_seasonal[[var]][years]
        na.approx(covs, rule=2)
      }))
      
      
      
      # Set row names for clarity
      rownames(var_matrix) <- var_names
      
      # Create a name for the combination (e.g., "oni.spring+pdo.spring")
      combo_name <- paste(var_names, collapse = "+")
      
      # Store in list
      combo_matrices[[combo_name]] <- var_matrix
      
      
      #MARSS Part
      mod.list <- list(
        U = "unequal",
        R = "diagonal and equal",
        Q = "diagonal and unequal",
        c =  var_matrix,
        C = "unequal"
      )
      
      fit <- MARSS(dat, model=mod.list)
      
      if (fit$convergence == 0) {
        AIC_results[[combo_name]] <- AIC(fit)
      }
      else{
        dropped_fits[[combo_name]] <- fit
        combo_matrices[[combo_name]] <- NULL
      }
      
      #Cool troubleshooting trick!! Remember for chapter. 
      # if (inherits(fit, "try-error")) {
      #   AIC_results[[combo_name]] <- NA  # mark failure
      # } else {
      #   AIC_results[[combo_name]] <- AIC(fit)
      # }
      
    }
    
    #Crashes otherwise
    if (is.null(all_cmat[[season]])) {
      all_cmat[[season]] <- list()
    }
    
    
    all_cmat[[season]][[combo_group_name]] <- combo_matrices
  }
}

#save(AIC_results,file="AIC_Lab2.Rdata")
    
AIC_results

aic_rank <- data.frame(
  combo = names(AIC_results),
  AIC = unlist(AIC_results),
  stringsAsFactors = FALSE
)

aic_rank$overall_rank <- rank(aic_rank$AIC, ties.method = "first")
aic_rank <- aic_rank[order(aic_rank$overall_rank), ]

rownames(aic_rank)<- NULL


get_season <- function(combo_name, season_list) {
  matched <- season_list[sapply(season_list, function(season) grepl(season, combo_name))]
  if (length(matched) > 0) return(matched[1]) else return(NA)
}

aic_rank$season <- sapply(aic_rank$combo, get_season, season_list = seasons)

seasona_ranks <- list()
for (s in 1:length(seasons)){
  season <- seasons[s]
  temp <- aic_rank[which(aic_rank$season==season),]
  temp$seas_rank <- rank(temp$AIC, ties.method = "first")
  temp <- temp[order(temp$seas_rank), ]
  rownames(temp)<- NULL
  seasona_ranks[[season]] <- temp
}

dim(cmat)

dim(dat)

# pdo
mod.list1 <- list(
  U = "unequal",
  R = "diagonal and equal",
  Q = "diagonal and unequal",
  c = cmat,
  C = "unequal"
)

# no pdo
mod.list2 <- list(
  U = "unequal",
  R = "diagonal and equal",
  Q = "diagonal and unequal")


years <- which(np_climate_seasonal$year %in% colnames(dat))
spring_gmsst <- np_climate_seasonal$gmsst.spring[years]
cmattemp <- matrix(spring_gmsst, nrow=1)
cmat2 <- rbind(cmat,cmattemp)
names(np_climate_seasonal)
mod.list3 <- list(
  U = "unequal",
  R = "diagonal and equal",
  Q = "diagonal and unequal",
  c = cmat2,
  C = "equal"
)



library(MARSS)
fit1 <- MARSS(dat, model=mod.list1)
fit2 <- MARSS(dat, model=mod.list2)
fit3 <- MARSS(dat, model=mod.list3)

autoplot(fit3, plot.type="fitted.ytT")
autoplot(fit1, plot.type="fitted.ytT")





AIC(fit2)

AIC(fit1)
AIC(fit3)









####If you want to plot together
fitted1 <- as.data.frame(t(fit1$ytT))
fitted3 <- as.data.frame(t(fit3$ytT))

fitted1$Year <- as.numeric(colnames(dat))
fitted3$Year <- as.numeric(colnames(dat))

# Add model identifiers
fitted1$Model <- "Model PDO"
fitted3$Model <- "Model pdogmsst"
dat
# Reshape data for plotting
fitted_long <- bind_rows(fitted1, fitted3) %>%
  pivot_longer(cols = -c(Year, Model), names_to = "Series", values_to = "Fitted")

# Plot
ggplot(fitted_long, aes(x = Year, y = Fitted, color = Model)) +
  geom_line() +
  facet_wrap(~ Series, scales = "free_y") +
  labs(title = "Fitted Values from Two MARSS Models",
       y = "Fitted Value") +
  theme_minimal()
