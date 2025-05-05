

library(atsalibrary)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)


chinook.year.filtered <- chinook.year %>%
  filter(!State %in% c("Michigan", "Pennsylvania"))

ggplot(chinook.year.filtered, aes(x = Year, y = metric.tons)) +
  geom_line() +
  facet_wrap(~ State, ncol = 2, scales = "free_y") +
  labs(
    title = "Chinook Landings by State",
    x = "Year",
    y = "Log(Metric Tons)"
  ) +
  theme_minimal()
unique(chinook.month$State)
ch_month <- chinook.month %>%
  filter(!State %in% c("Michigan", "Pennsylvania"))

# Create the time series 
dat_CA <- ts(chinook.year.filtered$log.metric.tons[chinook.year.filtered$State == "California"], start = chinook.year.filtered$Year[chinook.year.filtered$State == "California"][1], )
dat_WA <-ts(chinook.year.filtered$log.metric.tons[chinook.year.filtered$State == "Washington"], start = chinook.year.filtered$Year[chinook.year.filtered$State == "Washington"][1], )
dat_AK <-ts(chinook.year.filtered$log.metric.tons[chinook.year.filtered$State == "Alaska"], start = chinook.year.filtered$Year[chinook.year.filtered$State == "Alaska"][1], )
dat_OR <- ts(chinook.year.filtered$log.metric.tons[chinook.year.filtered$State == "Oregon"], start = chinook.year.filtered$Year[chinook.year.filtered$State == "Oregon"][1], )


#Check acf and pacf function

check_acf <- function(ts_list, area_names) {
  # ts_list: a list of 4 time series (e.g., list(ts_CA, ts_OR, ts_WA, ts_AK))
  # area_names: character vector of 4 names (e.g., c("California", "Oregon", "Washington", "Alaska"))
  
  if (length(ts_list) != 4 | length(area_names) != 4) {
    stop("Provide exactly 4 time series and 4 area names.")
  }
  
  par(mfrow = c(4, 2), mar = c(3, 3, 3, 1))  # 4 rows, 2 columns layout
  
  for (i in 1:4) {
    ts <- na.interp(ts_list[[i]])
    
    # ACF plot
    acf(ts, main = paste("ACF -", area_names[i]))
    
    # PACF plot
    pacf(ts, main = paste("PACF -", area_names[i]))
  }
  
  # Reset layout to default
  par(mfrow = c(1,1))
}


list_area <- list(dat_AK,dat_CA,dat_OR,dat_WA)
area_names <- c("California", "Oregon", "Washington", "Alaska")

check_acf(ts_list = list_area,area_names)

ndiffs(dat_CA, test='adf')
ndiffs(dat_AK, test='adf') #diff 1
ndiffs(dat_WA, test='adf') #diff 1
ndiffs(dat_OR, test='adf')



#########################################################################


arima_ff <- function(ts_list, labels) {
  if (is.null(labels)) {
    labels <- paste("Series", 1:length(ts_list))
  }
  
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))  #Make the panels for the plot
  
  results <- list()
  for (i in 1:length(ts_list)) {
    ts_data <- ts_list[[i]]
    
    
    # Train and test data windows
    train <- window(ts_data, start = start(ts_data), end = c(start(ts_data)[1] + 20))
    test  <- window(ts_data, start = c(start(ts_data)[1] + 20), end = c(start(ts_data)[1] + 66))
    
    # Fit ARIMA and forecast
    model <- auto.arima(train)
    
    
    fore <- forecast(model, h = 3)
    acc <- accuracy(fore, test)
    
    # Plot forecast and compare
    plot(fore, main = paste("Forecast -", labels[i]), ylab = "Value", xlab = "Time")
    points(test, col = "red", pch = 16)
    
    results[[i]] <- list(
      Label = labels[i],
      AIC = AIC(model),
      BIC = BIC(model),
      MAPE = acc[2, "MAPE"],
      RMSE = acc[2, "RMSE"],
      Model = as.character(model))
    
    
  }
  
  par(mfrow = c(1, 1))  # Reset plots
  df_results <- do.call(rbind, lapply(results, as.data.frame))
  return(df_results)
  
}

arima_ff(list_area,area_names)



# Create the time series 
datmon_CA <- ts(ch_month$metric.tons[ch_month$State == "CA"], start = c(1, 1), frequency = 12 )
datmon_WA <-ts(ch_month$metric.tons[ch_month$State == "WA"], start = c(1, 1), frequency = 12 )
datmon_OR <- ts(ch_month$metric.tons[ch_month$State == "OR"], start = c(1, 1), frequency = 12 )

list_area_month <- list(datmon_CA,datmon_OR,datmon_WA )
area_names_m <- c("California", "Oregon", "Washington")


ets_ff <- function(ts_list, labels = NULL) {
  if (is.null(labels)) {
    labels <- paste("Series", 1:length(ts_list))
  }
  
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))  # 2x2 panel layout
  results <- list()
  for (i in 1:length(ts_list)) {
    ts_data <- na.interp(ts_list[[i]])
    
    # Define train/test split (adjust if needed based on length of your data)
    train <- window(ts_data, start = start(ts_data), end = c(start(ts_data)[1] + 20))
    test  <- window(ts_data, start = c(start(ts_data)[1] + 20), end = c(start(ts_data)[1] + 66))
    
    # Fit ARIMA and forecast
    model <- ets(train)
    pred <- forecast(model, h = 2*12)
    
    acc <- accuracy(pred, test)
    
    # Plot forecast with test data
    plot(pred, main = paste("Forecast -", labels[i]), ylab = "Value", xlab = "Time")
    points(test, col = "red", pch = 16)
    
    results[[i]] <- list(
      Label = labels[i],
      AIC = AIC(model),
      BIC = BIC(model),
      MAPE = acc[2, "MAPE"],
      RMSE = acc[2, "RMSE"],
      Model = model$method)
    
  }
  
  
  par(mfrow = c(1, 1))  # Reset plotting for later
  final_results <- do.call(rbind, lapply(results, as.data.frame))
  return(final_results)
}



ets_full <- ets_ff(list_area_month,area_names_m)
ets_full$type <- "test"


hw_ff <- function(ts_list,labels) {
  if (is.null(labels)) {
    labels <- paste("Series", 1:length(ts_list))
  }
  
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))  # 2x2 panel layout
  results <- list()
  for (i in 1:length(ts_list)) {
    ts_data <- na.interp(ts_list[[i]])
    

    train <- window(ts_data, start = start(ts_data), end = c(start(ts_data)[1] + 20))
    test  <- window(ts_data, start = c(start(ts_data)[1] + 20), end = c(start(ts_data)[1] + 66))
    
    # Fit and forecast
    model <- hw(train)

    pred <- forecast(model, h = 2*12)
    acc <- accuracy(pred, test)

    
    # Plot forecast with test data
    plot(pred, main = paste("Forecast HW -", labels[i]), ylab = "Value", xlab = "Time")
    points(test, col = "red", pch = 16)
    
    results[[labels[i]]] <- list(
      model_structure = model$method,
      accuracy_metrics = acc
    )
  }
  
  final_results <- do.call(rbind, lapply(results, as.data.frame))
  par(mfrow = c(1, 1))  # Reset plotting for later

  return(final_results)
}



hw_ff(list_area_month,area_names_m)





#block size is how much is dropped, drop_start is where it happens
ets_ff_sensitivity <- function(ts_list, labels = NULL, block_size, drop_start) {
  
  if (is.null(labels)) {
    labels <- paste("Series", 1:length(ts_list))
  }
  
  results <- list()
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))  # Plot setup
  
  for (i in seq_along(ts_list)) {
    ts_orig <- ts_list[[i]]
    ts_mod <- ts_orig
    
    # Drop the block and interpolate the data
    drop_end <- min(drop_start + block_size - 1, length(ts_mod))
    ts_mod[drop_start:drop_end] <- NA
    ts_clean <- na.interp(ts_mod)
    
    # Save original missing indexes for plotting
    missing_idx <- which(is.na(ts_mod))
    interpolated_times <- time(ts_clean)[missing_idx]
    interpolated_values <- ts_clean[missing_idx]
    
    # Define time range
    ts_times <- time(ts_clean)
    
    # Split into train/test
    train <- window(ts_clean, start = start(ts_clean), end = c(start(ts_clean)[1] + 20))
    test  <- window(ts_clean, start = c(start(ts_clean)[1] + 20), end = c(start(ts_clean)[1] + 66))
    
    # Fit ETS model
    model <- ets(train, model = "ZZZ")
    fc <- forecast(model, h = length(test))
    
    # Accuracy
    acc <- accuracy(fc, test)
    
    # Store results 
    results[[i]] <- list(
      Label = labels[i],
      AIC = AIC(model),
      BIC = BIC(model),
      MAPE = acc[2, "MAPE"],
      RMSE = acc[2, "RMSE"],
      Model = model$method
    )
    
    # Plot forecast
    plot(fc, main = paste(labels[i], "- Drop:", drop_start, "to", drop_end), ylab = "Value", xlab = "Time")
    
    # Learning to shade the dropped regions
    drop_time_start <- ts_times[drop_start]
    drop_time_end   <- ts_times[drop_end]
    usr <- par("usr")  # plotting range
    rect(xleft = drop_time_start, xright = drop_time_end,
         ybottom = usr[3], ytop = usr[4],
         col = rgb(0.9, 0.9, 0.9, 0.5), border = NA)
    

    
    # Add test points and interpolated points
    lines(test, col = "lightgreen")
    points(interpolated_times, interpolated_values, col = "blue")
    
    # Re-plot forecast line on top
    lines(fc$mean, col = "red")
    

    legend("topright",
           legend = c("Forecast", "Test", "Interpolated", "Dropped"),
           col = c("red", "lightgreen", "blue", rgb(0.9, 0.9, 0.9, 0.5)),
           pch = c(NA, NA, 1, 15), lty = c(1, 1, NA, NA),
           pt.cex = 0.7,  # Size of points/symbols
           cex = 0.7,     # Size of text
           bty = "n",     # No box
           y.intersp = 0.7,  # Vertical spacing between lines
           x.intersp = 0.5) 
  }
  
  par(mfrow = c(1, 1))  # Reset plot layout
  
  # Compile results into a data frame
  final_results <- do.call(rbind, lapply(results, as.data.frame))
  return(final_results)
}

ets_sens <- ets_ff_sensitivity(list_area_month, labels=area_names_m, drop_start = 30,block_size=96)

ets_target_sensitivity <- function(ts_list, labels = NULL, block_size, drop_start, target) {
  
  if (is.null(labels)) {
    labels <- paste("Series", 1:length(ts_list))
  }
  
  results <- list()
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))  # Plot setup
  
  for (i in seq_along(ts_list)) {
    ts_orig <- ts_list[[i]]
    ts_mod <- ts_orig
    
    # Replace values above 200 with NA in the block 
    drop_end <- min(length(ts_mod), drop_start + block_size - 1)  # Avoids indexing problem if we chose to do the end of series
    ts_mod[drop_start:drop_end][ts_mod[drop_start:drop_end] > 200] <- NA
    
    # Interpolate the dropped values
    ts_clean <- na.interp(ts_mod)
    
    # Keeping the missing indexes for plotting
    miss_i <- which(is.na(ts_mod))
    interpolated_times <- time(ts_clean)[miss_i]
    interpolated_values <- ts_clean[miss_i]
    
    # Use time get the months as point values
    ts_times <- time(ts_clean)
    
    # Split into train/test
    train <- window(ts_clean, start = start(ts_clean), end = c(start(ts_clean)[1] + 20))
    test  <- window(ts_orig, start = c(start(ts_orig)[1] + 20), end = c(start(ts_orig)[1] + 66))
    
    # Fit model
    model <- ets(train, model = "ZZZ")
    fc <- forecast(model, h = length(test))
    
    
    acc <- accuracy(fc, test)
    
    # Save results for later
    results[[i]] <- list(
      Label = labels[i],
      AIC = AIC(model),
      BIC = BIC(model),
      MAPE = acc[2, "MAPE"],
      RMSE = acc[2, "RMSE"],
      Model = model$method
    )
    
    # Plot everything 
    
    plot(ts_orig, main = paste(labels[i], "- Drop:", target), ylab = "Value", xlab = "Time", col = "black") #original
    lines(ts_clean, col = "blue", lwd = 2)  # interpolated
    
    
    #True values of the test data
    lines(test, col = "lightgreen")
    
    # plot the forecasts
    lines(fc$mean, col = "red")
    
    
    legend("topright", legend = c("Original", "Interpolated", "Forecast", "Test"),
           col = c("black", "blue", "red", "lightgreen"),
          lty = c(1, 1, 1, 1), pt.cex = 1.5, bty = "n")
  }
  
  par(mfrow = c(1, 1))  # Reset plot layout
  
  # Compile results into a data frame
  final_results <- do.call(rbind, lapply(results, as.data.frame))
  return(final_results)
}


#Pulling it all together for comparison
ets_targ_sens <- ets_target_sensitivity(list_area_month, labels = area_names_m, drop_start = 1, block_size = 33 * 12, target = 200)

ets_sens$type <- "Block"
ets_targ_sens$type <- "Target200"

summary_sens <- rbind(ets_sens,ets_targ_sens,ets_full)

summary_sens

