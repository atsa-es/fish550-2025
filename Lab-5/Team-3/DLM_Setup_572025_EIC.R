#MARSS DLM Modelling Setup 
#May 7th 2025
#Written by EIC to accompany the methods written for ATSA Course 550 Spring 2025
#This script sets up the MARSS DLM model for the ATSA550 course

#PSEUDOCODE PLAN

#Step 2: Organize the covariates matrix to have one row for each covariate (ice out and escapement)
#Have a column for each year and have this form be a matrix
#Then bind the two covariates together to form a matrix with 2 rows and ncol=years
#Will have 2 alternatives for the Wood Freshwater model, one with SNAP and one with IO
## covariates <- rbind(iceout,escapement)

#Step 3: Set up the Z Matrix for time-varying regression coefficients
##Z <- matrix(c(1, covariates[1, ], covariates[2, ]), nrow = 1)

#Step 4: Set up the Z matrix to be a list of matricies so that it can vary by year??
##TT <- ncol(y)
#Z.list <- list()
#for (t in 1:TT) {
#  Z.list[[t]] <- matrix(c(1, covariates[1, t], covariates[2, t]), nrow = 1)
#}

#Load Libraries
library(MARSS)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(atsalibrary)

#Set WD
setwd("~/Desktop/Research_Repos/CourseWork/FISH550/ModellingCodes")
setwd("C:/Users/Emma/Desktop/Research_Repos/CourseWork/FISH550/ModellingCodes")


#### STEP 1 ####
#Load in the data - Starting with just Wood
WMFW1_1x <- read.csv("Wood_12_13_mean.csv")

#Trim to only include post-1950 samples (Now have 1960-2020)
WMFW1_1x <- WMFW1_1x %>% filter(FW1_Year > 1950)

#Transpose so that each row in the first column is now it's own column header and each row in the second column is all in one row across columns
WMFW1_1x <- t(WMFW1_1x)

#Convert to a matrix and remove the first row, and transpose again
WMFW1_1x <- WMFW1_1x[-1,]
WMFW1_1x <- as.matrix(WMFW1_1x)
WMFW1_1x <- t(WMFW1_1x)

y <- WMFW1_1x

#Need to convert to log10 or natural log of mm measurements?


#### STEP 2 ####
#Load in the covariates, including the alternative covariates

#Lake Aleknagik Ice Out, normalize as well using Cube Root Method
IO <- read.csv("ALEKIO.csv")

#Remove all columns from Ice except columns 3 and 6
IO <- IO[, c(3, 6)]

#Rename Column 6 in Ice as "Day"
IO <- IO %>% rename(Day = Day_.after_May_1)
#Add a row to ice that is year = 2016 and Day = -27
IO <- rbind(IO, c(2016, -2))
IO <- rbind(IO, c(2013, 40))
IO <- IO %>% rename(Year = YearSampled)

#Remove any rows with NAs
IO <- IO[complete.cases(IO), ]

#Normalize the IO data using the Robust Method
# Calculate the median and IQR of the Day column
median_value <- median(IO$Day, na.rm = TRUE)
IQR_value <- IQR(IO$Day, na.rm = TRUE)

# Apply robust normalization (subtract median and divide by IQR)
IO$Day_Robust <- (IO$Day - median_value) / IQR_value

#Now, still a higher value = cooler summer (late Ice Out)

#Remove the Day column and transpose
IO <- IO[, -2]
IO <- t(IO)

#Make the first row the column names
colnames(IO) <- IO[1, ]

#Remove the first row
IO <- IO[-1, ]
IO <- t(IO)

#SNAP Data from Lake Aleknagik
SNAP <- read.csv("SNAP.csv")

#Filter to just have Month = 6 and Lake = Aleknagik and Year > 1950
SNAP <- SNAP %>% filter(Month == 6 & Lake == "Aleknagik" & Year > 1950)

#Remove the Month and Lake columns
SNAP <- SNAP[, -c(2, 4)]
SNAP <- t(SNAP)

colnames(SNAP) <- SNAP[1, ]
SNAP <- SNAP[-1, ]
SNAP <- t(SNAP)


#Log10 Escapement
wbrood <- read.csv("wood_brood copy.csv")

#Changing the column "Brood.Year" to "Return_Year"
colnames(wbrood)[colnames(wbrood) == "Brood.Year"] <- "Return_Year"

#Make the format correct for the numbers in the Escapement column
wbrood$Escapement <- as.numeric(gsub(",", "", wbrood$Escapement))

#Remove all columns except Return_Year and Escapement
wbrood <- wbrood %>% dplyr::select(Return_Year, Escapement)

#Remove rows where there is an NA
wbrood <- wbrood %>% filter(!is.na(Escapement))

#Add a column to wbrood called "log_escp" that is the log of the Escapement column
wbrood$log_escp <- log10(wbrood$Escapement)


#Now need to correctly lag the years in the Escapement data to make sure that the Escapement Year = FW1 growth year - 1
wbrood <- wbrood %>% rename(EscpYear = Return_Year)

#Add column to the wbrood called "Year" that is the EscpYear + 1
wbrood$Year <- wbrood$EscpYear + 1

#Now get rid of the EscpYear Column and the log_escp column
wbrood <- wbrood %>% dplyr::select(Year, Escapement)

#Now transpose
wbrood <- t(wbrood)
colnames(wbrood) <- wbrood[1, ]
wbrood <- wbrood[-1, ]
wbrood <- t(wbrood)

#Trim the wbrood to only keep columns 1-57
str(wbrood)
wbrood <- wbrood[, 1:57, drop = FALSE]

#Now need to combine the covariates into one matrix
#Make sure IO and wbrood are the same length
length(IO)
IO <- IO[, 16:74, drop = FALSE]


# Get the column names and convert them to numeric
years <- as.numeric(colnames(IO))

# Get the order of the years
year_order <- order(years)

# Reorder the columns
IO <- IO[, year_order, drop = FALSE]
length(IO)
length(wbrood)

#Drop the last two columns in IO
IO <- IO[, -c(58, 59), drop = FALSE]

#Trim SNAP to also match wbrood
length(SNAP)
colnames(SNAP)
SNAP <- SNAP[, 14:70, drop = FALSE]

#Now they're the same length and we can combine them into one
covariates_IOwbrood <- rbind(IO, wbrood)
rownames(covariates_IOwbrood) <- c("IceOut", "Escapement")
covariates_SNAPwbrood <- rbind(SNAP, wbrood)
rownames(covariates_SNAPwbrood) <- c("SNAP", "Escapement")


#Now need to add a row called "lnescp" to both of the covariate dfs that is the log of the escapement in all of the columns
covariates_IOwbrood <- rbind(covariates_IOwbrood, log(covariates_IOwbrood[2, ]))
covariates_SNAPwbrood <- rbind(covariates_SNAPwbrood, log(covariates_SNAPwbrood[2, ]))

#Call the new row "lnescp"
rownames(covariates_IOwbrood)[3] <- "lnescp"
rownames(covariates_SNAPwbrood)[3] <- "lnescp"

#Now need to remove the escapement row from the covariates
covariates_IOwbrood <- covariates_IOwbrood[-2, ]
covariates_SNAPwbrood <- covariates_SNAPwbrood[-2, ]



#### STEP 3 ####
Z_IOwb <- matrix(c(1, covariates_IOwbrood[1, ], covariates_IOwbrood[2, ]), nrow = 1)

Z.list_IOwb <- lapply(1:T, function(t) {
  matrix(c(IO[t], wbrood[t]), nrow = 1)
})

Z_SNAPwb <- matrix(c(1, covariates_SNAPwbrood[1, ], covariates_SNAPwbrood[2, ]), nrow = 1)

#### STEP 4 ####
#Need to trim y so that its the same length as the covariates (57)
length(y)
#Remover the first four columns of y (This is 1960-1963)
y <- y[, -c(1:4), drop = FALSE]

TT <- ncol(y)
Z.list_IOwb <- list()
for (t in 1:TT) {
  Z.list_IOwb[[t]] <- matrix(c(1, covariates_IOwbrood[1, t], covariates_IOwbrood[2, t]), nrow = 1)
}


##### STEP 5 ####
#Set up the model list
#State Transition Equation
#B <- "identity"
#State Intercept
#U <- "zero"
#State Process Error
#Q <- "diagonal and unequal"
#Observation Error Matrix R
#R <- "equalvar" or "indentity" or diag(1, nrow = 1) or diag(1, nrow=3)?


#Check that y is the same length as Z.list_IOwb (57)
length(y)
length(Z.list_IOwb)

#Force everything into being a matrix to run
Z.list_IOwb <- lapply(Z.list_IOwb, function(z) {
  if (!inherits(z, "matrix")) matrix(z, nrow = 1) else z
})

Z.list_IOwb <- lapply(Z.list_IOwb, function(z) {
  z[] <- as.numeric(z) 
  return(z)
})

# Define initial state x0 (one value for each of the 3 states)
initial_x0 <- rep(0, 3)  # 3 states: FW1, IO, and density

# Define the model list with the corrected Z and x0
model.list_IOwb <- list(
  Z = matrix(1),
  B = matrix(1),
  U = matrix(0),
  Q = matrix("q"),  
  R = matrix("r"),        
  A = "zero",             
  x0 = matrix(0),         
  V0 = "identity"  # <-- updated!
)

# Run the model with the corrected specification
fit_IOwbrood <- MARSS(y, model = model.list_IOwb, 
             control = list(maxit = 1000, allow.degen = TRUE))

#Check the output
summary(fit_IOwbrood)

#Plotting the observed vs fitted
# Make sure y is a numeric vector
y_vec <- as.numeric(y)

# Extract fitted values and SEs
fitted_vals <- fit_IOwbrood$states[1, ]
se <- sqrt(fit_IOwbrood$states.se[1, ])

# Compute CI bounds
upper <- fitted_vals + 1.96 * se
lower <- fitted_vals - 1.96 * se

# Create time axis
time <- 1:length(fitted_vals)

# Combine into a data frame
df_plot <- data.frame(
  Time = time,
  Observed = y_vec,
  Fitted = fitted_vals,
  Upper = upper,
  Lower = lower
)

ggplot(df_plot, aes(x = Time)) +
  geom_line(aes(y = Observed), color = "black", size = 1, linetype = "solid") +
  geom_line(aes(y = Fitted), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  labs(title = "Observed vs Fitted with 95% CI",
       y = "Response", x = "Time") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )



######################Ending here 5/22. Tried to plot the effect of IO and escp over time, but the Z matrix is not estimating 3 states, only one.