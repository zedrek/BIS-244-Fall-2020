# Solution file for BIS 244 Assignment 02, Fall 2020

# Clear out Console and Environment
rm(list=ls(all=TRUE))
cat("\014")

# Let's read in the us-counties file from covid-19-data

# We'll use package readr, which is part of the tidyverse
library(tidyverse)

# Storing the path of the current working directory
Temp <- getwd()

# Switching the working directory to the covid-19-data subfolder
setwd("./covid-19-data/")

# Reading the us-states.csv in as a data frame
STATES <- read_csv("us-states.csv")

# Switching the working directory back to the project folder
setwd(Temp)

# Examining the data
# View(STATES)

# Using filter()to get just Snohomish county in Washington
PA <- filter(STATES, state=="Pennsylvania")
# View(PA)

# Set n to legth of data set
n <- length(PA$date)

# Initialize new variable in data frame
PA$adj_deaths <- 0

# Calculate values for adj_deaths
for (i in 1:n) {
  if (as.character(PA$date[i])=="2020-04-21") {
    PA$adj_deaths[i] <- (PA$deaths[i] - 282)
  } else if (as.character(PA$date[i])=="2020-04-22") {
    PA$adj_deaths[i] <- (PA$deaths[i] - 297)
  }
    else {PA$adj_deaths[i] <- PA$deaths[i]}
}

# Calculating sum of all adjusted deaths as checksum
sum(PA$adj_deaths)
