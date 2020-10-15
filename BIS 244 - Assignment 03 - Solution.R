# Solution file for BIS 244 Assignment 03, Fall 2020

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
COUNTIES <- read_csv("us-counties.csv")

# Switching the working directory back to the project folder
setwd(Temp)

# Examining the data
# View(counties)

# Using filter()to get just Snohomish county in Washington
MONROE <- filter(COUNTIES, (state=="Pennsylvania" & county == "Monroe"))
# View(MONROE)

p <- ggplot(data = MONROE)
p + geom_point(mapping = aes(x = date, y = cases), color = "blue") +
  geom_point(mapping = aes(x = date, y = deaths), color = "black") +
      labs(x = "Date",
       y = "Counts",
       title = "COVID-19 Cases and Deaths in Monroe County, PA") 
