# Coursera R Programming
# Week 2

# Exercise 1

getwd()
setwd("C:/Users/Piotr/Desktop/Coursera/R Programming/week 2/specdata")

pollutantmean <- function(directory = "specdata", pollutant = "sulfate", id = 1:10){
  # Read vector of csv names
  id.list <- dir()
  # Create vector to keep a value of means for all dataset
  mean.file <- data.frame()
  # Loop to read csv data and calculate mean 
  for(i in id) {
    data <- read.csv(id.list[i], header = TRUE, sep = ",")
    mean.file <- rbind(mean.file, data)
  }
  
  mean(mean.file[, pollutant], na.rm = TRUE)
}

# Test

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
