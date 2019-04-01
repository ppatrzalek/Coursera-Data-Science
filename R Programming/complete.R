# Coursera R Programming
# Week 2

# Exercise 2

setwd("C:/Users/Piotr/Desktop/Coursera/R Programming/week 2/specdata")

complete <- function(directory = "specdata", id = 1:10){
  
  stopifnot(is.character(directory), 
            length(directory) == 1, 
            is.vector(id))
  
  id.list <- dir()
  data <- data.frame()
  
  for(i in id){
    file <- read.csv(id.list[i], header = TRUE, sep = ",")
    complete.file <- file[which(complete.cases(file) == TRUE), ]
    value <- c(i, nrow(complete.file))
    data <- rbind(data, value)
  }
  colnames(data) <- c("id", "nobs")
  data
}


# Test

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)
