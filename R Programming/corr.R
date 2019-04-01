
source("complete.R")

corr <- function(directory = "specdata", threshold = 0) {
  
  id.list <- dir()
  data_complete <- complete("specdata", 1:332)
  row.count <- data_complete$nobs
  
  ids <- data_complete$id[row.count > threshold]
  cor.vector <- c()

  for(i in ids){
    file <- read.csv(id.list[i], header = TRUE, sep = ",")
    cor.vector[i] <- cor(file$sulfate, file$nitrate, use = "complete.obs", method = "pearson")
  }
  obs <- which(complete.cases(cor.vector) == TRUE)
  cor.vector[obs]
}

# Test
corr("specdata", 150)
