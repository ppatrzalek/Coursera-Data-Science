# 1. Preprocessing -----------------------------------------------------------
setwd("C:/Users/ppatrzalek/Desktop/Coursera/Practical Machine Learning")
training_original <- read.csv("pml-training.csv", sep = ",", header = TRUE) 
testing_original <- read.csv("pml-testing.csv", sep = ",", header = TRUE)

library(dplyr)
training <- training_original %>%
  select(-c("raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "num_window", "X", "user_name"))

testing <- testing_original %>%
  select(-c("raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "num_window", "X", "user_name"))

dim(training); dim(testing)

# 1.1 Near Zero Variables -----------------------------------------------------
library(caret)
training_nzv <- nearZeroVar(training, saveMetrics = TRUE)
nzv <- which(training_nzv$nzv == TRUE)
training <- training[,-nzv]
testing <- testing[,-nzv]
dim(training); dim(testing)


# We use nearZeroVar() function to drop columns which probably can't help us to predict "classe" variable.
# These variables have very low variance value so I think that it is a good idea to drop them at the moment.

# 1.2 Na variables ------------------------------------------------------------
library(tidyr)
na.variables <- function(data){
  n_column <- ncol(data)
  n_rows <- nrow(data)
  na_columns <- c()
  for(i in 1:n_column){
  
    if (length(which(is.na(data[,i]) == TRUE))/n > 0.95) na_columns[i] <- i

  }
  which(is.na(na_columns) == FALSE)
}

na_variables<- na.variables(training)

training <- training[,-na_variables]
testing <- testing[,-na_variables]
dim(training); dim(testing)

# Moreover I think that variables where are almost 95% of NA's values aren't nessesary. Ofcourse sometimes it could be very good predictor,
# but in this analysis I will try to forecast "classe" variables without that predictors. 

# After 1st and 2nd steps of my analysis i have not 160 variables but only 60. Now i will try to plot some predictors and i 
# calculate correlation between predictors. In my opinion if some predictors have high correlation then i can use only one of them.
# Maybe after this part of analysis i will use PCA method to replace many predictors by one or two. Below are results of my work.

# 1.3 Correlation predictors -------------------------------------------------------
correlation_variables <- training %>%
  select(-classe)

high_correlation_predictors <- function(data, value){
  results_matrix <- as.matrix(cor(data, method = "pearson"))
  results <- data.frame(row = rownames(results_matrix)[row(results_matrix)], 
                        col = colnames(results_matrix)[col(results_matrix)], 
                        corr = c(results_matrix))
  results <- results %>%
    filter(corr != 1)
  
  results %>%
    filter(abs(corr) > value )
}

correlation_variables2 <- high_correlation_predictors(correlation_variables, 0.9)
# View(correlation_variables)

correlation_belt <- high_correlation_predictors(select(correlation_variables, contains("belt")), 0.9)
drop_belt <- as.vector.factor(distinct(correlation_belt, col)[2:6,1])

correlation_arm <- high_correlation_predictors(select(correlation_variables, contains("arm")), 0.9)
drop_arm <- c("gyros_arm_y")
  
correlation_dumbbell <- high_correlation_predictors(select(correlation_variables, contains("dumbbell")), 0.9)
drop_dumbbell <- c("gyros_dumbbell_z")

training <- training %>%
  select(-c(drop_belt, drop_arm, drop_dumbbell))

testing <- testing %>%
  select(-c(drop_belt, drop_arm, drop_dumbbell))

dim(training); dim(testing)

# 2. Modelling --------------------------------------------------------------------

# 2.0 Create two sets - traning and test 

inTrain <- createDataPartition(y = training$classe, p = 0.7, list = FALSE)

training_model <- training[inTrain,]
testing_model <- training[-inTrain,]

# 2.1. Decision trees ----------------------------------------------------------------
modelFitTree <- train(classe ~., method = "rpart", data = training_model)

library(rattle)
fancyRpartPlot(modelFitTree$finalModel)

new_classe <- predict(modelFitTree, training_model)
training_predict_tree <- cbind(training_model, new_classe)
training_predict_tree$check <- ifelse(training_predict_tree$classe == training_predict_tree$new_classe, 1, 0)
Accuracy_tree_training <- sum(training_predict_tree$check)/nrow(training_predict_tree)

new_classe_testing <- predict(modelFitTree, testing_model)
testing_predict_tree <- cbind(testing_model, new_classe_testing)
testing_predict_tree$check <- ifelse(testing_predict_tree$classe == testing_predict_tree$new_classe_testing, 1, 0)
Accuracy_tree_testing <- sum(testing_predict_tree$check)/nrow(testing_predict_tree)

# 2.2. Random Forest ----------------------------------------------------------------
library(randomForest)
modelFitRandomForest <- train(classe ~., method = "rf", data = training_model, ntree = 10,
                              trControl = trainControl(method = "repeatedcv", 
                                                       number = 10,
                                                       repeats = 10,
                                                       classProbs = TRUE
                                                       ))

new_classe_training <- predict(modelFitRandomForest, training_model)
training_predict_rf <- cbind(training_model, new_classe_training)
training_predict_rf$check <- ifelse(training_predict_rf$classe == training_predict_rf$new_classe_training, 1, 0)
Accuracy_rf_training <- sum(training_predict_rf$check)/nrow(training_predict_rf)

new_classe_testing <- predict(modelFitRandomForest, testing_model)
testing_predict_rf <- cbind(testing_model, new_classe_testing)
testing_predict_rf$check <- ifelse(testing_predict_rf$classe == testing_predict_rf$new_classe_testing, 1, 0)
Accuracy_rf_testing <- sum(testing_predict_rf$check)/nrow(testing_predict_rf)

# 3.0 Predicting -----------------------------------------------
predict_classe <- predict(modelFitRandomForest, testing)

testing$new_classe <- predict_classe
print(testing)



