############################################################################
#
library(e1071)
library(caret)
library(rpart)
library(ggplot2)
library(magrittr)  
# Linear Regression 
linear_model <- function(
    data = dataset, outcome = "Outcome.1", predictor_vars = c("X.1")){
  # Predictor_vars is supplied with vector of predictors
  # Ensure number of observations is greater than 1 and
  # outcome argument is supplied
  stopifnot(dim(data)[1]>1, !is.null(outcome), !is.null(predictor_vars))
  #
  y <- outcome
  #
  set.seed(420)
  #Partition Data Train And Test
  # Partition
  sample_size <- floor((2/3)*nrow(data))
  #
  train_index <- sample(x = seq_len(nrow(data)),
                        size = sample_size)
  #
  train_data <- data[train_index,]
  test_data <- data[-train_index,]
  # formula of the model
  formula_reg <- as.formula(paste(y,"~",paste(predictor_vars,collapse = "+")))
  #
  # Simple Linear Regression
  #
  linear_model <- lm(
    formula_reg,  data = train_data,)
  reg_summary <- summary(linear_model) # Summary
  # ---------------------------------------------------------------
  ## Coefficient Confidence Intervals
  coeff_interval <- confint(linear_model, level = 0.90) %>%
    knitr::kable(caption = "90% CI on Model Coefficients")
  
  ## Driver Analysis
  coef <- as.numeric(signif(linear_model$coefficients[2:length(linear_model$coefficients)],2))
  var <- names(linear_model$coefficients[2:length(linear_model$coefficients)])
  num_s <- unlist(lapply(data, is.numeric)) 
  data_1 <- data[ , num_s]
  data_1 <- data[, which(names(data_1) %in% c(predictor_vars)), drop=F]
  variab.sat <- signif(colMeans(data_1), 2)
  coeff_data <- data.frame(coef, variab.sat)
  #rownames(coeff_data) = as.character(var)
  # Driver Analysis
  driver_plot <- ggplot(coeff_data, aes(x=variab.sat, y=coef)) + geom_point() +
    theme(legend.title=element_blank()) +
    labs(title="Driver Analysis",
         x="satisfaction", y= "Coefficients") +
    geom_hline(yintercept = (((range(coef)[[2]] - range(coef)[[1]])/2)
                             + range(coef)[[1]])) +
    geom_vline(xintercept = (((range(variab.sat)[[2]] - range(variab.sat)[[1]])/2)
                             + range(variab.sat)[[1]])) +
    geom_label(aes(variab.sat, coef, label=row.names(coeff_data)), nudge_y = .05)
  
  #   Model Diagnostics
  #### There are plots made from Model Diagnostics in Linear regression
  #### We always have 4 plots, which are below for which you can get
  #   
  ### """  The diagnostic plots show residuals in four different ways:
  ###   Residuals vs Fitted. Used to check the linear relationship assumptions. A horizontal line, without distinct patterns is an indication for a linear relationship, what is good.
  ###   Normal Q-Q. Used to examine whether the residuals are normally distributed. It's good if residuals points follow the straight dashed line.
  ###   Scale-Location (or Spread-Location). Used to check the homogeneity of variance of the residuals (homoscedasticity). Horizontal line with equally spread points is a good indication of homoscedasticity. This is not the case in our example, where we have a heteroscedasticity problem.
  ###   Residuals vs Leverage. Used to identify influential cases, that is extreme values that might influence the regression results when included or excluded from the analysis. This plot will be described further in the next sections."""
  #   
  
  ### As much as there was a comment on that:
  
  ### 2. There should be some portion of the code where it 
  ### checks on the 4 assumptions for simple linear regression
  
  ### Whta you are in search for are usually observations made from the below 4
  ### graphs that are part of the output, so am unsure what u mean by CODE NODE AVAILABLE comment 2
  
  # Checking Data data meets the regression conditions
  #par(mfrow=c(2,2))
  par(mar=c(1,1,1,1))
  plot1 <- plot(linear_model, 1)
  plot2 <- plot(linear_model, 2)
  plot3 <- plot(linear_model, 3)
  plot4 <- plot(linear_model, 4)
  par(mar=c(5.1, 4.1, 4.1, 2.1))
  #par(mfrow=c(1,1))
  return(list(
    "regression_summary" = reg_summary,
    "coefficient_interval" = coeff_interval,
    "Driver Analysis" = driver_plot, 
    "Diagnostic Plot 1" = plot1, "Diagnostic Plot 2" = plot2, 
    "Diagnostic Plot 3" = plot3, "Diagnostic Plot 4" = plot4))
}

# Logistic 
logistic_model <- function(
    data = dataset, outcome = "Outcome.1", predictor_vars = c("X.1")){
  # Predictor_vars is supplied with vector of predictors
  # Ensure number of observations is greater than 1 and
  # outcome argument is supplied
  stopifnot(dim(data)[1]>1, !is.null(outcome), !is.null(predictor_vars))
  #
  y <- outcome
  #
  set.seed(420)
  #Partition Data Train And Test
  # Partition
  sample_size <- floor((2/3)*nrow(data))
  #
  train_index <- sample(x = seq_len(nrow(data)),
                        size = sample_size)
  #
  train_data <- data[train_index,]
  test_data <- data[-train_index,]
  # formula of the model
  formula_reg <- as.formula(paste(y,"~",paste(predictor_vars,collapse = "+")))
  #
  ## --------------------------------------------------------
  # Logistic Regression
  trControl <- trainControl(method = 'cv', number = 5)
  logit5_CV <- train(
    formula_reg, method = 'glmnet', trControl = trControl,
    data = train_data,
    family = 'binomial' )
  
  trControl <- trainControl(method = 'cv', number = 5)
  logit5_CV <- train(
    formula_reg, method = 'glmnet', trControl = trControl,
    data = train_data,
    family = 'binomial' )
  # Make predictions on the train data
  predicted_classes_LOG_train <- logit5_CV %>%
    predict(train_data)
  # Make predictions on the train data
  predicted_classes_LOG_test <- logit5_CV %>%
    predict(test_data)
  # Confusion Matrix
  confusion_matrix_logistic_train <- confusionMatrix(
    data = as.factor(predicted_classes_LOG_train),
    reference = as.factor(train_data[[outcome]]))
  confusion_matrix_logistic_test <- confusionMatrix(
    data = as.factor(predicted_classes_LOG_test),
    reference = as.factor(test_data[[outcome]]))
  # Variable Importance
  logitImp <- varImp(logit5_CV, scale = FALSE)
  logitImp_plot <- plot(logitImp)
  #
  return(list(
    "logistic_regression_train" = confusion_matrix_logistic_train,
    "logistic_regression_test" = confusion_matrix_logistic_test,
    "logistic Variable Importance" = logitImp_plot,
    "Model" = logit5_CV))
}

# RandomForest
randomForest_model <- function(
    data = dataset, outcome = "Outcome.1", predictor_vars = c("X.1")){
  # Predictor_vars is supplied with vector of predictors
  # Ensure number of observations is greater than 1 and
  # outcome argument is supplied
  stopifnot(dim(data)[1]>1, !is.null(outcome), !is.null(predictor_vars))
  #
  y <- outcome
  #
  set.seed(420)
  #Partition Data Train And Test
  # Partition
  sample_size <- floor((2/3)*nrow(data))
  #
  train_index <- sample(x = seq_len(nrow(data)),
                        size = sample_size)
  #
  train_data <- data[train_index,]
  test_data <- data[-train_index,]
  # formula of the model
  formula_reg <- as.formula(paste(y,"~",paste(predictor_vars,collapse = "+")))
  #
  # Fit the Random Forest model on the training set
  set.seed(1)
  randomForest5_CV <- train(
    formula_reg,
    data = train_data, method = "rf",
    trControl = trainControl("cv", number = 5)
  )
  # Make predictions on the train data
  predicted_classes_RF_train <- randomForest5_CV %>%
    predict(train_data)
  # Make predictions on the test data
  predicted_classes_RF_test <- randomForest5_CV %>%
    predict(test_data)
  # Variable Importance
  confusion_matrix_random_forest_train <- confusionMatrix(
    data = as.factor(predicted_classes_RF_train),
    reference = as.factor(train_data[[outcome]]))
  confusion_matrix_random_forest_test <- confusionMatrix(
    data = as.factor(predicted_classes_RF_test),
    reference = as.factor(test_data[[outcome]]))
  # Variable Importance
  randfImp <- varImp(randomForest5_CV, scale = FALSE)
  randfImp_plot <- plot(randfImp)
  #
  return(list(
    "random_forest_train" = confusion_matrix_random_forest_train,
    "random_forest_test" = confusion_matrix_random_forest_test,
    "Random Forest Variable Importance" = randfImp_plot,
    "model" = randomForest5_CV))
}
#
# ======================================================================


# Input #####################################

# =======================================================================

### So we are reading in the data file, the below codes open selection

cat("Select Data", "\n", sep=" ")
# R program reading a csv file using file.choose()
dataset = read.csv(file.choose(), header = T)
# If you use the code above in RStudio
# you will be asked to choose a file
head(dataset)
#

### Enter the outcome variable of interest after the input
sapply(dataset, class)
#
cat("Input the Outcome Variable", "\n", sep=" ")
outcomeVar <- as.character(readline("Outcome Variable = "))
#
cat("Input the Predictor Variables, separated by spaces", "\n", sep=" ")
### Enter the predictor variables of interest in input below
### separating each by a space
predictorVar <- as.character(readline("Predictor Variable = "))
predictorVar <- c(strsplit(x = predictorVar, split = " ")[[1]])
#
cat(paste0("Outcome Variable: ", outcomeVar, "\n", "Predictor Variable: ", 
           paste0(predictorVar, collapse = ", ")))
#
### Converting predictor variables to factor
### This is based on the assumption that all are categorical 
###### Dont run this if data variables have different classes, 
###### we used in the sample data given its nature and can/should be adjusted
dataset[predictorVar] <- lapply(dataset[predictorVar], factor) 
sapply(dataset, class)

## Manual Dummy Coding is just time consuiming as the various methos are able to 
## better handle factor columns, SO WHY DO IT MANUALLY WHEN THE METHOD CAN HANDLE
## THE PREDICTOR FACTORS?
#####################CODE

### We have alist of the available Models, the 3 models requested on the data

cat(paste0(
  "Available models", "\n", 
  "1. Linear Regression", "\n", 
  "2. Logistic Regression", "\n", 
  "3. Random Forest"))
#
Models_available = data.frame(
  number = c(1:3),
  model = c("Linear Regression", "Logistic Regression", "Random Forest")
)
#

### In the next code, we 1st check the count of unique values in the outcome variable
### Why so? We checking because for Logistic Model classification, we classifying
### observations into 2 classes. So if the outcome variable has only 2 values ie 
### `(length(outcomeVar_unique)==2)`, then we have a binary outcome. 
### Otherwise, if the length is higher than 2, we assume that it is continuous
### You can see the models suggested from the output based on below code 
###
outcomeVar_unique <- unique(dataset[[outcomeVar]])
if(length(outcomeVar_unique)==2){
  cat(paste0("Outcome Variable is Binary with Category:", 
             paste0(outcomeVar_unique, collapse = ","),"\n", "\n", 
             "Available Models:", "\n", "2. Logistic Regression", "\n", 
             "3. Random Forest",
             collapse = ""))
} else {cat("Outcome Variable is Continuous", "\n", "\n", 
            "Available Models:", "\n", "1. Linear Regression", "\n", 
            "3. Random Forest")}

### So we have an output printed with suggested models 
### So below, enter the MODEL NUMBER as input

# Input the Model based on Above table
cat("Input the Model: Input Number", "\n", sep=" ")
Model_Chosen <- as.numeric(readline(prompt = "Model = "))
#
Model_Chosen <- Models_available[grep(paste0("^",as.character(Model_Chosen)), 
                                      Models_available$number),2]
cat("Model Chosen: ", Model_Chosen, sep=" ")
### We have a printout of model chosen


### So if you chose a model above, it will be run whenever you run code below
### Each model is supplied with parameters of the dataset used, the 
### outcome variable and predictor variables

# Model Selection based on Above
output <- NULL #Stores the output from the models
if (Model_Chosen == "Linear Regression"){
  output <- linear_model(data = dataset, outcome = outcomeVar, predictor_vars = predictorVar)
} else {
  if (Model_Chosen == "Logistic Regression"){
    output <- logistic_model(data = dataset, outcome = outcomeVar, predictor_vars = predictorVar)
  } else {
    output <- randomForest_model(data = dataset, outcome = outcomeVar, predictor_vars = predictorVar)
  }}
#
output
#








