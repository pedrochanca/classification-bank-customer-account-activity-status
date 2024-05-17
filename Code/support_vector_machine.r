# Libraries
library("e1071")
library("data.table")
library("caret")
library("caTools")
source("Code/utils.r")
set.seed(2020)



# Get the Train and Test Datasets
# -------------------------------
dt_type <- "original"

# get datasets
dt <- get_dataset(dt_type)
target_col_index <- get_target_column_index(dt_type)
dt_train <- dt$train
dt_test <- dt$test

# convert categorical columns to factors
# dt_train <- convert_cat_to_factor(dt_train, dt_type)
# dt_test <- convert_cat_to_factor(dt_test, dt_type)

# align factor levels
# dt_train <- align_factor_levels(dt_train, dt_test)

# convert target column values to factor
dt_train <- convert_target_to_factor(dt_train, dt_type)
dt_test <- convert_target_to_factor(dt_test, dt_type)

# visualize
head(dt_train)
head(dt_test)

# split data into feature and target
result <- split_feature_target(dt_train, dt_type)
x_train <- result$x
y_train <- result$y

result <- split_feature_target(dt_test, dt_type)
x_test <- result$x
y_test <- result$y

str(dt_test)



# Split the dataset into training and validation
# ----------------------------------------------
split <- sample.split(dt_train[, target_col_index], SplitRatio = 0.7)
dt_train_subset <- subset(dt_train, split == TRUE)
dt_validation_subset <- subset(dt_train, split == FALSE)

# Create object x which holds the features
x_train <- copy(dt_train_subset[, -target_col_index])
x_validation <- copy(dt_validation_subset[, -target_col_index])

# Create an object y which holds the target
y_train <- copy(dt_train_subset[, target_col_index])
y_validation <- copy(dt_validation_subset[, target_col_index])




# Define Hyperparameters to gridsearch
# ------------------------------------
cost_range <- cbind(1, 10, 100, 1000, 10000, 100000, 100000)
gamma_range <- cbind(
  0.0000001, 0.000001, 0.00001,
  0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000
)

# Hypertune SVM paramters - GridSearch
# ------------------------------------
for (cost in cost_range) {
  for (gamma in gamma_range) {
    # fit model
    model <- svm(
      x_train, y_train,
      type = "C-classification",
      kernel = "radial", gamma = gamma, cost = cost
    )

    # predict target
    y_validation_pred <- predict(model, newdata = x_validation)

    # evaluate
    confusion_matrix <- table(y_validation, y_validation_pred)
    accuracy <- (
      confusion_matrix[1, 1] + confusion_matrix[2, 2]
    ) / sum(confusion_matrix)

    cat(
      "The accuracy for cost", cost, "and gamma", gamma, "was", accuracy, "\n"
    )
  }
}





# Cross Validaton + GridSearch hyperparamters
# -------------------------------------------

compute_cross_validation <- function(x, gamma, cost) {
  train_fold <- dt_train[-x, ]
  validation_fold <- dt_train[x, ]

  x_train <- train_fold[, -target_col_index]
  y_train <- train_fold[, target_col_index]

  x_validation <- validation_fold[, -target_col_index]
  y_validation <- validation_fold[, target_col_index]

  model <- svm(
    x_train, y_train,
    type = "C-classification",
    kernel = "radial", gamma = gamma, cost = cost
  )

  y_validation_pred <- predict(model, newdata = x_validation)

  confusion_matrix <- table(y_validation, y_validation_pred)
  accuracy <- (
    (confusion_matrix[1, 1] + confusion_matrix[2, 2]) / sum(confusion_matrix)
  )

  return(accuracy)
}

folds <- createFolds(dt_train[, target_col_index], k = 10)
gamma <- 0.1
cost <- 1000
cv_accuracies <- lapply(
  folds, function(fold) compute_cross_validation(fold, gamma, cost)
)

cv_mean_accuracies <- mean(
  array(as.numeric(unlist(cv_accuracies)), dim = c(10, 1))
)





# Predict target values
# ---------------------´
gamma <- 0.1
cost <- 1000

# fit the model with selected parameters
model <- svm(
  x_train, y_train,
  type = "C-classification", kernel = "radial", gamma = gamma, cost = cost
)

# predict train target values
y_train_pred <- predict(model, newdata = x_train)

# compute confusion matrix + accuracy (train)
confusionMatrix(y_train_pred, y_train)

# predict test target values
y_test_pred <- predict(model, newdata = x_test)

# compute confusion matrix + accuracy (test)
confusionMatrix(y_test_pred, y_test)
