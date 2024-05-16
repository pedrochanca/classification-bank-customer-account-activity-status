# Libraries
library("e1071")
library("caTools")
library("caret")
library("data.table")

# Get the Train and Test Datasets
dt_train <- read.table("Data/dt_train_original.txt")
dt_test <- read.table("Data/dt_test_original.txt")

# Ensure target columns are factors
dt_train[, 37] <- factor(dt_train[, 37])
dt_test[, 37] <- factor(dt_test[, 37])

# Split the dataset into training and validation
# ----------------------------------------------
set.seed(123)
split <- sample.split(dt_train[, 37], SplitRatio = 0.7)
dt_train_subset <- subset(dt_train, split == TRUE)
dt_validation_subset <- subset(dt_train, split == FALSE)

# Create object x which holds the features
x_train <- copy(dt_train_subset[, -37])
x_validation <- copy(dt_validation_subset[, -37])

# Create an object y which holds the target
y_train <- copy(dt_train_subset[, 37])
y_validation <- copy(dt_validation_subset[, 37])

# Define Hyperparameters to gridsearch
# ------------------------------------
c_range <- cbind(1, 10, 100, 1000, 10000, 100000, 100000)
g_range <- cbind(
  0.0000001, 0.000001, 0.00001,
  0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000
)

# Hypertune SVM paramters - GridSearch
# ------------------------------------
for (c in c_range) {
  for (g in g_range) {
    model <- svm(
      x_train, y_train,
      type = "C-classification",
      kernel = "radial", gamma = g, cost = c
    )
    y_pred <- predict(model, newdata = x_validation)

    confusion_matrix <- table(y_validation, y_pred)
    accuracy <- (
      confusion_matrix[1, 1] + confusion_matrix[2, 2]
    ) / sum(confusion_matrix)

    cat("The accuracy for cost", c, "and gamma", g, "was", accuracy, "\n")
  }
}



# Function to do Cross Validation to find the best pair
# of hyper parameters discovered previously
accuracy <- 0
folds <- createFolds(dt_train[, 37], k = 10)
cross_v_bank <- lapply(folds, function(x) {
  training_fold <- dt_train[-x, ]
  test_fold <- dt_train[x, ]

  svm_cross <- svm(
    formula = V37 ~ .,
    data = training_fold,
    type = "C-classification",
    kernel = "radial",
    cost = 1000,
    gamma = 0.1
  )

  y_cross <- predict(svm_cross, newdata = test_fold[, 1:36])

  conf_matrix <- table(test_fold[, 37], y_cross)
  accuracy <- ((conf_matrix[1, 1] + conf_matrix[2, 2]) / sum(conf_matrix))
  return(accuracy)
})

# Get the final accuracy
mean_cv <- array(as.numeric(unlist(cross_v_bank)), dim = c(10, 1))
mean(mean_cv)

# Prediction on the Test Set
svm_bank <- svm(
  formula = V37 ~ ., data = dt_train,
  type = "C-classification", kernel = "radial", gamma = 0.1, cost = 1000
)
y_pred_bank <- predict(svm_bank, newdata = dt_test[, 1:36])

conf_matrix_bank <- table(dt_test[, 37], y_pred_bank)
accuracy <- (
  conf_matrix_bank[1, 1] + conf_matrix_bank[2, 2]
) / sum(conf_matrix_bank)

confusionMatrix(y_pred_bank, dt_test[, 37])
