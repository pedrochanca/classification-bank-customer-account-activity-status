# Libraries
library("caret")
library("e1071")
library("data.table")

# Get the Train and Test Datasets
dt_train <- read.table("Data/dt_train_original.txt")
dt_test <- read.table("Data/dt_test_original.txt")

# Split Features and Target
# -------------------------
# Create object x which holds the features
x_train <- copy(dt_train[, -37])
x_test <- copy(dt_test[, -37])

# Create an object y which holds the target
y_train <- factor(copy(dt_train[, 37]))
y_test <- factor(copy(dt_test[, 37]))

head(y_train)
head(y_test)

# Fit the Model
# -------------
# Define the training control
# Methods tested:
# 'boot' (bootstrap), 'cv' (k-fold cross validation), 'repeatedcv'
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

# Tuning the smoothing
search_grid <- expand.grid(usekernel = c(TRUE, FALSE), fL = 0:1, adjust = 1)

# Trainig the Naive Bayes Model
model <- train(
  x_train, y_train, "nb",
  trControl = train_control,
  tuneGrid = search_grid
)

# Train Evaluation
# ----------------
# predict target values
y_pred <- predict(model, newdata = x_train)
# get confusion matrix, accuracy and other metrics
confusionMatrix(y_pred, y_train)

# Test Evaluation
# ---------------
# predict target values
y_pred <- predict(model, newdata = x_test)
# get confusion matrix, accuracy and other metrics
confusionMatrix(y_pred, y_test)

# Plot variables performance
# --------------------------
variable_importance <- varImp(model)
plot(
  variable_importance,
  xlab = "Importance", ylab = "Variables",
  main = "Variables Importance on the Naive Bayes Classifier"
)
