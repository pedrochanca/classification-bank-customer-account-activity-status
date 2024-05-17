# logreg
# LogitBoost
# regLogistic  Regularized Logistic Regression
# http://topepo.github.io/caret/train-models-by-tag.html#logic-regression

# Libraries
library("caret")
library("MLmetrics")
library("kknn")
library("class")

########## HOW TO RUN IT: ###########

# For each classifier, its necessary to replace the "method" and the tuning paramt. by replacing:
## knn: "kknn"
#  tuning paramt: kmax = seq(1, 25, 2), distance=1:2, kernel=c("optimal","rectangular","gaussian")

## Random Forest: "rf"             -
# tuning paramt: mtry = 1:20

## Logistic Regression boosted: "LogitBoost"     -
# tuning paramt: nIter = seq(1, 25, 2)

# Its necessary to replace the datasets as well

# The datasets:             X train |  X test | Y train | Y test
#                           -------------------------------------
# Initial Dataset :         x_train    x_test   y_train   y_test
# Processed Dataset :       x_train3   x_test3  y_train3  y_test3
# Initial Dataset w/ PCA:   x_train4   x_test4  y_train4  y_test4
# Processed Dataset w/ PCA: x_train5   x_test5  y_train5  y_test5

######################################


# Load Datasets
# -------------
dataset.train <- read.table("train_data_new_classifier.txt", header = FALSE)
dataset.test <- read.table("test_data_new_classifier.txt", header = FALSE)

dataset.edited.train <- read.table("train_data_removed", header = FALSE)
dataset.edited.test <- read.table("test_data_removed.txt", header = FALSE)

dataset.edited.train2 <- read.table("train_data_removed_2.txt", header = FALSE)
dataset.edited.test2 <- read.table("test_data_removed_2.txt", header = FALSE)

dataset.pca.train <- read.csv("dataset_pca.csv", header = TRUE)
dataset.pca.test <- read.csv("dataset_pca_test.csv", header = TRUE)

dataset.pca.train2 <- read.csv("dataset_processed_pca_train_1.csv", header = TRUE, sep = ",")
dataset.pca.test2 <- read.csv("dataset_processed_pca_test_1.csv", header = TRUE, sep = ",")

dataset.pca.train2$X <- NULL
dataset.pca.test2$X <- NULL

dataset.pca.train["X"] <- NULL
dataset.pca.test["X"] <- NULL


# Dataset 1 - Initial Dataset
# Use: x_train y_train / x_test y_test
# ------------------------------------

# Train set
y_train <- dataset.train[, 37] # 37 column is the class
x_train <- dataset.train[, 1:36] # 36 first columns are the features

# Test set
y_test <- dataset.test[, 37] # 37 column is the class
x_test <- dataset.test[, 1:36] # 36 first columns are the features

# Dataset 3 - Processed Dataset
# Use: x_train3 y_train3 / x_test3 y_test3
# ----------------------------------------

# Removed and new features
x_train3 <- dataset.edited.train[, 1:26]
y_train3 <- dataset.edited.train[, 27]

x_test3 <- dataset.edited.test[, 1:26]
y_test3 <- dataset.edited.test[, 27]

x_train3_2 <- dataset.edited.train2[, 1:27]
y_train3_2 <- dataset.edited.train2[, 28]

x_test3_2 <- dataset.edited.test2[, 1:27]
y_test3_2 <- dataset.edited.test2[, 28]


# Dataset 4 - Inicial Dataset w/ PCA
# Use: x_train4 y_train4 / x_test4 y_test4
# ----------------------------------------

y_train4 <- dataset.pca.train[, 16] # 16 column is the class
x_train4 <- dataset.pca.train[, 1:15] # 315 first columns are the features

y_test4 <- dataset.pca.train[, 16] # 16 column is the class
x_test4 <- dataset.pca.train[, 1:15] # 15 first columns are the features

# Dataset 5 - Processed Dataset w/ PCA
# Use: x_train5 y_train5 / x_test5 y_test5
# ----------------------------------------

y_train5 <- dataset.pca.train2[, 24] # 24 column is the class
x_train5 <- dataset.pca.train2[, 1:23] # 23 first columns are the features

y_test5 <- dataset.pca.test2[, 24] # 24 column is the class
x_test5 <- dataset.pca.test2[, 1:23] # 23 first columns are the features





# Set seed
# --------
set.seed(2020)


# Repetead K-Folds Cross Validation
# ---------------------------------

# Train the model
pred.knn.m2 <- train(CL ~ .,
  method = "kknn",
  tuneGrid = expand.grid(
    kmax = seq(1, 25, 2),
    distance = 1:2,
    kernel = c("optimal", "rectangular", "gaussian")
  ),
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 4),
  metric = "Accuracy",
  data = cbind(CL = as.factor(y_train), x_train)
)

# Summarize the results
print(pred.knn.m2)
plot(pred.knn.m2)
fitm2 <- predict(pred.knn.m2, newdata = x_test)

# Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(fitm2, as.factor(y_test))


# Bootstrap
# ---------

# Train the model
pred.knn.m3 <- train(CL ~ .,
  method = "kknn",
  tuneGrid = expand.grid(
    kmax = seq(1, 25, 2),
    distance = 1:2,
    kernel = c("optimal", "rectangular", "gaussian")
  ),
  trControl = trainControl(method = "boot", number = 100),
  metric = "Accuracy",
  data = cbind(CL = as.factor(y_train), x_train)
)

# Summarize the results
print(pred.knn.m3)
plot(pred.knn.m3)
fitm4 <- predict(pred.knn.m3, newdata = x_test)

# Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(fitm3, as.factor(y_test))


# Leave One Out Cross validation
# ------------------------------

# Train the model
pred.knn.m4 <- train(CL ~ .,
  method     = "LogitBoost",
  tuneGrid   = expand.grid(nIter = seq(1, 25, 2)),
  trControl  = trainControl(method = "LOOCV"),
  metric     = "Accuracy",
  data       = cbind(CL = as.factor(y_train), x_train)
)

# Summarize the results
print(pred.knn.m4)
plot(pred.knn.m4)
fitm4 <- predict(pred.knn.m4, newdata = x_test)

# Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(fitm4, as.factor(y_test))


# K Folds Cross Validation
# ------------------------

# Train the model
pred.knn.m53 <- train(CL ~ .,
  method     = "rf",
  tuneGrid   = expand.grid(mtry = 1:20),
  trControl  = trainControl(method = "cv", number = 10),
  metric     = "Accuracy",
  data       = cbind(CL = as.factor(y_train), x_train)
)

# Summarize the results
print(pred.knn.m53)
plot(pred.knn.m53)
fitm53 <- predict(pred.knn.m53, newdata = x_test)

# Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(fitm53, as.factor(y_test))
