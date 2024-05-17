# Libraries
library("caret")
source("Code/utils.r")
set.seed(2020)




# Get the Train and Test Datasets
# -------------------------------
dt_type <- "original"

# get datasets
dt <- get_dataset(dt_type)
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




# Fit the model
# -------------

# define train control ('boot', 'cv', 'repeatedcv', 'loocv')
validation_method <- "cv"
train_control <- get_train_control(validation_method)

# define smoothing
search_grid <- expand.grid(
  kmax = seq(1, 25, 2),
  distance = 1:2,
  kernel = c("optimal", "rectangular", "gaussian")
)

# fit the model
model <- train(
  x_train, y_train, "kknn",
  trControl = train_control,
  tuneGrid = search_grid,
  metric = "Accuracy"
)

# summarized results
print(model)
plot(model)





# Predict target values
# ---------------------

# predict train target values
y_train_pred <- predict(model, newdata = x_train)

# compute confusion matrix + accuracy (train)
confusionMatrix(y_train_pred, y_train)

# predict test target values
y_test_pred <- predict(model, newdata = x_test)

# compute confusion matrix + accuracy (test)
confusionMatrix(y_test_pred, y_test)
