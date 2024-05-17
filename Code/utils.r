library("data.table")
library("caret")

get_dataset <- function(type) {
  if (type == "original") {
    train <- read.table("Data/dt_train_original.txt")
    test <- read.table("Data/dt_test_original.txt")
  } else if (type == "original_pca") {
    train <- read.table("Data/dt_train_original_pca.txt")
    test <- read.table("Data/dt_test_original_pca.txt")
  } else if (type == "processed") {
    train <- read.table("Data/dt_train_processed.txt")
    test <- read.table("Data/dt_test_processed.txt")
  } else if (type == "processed_pca") {
    train <- read.table("Data/dt_train_processed_pca.txt")
    test <- read.table("Data/dt_test_processed_pca.txt")
  }
  return(list(train = train, test = test))
}

convert_cat_to_factor <- function(dt, type) {
  dt_ <- copy(dt)

  if (type == "original") {
    dt_[, c(1:6, 37)] <- lapply(dt_[c(1:6, 37)], as.factor)
  } else if (type == "original_pca") {
    dt_[, c(1:6, 16)] <- lapply(dt_[c(1:6, 16)], as.factor)
  } else if (type == "processed") {
    dt_[, c(1:5, 27)] <- lapply(dt_[c(1:5, 27)], as.factor)
  } else if (type == "processed_pca") {
    dt_[, c(1:5, 15)] <- lapply(dt_[c(1:5, 15)], as.factor)
  }
  return(dt_)
}

# Align factor levels
align_factor_levels <- function(dt_train, dt_test) {
  for (col in names(dt_train)) {
    if (is.factor(dt_train[[col]]) && is.factor(dt_test[[col]])) {
      # Align the levels
      print(col)
      levels(dt_train[[col]]) <- levels(dt_test[[col]])
    }
  }
  return(dt_train)
}

split_feature_target <- function(dt, type) {
  if (type == "original") {
    x <- copy(dt[, -37])
    y <- copy(dt[, 37])
  } else if (type == "original_pca") {
    x <- copy(dt[, -16])
    y <- copy(dt[, 16])
  } else if (type == "processed") {
    x <- copy(dt[, -27])
    y <- copy(dt[, 27])
  } else if (type == "processed_pca") {
    x <- copy(dt[, -15])
    y <- copy(dt[, 15])
  }
  return(list(x = x, y = y))
}

get_train_control <- function(validation_method) {
  if (validation_method == "cv") {
    # 10-Fold Cross Validation
    return(trainControl(method = "cv", number = 10))
  } else if (validation_method == "repeatedcv") {
    # Repeated 10-Fold Cross Validation (5 repeats)
    return(trainControl(method = "repeatedcv", number = 10, repeats = 5))
  }
}
