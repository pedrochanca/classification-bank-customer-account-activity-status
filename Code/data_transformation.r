# Libraries
library("data.table")

# Load Train Data
# ---------------
dt_train <- read.table("Data/client_train.txt")
dt_test <- read.table("Data/client_test_marked.txt")

# Checking the data
head(dt_train)
head(dt_test)

# Clean Train and Test Datasets
# -----------------------------
# Function to remove underscores and convert to numeric
remove_underscore <- function(column) {
  # Remove underscores
  column <- gsub("_", "", column)

  return(column)
}

for (i in c(1:6, 37)) {
  # apply remove_underscore function
  dt_train[, i] <- remove_underscore((dt_train[, i]))
  dt_test[, i] <- remove_underscore((dt_test[, i]))

  # make a column categorical
  dt_train[, i] <- as.factor(as.numeric(as.character(dt_train[, i])))
  dt_test[, i] <- as.factor(as.numeric(as.character(dt_test[, i])))
}

# Saving the datasets
write.table(
  dt_train,
  file = "Data/dt_train_original.txt",
  append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE
)

write.table(
  dt_test,
  file = "Data/dt_test_original.txt",
  append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE
)


# New Train Dataset with extra features
# The new features are transformations of the existing ones
# ---------------------------------------------------------

dt_train_extra <- copy(dt_train)
head(dt_train_extra)

dt_train_extra[, 37] <- dt_train_extra[, 11]^2 + dt_train_extra[, 13]^2
dt_train_extra[, 38] <- 5 * (
  (dt_train_extra[, 13] - mean(dt_train_extra[, 13]))^2 +
    (dt_train_extra[, 33] - mean(dt_train_extra[, 33]))^2
)
dt_train_extra[, 39] <- (
  (dt_train_extra[, 19] - mean(dt_train_extra[, 19]))^2 +
    (dt_train_extra[, 33] - mean(dt_train_extra[, 33]))^2
)
dt_train_extra[, 40] <- (
  (dt_train_extra[, 31] - mean(dt_train_extra[, 31]))^2 +
    (dt_train_extra[, 27] - mean(dt_train_extra[, 27]))^2
)
dt_train_extra[, 41] <- (
  (dt_train_extra[, 27] - mean(dt_train_extra[, 27]))^2 +
    (dt_train_extra[, 33] - mean(dt_train_extra[, 33]))^2
)
dt_train_extra[, 42] <- (
  (dt_train_extra[, 21] - mean(dt_train_extra[, 21]))^2 +
    (dt_train_extra[, 16] - mean(dt_train_extra[, 16]))^2
)
dt_train_extra[, 43] <- (
  (dt_train_extra[, 21] - mean(dt_train_extra[, 21]))^2 +
    (dt_train_extra[, 19] - mean(dt_train_extra[, 19]))^2
)
dt_train_extra[, 44] <- (
  (dt_train_extra[, 33] - mean(dt_train_extra[, 33]))^2 +
    (dt_train_extra[, 11] - mean(dt_train_extra[, 11]))^2
)
dt_train_extra[, 45] <- (
  (dt_train_extra[, 19] - mean(dt_train_extra[, 19]))^2 +
    (dt_train_extra[, 31] - mean(dt_train_extra[, 31]))^2
)
dt_train_extra[, 46] <- (
  1000 * (dt_train_extra[, 31] - mean(dt_train_extra[, 31]))^2 +
    (dt_train_extra[, 21] - mean(dt_train_extra[, 21]))^2
)
dt_train_extra[, 47] <- (
  (dt_train_extra[, 25] - mean(dt_train_extra[, 25]))^2 +
    (dt_train_extra[, 27] - mean(dt_train_extra[, 27]))^2
)
dt_train_extra[, 48] <- (
  (dt_train_extra[, 13] - mean(dt_train_extra[, 13]))^2 +
    (dt_train_extra[, 27] - mean(dt_train_extra[, 27]))^2
)
dt_train_extra[, 49] <- (
  (dt_train_extra[, 33] - mean(dt_train_extra[, 33]))^2 +
    (dt_train_extra[, 25] - mean(dt_train_extra[, 25]))^2
)
dt_train_extra[, 50] <- (
  (dt_train_extra[, 33] - mean(dt_train_extra[, 33]))^2 +
    (dt_train_extra[, 21] - mean(dt_train_extra[, 21]))^2
)
dt_train_extra[, 51] <- copy(dt_train[, 37])

# Saving the dataset
write.table(
  dt_train_extra,
  file = "Data/dt_train_original_extra.txt",
  append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE
)


# New Test Dataset with extra features
# The new features are transformations of the existing ones
# ---------------------------------------------------------
dt_test_extra <- copy(dt_test)
head(dt_test_extra)

dt_test_extra[, 37] <- (
  (dt_test_extra[, 19] - mean(dt_test_extra[, 19]))^2 +
    (dt_test_extra[, 33] - mean(dt_test_extra[, 33]))^2
)
dt_test_extra[, 38] <- (
  (dt_test_extra[, 21] - mean(dt_test_extra[, 21]))^2 +
    (dt_test_extra[, 19] - mean(dt_test_extra[, 19]))^2
)
dt_test_extra[, 39] <- copy(dt_test[, 37])

# Saving the dataset
write.table(
  dt_test_extra,
  file = "Data/dt_test_original_extra.txt",
  append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE
)

# New Train dataset with processed features
# This was done after having assessed the results from the EDA
# ------------------------------------------------------------
index_columns <- c(1:2, 4:10, 14:21, 24:25, 27:28, 30, 32, 36, 39, 43, 51)
dt_train_processed <- copy(
  dt_train_extra[, index_columns]
)
write.table(
  dt_train_processed,
  file = "Data/dt_train_processed.txt",
  append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE
)


# New Test dataset with processed features
# This was done after having assessed the results from the EDA
# ------------------------------------------------------------
index_columns <- c(1:2, 4:10, 14:21, 24:25, 27:28, 30, 32, 36:39)
dt_test_processed <- copy(
  dt_test_extra[, index_columns]
)
write.table(
  dt_test_processed,
  file = "Data/dt_test_processed.txt",
  append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE
)
