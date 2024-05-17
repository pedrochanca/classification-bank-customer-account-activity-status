# Libraries
library("data.table")

# Functions
# ---------
get_eigenvalues <- function(pca_result) {
  # Extract the standard deviations of the principal components
  sdev <- pca_result$sdev

  # Calculate the eigenvalues (variances of the principal components)
  eigen_values <- sdev^2

  return(eigen_values)
}

get_variance_explained <- function(eigen_values) {
  # Calculate the proportion of variance explained
  prop_variance_explained <- eigen_values / sum(eigen_values)

  # Calculate the cumulative proportion of variance explained
  cum_variance_explained <- cumsum(prop_variance_explained)

  return(list(
    prop_variance_explained = prop_variance_explained,
    cum_variance_explained = cum_variance_explained
  ))
}

get_principal_components <- function(
    dt, combinations, n_components, n_examples, n_numerical_features) {
  # Populate the list with the repeated sequences
  final <- list()
  for (i in 1:n_components) {
    final[[i]] <- 1:n_examples
  }

  # Get principal components
  for (i in 1:n_components) {
    for (j in 1:n_examples) {
      final[[i]][j] <- 0
      for (c in 1:n_numerical_features) {
        final[[i]][j] <- (
          final[[i]][j] + combinations[c, i] * dt[j, c]
        )
      }
    }
  }

  return(final)
}

# PCA on ORIGINAL Train Dataset
# -----------------------------
dt_train <- read.table("Data/dt_train_original.txt")

# Convert categorical columns to factors
dt_train[, c(1:6, 37)] <- lapply(dt_train[c(1:6, 37)], as.factor)

# Apply standardized PCA to the continuous variables
pca_result <- prcomp(dt_train[, 7:36], scale = TRUE)

# Picking Number of components from PCA
eigen_values <- get_eigenvalues(pca_result)
eigen_values

variance_explained <- get_variance_explained(eigen_values)
variance_explained # let's pick 9 components

# Lets keep 9 components
combinations <- pca_result$rotation[, 1:9]
combinations

# Lets ignore the categorical variables when creating the new dataset
dt_train_pca <- copy(dt_train[7:36])

# Define number of examples and components
n_numerical_features <- 30
n_examples <- 12000
n_components <- 9

final <- get_principal_components(
  dt_train_pca, combinations,
  n_components, n_examples, n_numerical_features
)

# Get the final dataset
dt_train_pca <- do.call(cbind, final)
# Bind with the categorical variables
dt_train_pca <- cbind(dt_train[1:6], dt_train_pca)
# Bind with the class variable
dt_train_pca <- cbind(dt_train_pca, dt_train[, 37])

# Save the dataset
write.table(
  dt_train_pca,
  file = "Data/dt_train_original_pca.txt",
  append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE
)


# PCA on ORIGINAL Test Dataset
# ----------------------------
dt_test <- read.table("Data/dt_test_original.txt")

# Convert categorical columns to factors
dt_test[, c(1:6, 37)] <- lapply(dt_test[c(1:6, 37)], as.factor)

# Apply standardized PCA to the continuous variables
pca_result <- prcomp(dt_test[, 7:36], scale = TRUE)

# Picking Number of components from PCA
eigen_values <- get_eigenvalues(pca_result)
eigen_values

variance_explained <- get_variance_explained(eigen_values)
variance_explained # let's pick 9 components

# Lets keep 9 components
combinations <- pca_result$rotation[, 1:9]

# Lets ignore the categorical variables when creating the new dataset
dt_test_pca <- copy(dt_test[7:36])

# Define number of examples and components
n_numerical_features <- 30
n_examples <- 12000
n_components <- 9

final <- get_principal_components(
  dt_test_pca, combinations,
  n_components, n_examples, n_numerical_features
)

# Get the final dataset
dt_test_pca <- do.call(cbind, final)
# Bind with the categorical variables
dt_test_pca <- cbind(dt_test[1:6], dt_test_pca)
# Bind with the class variable
dt_test_pca <- cbind(dt_test_pca, dt_test[, 37])

# Save the dataset
write.table(
  dt_test_pca,
  file = "Data/dt_test_original_pca.txt",
  append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE
)


# PCA on PROCESSED Train Dataset
# ------------------------------
dt_train_prosd <- read.table("Data/dt_train_processed.txt")
head(dt_train_prosd)

# Convert categorical columns to factors
dt_train_prosd[, c(1:5, 27)] <- lapply(dt_train_prosd[c(1:5, 27)], as.factor)

# Apply standardized PCA to the continuous variables
pca_result <- prcomp(dt_train_prosd[, 6:26], scale = TRUE)

# Picking Number of components from PCA
eigen_values <- get_eigenvalues(pca_result)
eigen_values

variance_explained <- get_variance_explained(eigen_values)
variance_explained # let's pick 9 components

# Lets keep 9 components
combinations <- pca_result$rotation[, 1:9]

# Lets ignore the categorical variables when creating the new dataset
dt_train_prosd_pca <- copy(dt_train_prosd[6:26])

# Define number of examples and components
n_numerical_features <- 21
n_examples <- 12000
n_components <- 9

final <- get_principal_components(
  dt_train_prosd_pca, combinations,
  n_components, n_examples, n_numerical_features
)

# Get the final dataset
dt_train_prosd_pca <- do.call(cbind, final)
# Bind with the categorical variables
dt_train_prosd_pca <- cbind(dt_train_prosd[1:5], dt_train_prosd_pca)
# Bind with the class variable
dt_train_prosd_pca <- cbind(dt_train_prosd_pca, dt_train_prosd[, 27])

# Save the dataset
write.table(
  dt_train_prosd_pca,
  file = "Data/dt_train_processed_pca.txt",
  append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE
)

head(dt_train_prosd_pca)


# PCA on PROCESSED Test Dataset
# -----------------------------
dt_test_prosd <- read.table("Data/dt_test_processed.txt")
head(dt_test_prosd)

# Convert categorical columns to factors
dt_test_prosd[, c(1:5, 27)] <- lapply(dt_test_prosd[c(1:5, 27)], as.factor)

# Apply standardized PCA to the continuous variables
pca_result <- prcomp(dt_test_prosd[, 6:26], scale = TRUE)

# Picking Number of components from PCA
eigen_values <- get_eigenvalues(pca_result)
eigen_values

variance_explained <- get_variance_explained(eigen_values)
variance_explained # let's pick 9 components

# Lets keep 9 components
combinations <- pca_result$rotation[, 1:9]

# Lets ignore the categorical variables when creating the new dataset
dt_test_prosd_pca <- copy(dt_test_prosd[6:26])

# Define number of examples and components
n_numerical_features <- 21
n_examples <- 12000
n_components <- 9

final <- get_principal_components(
  dt_test_prosd_pca, combinations,
  n_components, n_examples, n_numerical_features
)

# Get the final dataset
dt_test_prosd_pca <- do.call(cbind, final)
# Bind with the categorical variables
dt_test_prosd_pca <- cbind(dt_test_prosd[1:5], dt_test_prosd_pca)
# Bind with the class variable
dt_test_prosd_pca <- cbind(dt_test_prosd_pca, dt_test_prosd[, 27])

# Save the dataset
write.table(
  dt_test_prosd_pca,
  file = "Data/dt_test_processed_pca.txt",
  append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE
)

head(dt_test_prosd_pca)
