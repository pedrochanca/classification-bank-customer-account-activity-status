# Libraries
# ---------
library("ggplot2")
library("DataExplorer")
library("e1071")
library("corrplot")
library("RColorBrewer")
library("tidyinftheo")
library("muti")
library("mRMRe")

# Load Train Data
# ---------------
dt_train <- read.table("Data/dt_train_original_extra.txt")
dt_test <- read.table("Data/dt_test_original.txt")

# Checking the data
head(dt_train)
head(dt_test)

# Descriptive Statistics
# ----------------------
v <- sapply(dt_train[, 7:36], var)
m <- sapply(dt_train[, 7:36], mean)
q <- sapply(dt_train[, 7:36], quantile)
m0 <- sapply(dt_train[, 7:36], min)
m1 <- sapply(dt_train[, 7:36], max)
i <- sapply(dt_train[, 7:36], IQR)
s <- sapply(dt_train[, 7:36], skewness)
k <- sapply(dt_train[, 7:36], kurtosis)

descriptive <- data.frame(
  "mean" = m, "variance" = v, "minimum" = m0, "Q1" = q[2, ], "median" = q[2, ],
  "Q3" = q[3, ], "max" = m1, "IQR" = i, "skewness" = s, "kurtosis" = k
)

show <- TRUE
if (show) {
  descriptive
} else {
  write.csv(descriptive, file = "Data/descriptive.csv")
}

# Trying some transformations on low variance columns
# to identify why it has a low variance
var(log(dt_train[, 22]) + 1)


# Plotting histograms of each variable, and signaling the class for each data
# ---------------------------------------------------------------------------
feature_index <- 10
class1 <- data.frame(v = dt_train[dt_train[51] == "0", feature_index])
class0 <- data.frame(v = dt_train[dt_train[51] == "1", feature_index])
class1$Class <- "Class 1"
class0$Class <- "Class 0"
class_counts <- rbind(class1, class0)

# Continuous
ggplot(class_counts, aes(v, fill = Class)) +
  labs(x = paste("V", feature_index, sep = "")) +
  geom_histogram(bins = 50) +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )
# Discrete
ggplot(class_counts, aes(v, fill = Class)) +
  labs(x = paste("V", feature_index, sep = "")) +
  geom_bar() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )


# Compute Correlations + Plot Correlogram
# ---------------------------------------
corvar <- cor(dt_train[7:50])
corrplot(
  corvar,
  method = "color", col = brewer.pal(n = 8, name = "RdBu"), type = "lower",
  tl.col = "black", addCoef.col = "black", tl.srt = 15, tl.cex = 0.6,
  number.cex = 0.7
)


# Transform into an mRMR Dataset
# ------------------------------
dt_train_mrmr <- copy(dt_train)

for (i in c(1:6, 51)) {
  dt_train_mrmr[, i] <- as.ordered(dt_train_mrmr[, i])
}

dt_train_mrmr <- mRMR.data(dt_train_mrmr)

# Apply mRMR Algorithm to select features
feat_sel <- mRMR.classic(
  data = dt_train_mrmr, target_indices = 51, feature_count = 50
)
feat_sel@filters
feat_sel@scores
solutions(feat_sel)

feat_sel


# Processed Train Dataset after EDA
# ---------------------------------
dt_train_processed <- read.table("Data/dt_train_processed.txt")
dt_test_processed <- read.table("Data/dt_test_processed.txt")

head(dt_train_processed)
head(dt_test_processed)

corvar <- cor(dt_train_processed[6:26])
corrplot(
  corvar,
  method = "color", col = brewer.pal(n = 8, name = "RdBu"), type = "lower",
  tl.col = "black", addCoef.col = "black", tl.srt = 15, tl.cex = 0.6,
  number.cex = 0.7
)
