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
  folder <- "Output/"
  file <- "descriptive.csv"
  suppressWarnings(dir.create(folder))
  write.csv(descriptive, file = paste(path, file, sep = ""))
}

# Trying some transformations on low variance columns
# to indetify why it has a low variance
var(log(dt_train[, 22]) + 1)


# Plotting histograms of each variable, and signaling the class for each data
i <- 36
class1 <- data.frame(v = dt_train[dt_train[37] == "0", i])
class0 <- data.frame(v = dt_train[dt_train[37] == "1", i])
class1$Class <- "Class 1"
class0$Class <- "Class 0"
class_counts <- rbind(class1, class0)
# Continuous
ggplot(class_counts, aes(v, fill = Class)) +
  labs(x = paste("V", i, sep = "")) +
  geom_histogram(bins = 50) +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )
# Discrete
ggplot(class_counts, aes(v, fill = Class)) +
  labs(x = paste("V", i, sep = "")) +
  geom_bar() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )


# Computingd the correlations and plotting the correlogram
corvar <- cor(dt_train[7:50])
corrplot(corvar,
  method = "color", col = brewer.pal(n = 8, name = "RdBu"), type = "lower",
  tl.col = "black", addCoef.col = "black", tl.srt = 15, tl.cex = 0.6,
  number.cex = 0.7
)

# Computing the mutual information using python
m_dt_train <- as.matrix(dt_train, dimnames = NULL)
mut_inf <- sklearn1$mutual_info_classif(m_dt_train, m_dt_train[, 37])

# If we only want to source some script of python
# py_run_file("add.py") # nolint

dt_train[, 1] <- as.ordered(dt_train[, 1])
dt_train[, 2] <- as.ordered(dt_train[, 2])
dt_train[, 3] <- as.ordered(dt_train[, 3])
dt_train[, 4] <- as.ordered(dt_train[, 4])
dt_train[, 5] <- as.ordered(dt_train[, 5])
dt_train[, 6] <- as.ordered(dt_train[, 6])
dt_train[, 51] <- as.ordered(dt_train[, 51])


# Transform into an MRMR Dataset
train_mrmr <- mRMR.data(dt_train)

# MRMR Algorithm
feat_sel <- mRMR.classic(
  data = train_mrmr, target_indices = 51, feature_count = 50
)
feat_sel@filters
# feat_sel@scores
# solutions(feat_sel)

index_columns <- c(1:2, 4:10, 14:21, 24:25, 27:28, 30, 32, 36, 39, 43, 51)
dt_train_removed <- dt_train[, index_columns]
write.table(dt_train_removed,
  file = "train_data_removed", append = FALSE, sep = " ", dec = ".",
  row.names = FALSE, col.names = FALSE
)

corvar <- cor(train_data_removed[6:26])
corrplot(corvar,
  method = "color", col = brewer.pal(n = 8, name = "RdBu"), type = "lower", tl.col = "black",
  addCoef.col = "black", tl.srt = 15, tl.cex = 0.6, number.cex = 0.7
)
