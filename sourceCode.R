# Load data (replace "?" with NA)
ad_data <- read.csv("~/HCMUT/242/XSTK/Assignment/dataset/add.csv", na.strings = c("?"))

# Part 1: Clean the data

# Convert X0, X1, X2 from character to numeric
ad_data$X0 <- as.numeric(ad_data$X0)
ad_data$X1 <- as.numeric(ad_data$X1)
ad_data$X2 <- as.numeric(ad_data$X2)

# Select the columns
cols_to_show <- c(1:10, ncol(ad_data))
# Display first and last 10 rows
rbind(head(ad_data[, cols_to_show], 10),
      tail(ad_data[, cols_to_show], 10))
# Count number of NA
colSums(is.na(ad_data[, cols_to_show]))
#library(questionr)
#freq.na(ad_data)

# Handle missing data by imputation
ad_data$X0[is.na(ad_data$X0)] <- median(ad_data$X0, na.rm = TRUE)
ad_data$X1[is.na(ad_data$X1)] <- median(ad_data$X1, na.rm = TRUE)
ad_data$X2[is.na(ad_data$X2)] <- median(ad_data$X2, na.rm = TRUE)
# Remove 15 observations with NA in x3
ad_data <- ad_data[complete.cases(ad_data), ]

# Convert x1558 to binary
ad_data$X1558 <- trimws(ad_data$X1558)  # Remove whitespace (leading or trailing)
ad_data$X1558 <- gsub("\\.$", "", ad_data$X1558)  # Remove dot ad. nonad.
ad_data$X1558_bin <- ifelse(ad_data$X1558 == "ad", 1, 0)

# Part 2: Descriptive statistic
# Select numeric variables (excluding the binary target)
numeric_vars <- sapply(ad_data, is.numeric)
numeric_data <- ad_data[, numeric_vars & !names(ad_data) %in% c("X1558_bin")]

# Calculate summary statistics with more metrics
summary_stats <- sapply(numeric_data, function(x) {
  c(Mean = mean(x),
    SD = sd(x),
    Variance = var(x),
    Min = min(x),
    Q1 = quantile(x, 0.25),
    Median = median(x),
    Q3 = quantile(x, 0.75),
    Max = max(x),
    IQR = IQR(x),
    Skewness = moments::skewness(x),
    Kurtosis = moments::kurtosis(x),
    NAs = sum(is.na(x)))
})
# Transpose and round for better readability
stats_table <- t(as.data.frame(summary_stats))
round(stats_table, 2)
