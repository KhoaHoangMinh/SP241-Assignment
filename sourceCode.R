# Load data (replace "?" with NA)
ad_data <- read.csv("~/HCMUT/242/XSTK/Assignment/dataset/add.csv")

# Select the columns
cols_to_show <- c(1:15, ncol(ad_data))
# Display first and last 10 rows
rbind(head(ad_data[, cols_to_show], 10),
      tail(ad_data[, cols_to_show], 10))

# Part 1: Clean the data

# Remove the first column (No.)
ad_data <- ad_data[, -1]

# Remove col 4 to 1558
ad_data <- ad_data[, -c(4:1558)]

# Convert X0, X1, X2 from character to numeric
ad_data$X0 <- as.numeric(ad_data$X0)
ad_data$X1 <- as.numeric(ad_data$X1)
ad_data$X2 <- as.numeric(ad_data$X2)

# Rename columns
colnames(ad_data)[1] <- "height"
colnames(ad_data)[2] <- "width"
colnames(ad_data)[3] <- "ratio"
colnames(ad_data)[4] <- "target"

# Convert target to binary
ad_data$target <- ifelse(ad_data$target == "ad.", 1, 0)

# Display first and last 10 rows
rbind(head(ad_data, 10),
      tail(ad_data, 10))

# Count number of NA
na_counts <- colSums(is.na(ad_data))
# Percentage of NA for every col
na_percentage <- colMeans(is.na(ad_data)) * 100
na_summary <- data.frame(
  NA_Count = na_counts,
  NA_Percent = round(na_percentage, 2)
)
print(na_summary)

# Handle missing data by imputation
# Replace missing values with median
ad_data$height[is.na(ad_data$height)] <- median(ad_data$height, na.rm = TRUE)
ad_data$width[is.na(ad_data$width)] <- median(ad_data$width, na.rm = TRUE)
ad_data$ratio[is.na(ad_data$ratio)] <- median(ad_data$ratio, na.rm = TRUE)

# Recalculate Ratio 
update_ratio <- function(df) {
  df$ratio[is.na(df$ratio) & !is.na(df$height) & !is.na(df$width)] <- df$height[is.na(df$ratio) & !is.na(df$height) & !is.na(df$width)] / df$width[is.na(df$ratio) & !is.na(df$height) & !is.na(df$width)]
  return(df)
}

# Use update_ratio to replace missing values
ad_data <- update_ratio(ad_data)

# Part 2: Descriptive statistic
# Select numeric variables (excluding the binary target)
numeric_data <- ad_data[, c(1, 2, 3)]

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
    IQR = IQR(x))
})
# Transpose and round for better readability
stats_table <- t(as.data.frame(summary_stats))
round(stats_table, 2)

# Load necessary library
library(gridExtra)

# Create a PNG file to save the table
png("C:/Users/hoang/Documents/HCMUT/242/XSTK/Assignment/datasettarget_table.png", width = 725, height = 150)

# Render the table
grid.table(stats_table)

# Close the PNG device to save the file
dev.off()
# Number of targets
table(ad_data$target)

# Load required libraries
library(ggplot2)

# Part 3: Visualization

# Histogram for height
ggplot(ad_data, aes(x = height)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Height", x = "Height", y = "Frequency") +
  theme_minimal()

# Histogram for width
ggplot(ad_data, aes(x = width)) +
  geom_histogram(binwidth = 10, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Width", x = "Width", y = "Frequency") +
  theme_minimal()

# Histogram for ratio
ggplot(ad_data, aes(x = ratio)) +
  geom_histogram(binwidth = 0.5, fill = "purple", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Ratio", x = "Ratio", y = "Frequency") +
  theme_minimal()

# Barplot for target
ggplot(ad_data, aes(x = factor(target), fill = factor(target))) +
  geom_bar() +
  scale_fill_manual(values = c("blue", "orange")) +
  labs(title = "Barplot for Target", x = "Target (1=Ad, 0=Non-Ad)", y = "Count") +
  theme_minimal()

# Boxplot for height by target
ggplot(ad_data, aes(x = factor(target), y = height, fill = factor(target))) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue", "orange")) +
  labs(title = "Boxplot of Height by Target", x = "Target", y = "Height") +
  theme_minimal()

# Boxplot for width by target
ggplot(ad_data, aes(x = factor(target), y = width, fill = factor(target))) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue", "orange")) +
  labs(title = "Boxplot of Width by Target", x = "Target", y = "Width") +
  theme_minimal()

# Boxplot for ratio by target
ggplot(ad_data, aes(x = factor(target), y = ratio, fill = factor(target))) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue", "orange")) +
  labs(title = "Boxplot of Ratio by Target", x = "Target", y = "Ratio") +
  theme_minimal()

# Set up 3 plots in a column layout
par(mfrow = c(3, 1), mar = c(5, 5, 4, 2))

# Strip Plot: Height vs Ad/NoAd
stripchart(height ~ target,
           data = ad_data,
           horizontal = TRUE,
           method = "jitter",
           pch = 19,
           col = rgb(0.2, 0.4, 0.8, 0.5),  # Semi-transparent blue
           main = "Height Distribution by Ad Status",
           ylab = "Ad Status (0 = Non-Ad, 1 = Ad)",
           xlab = "Height")

# Strip Plot: Width vs Ad/NoAd
stripchart(width ~ target,
           data = ad_data,
           horizontal = TRUE,
           method = "jitter",
           pch = 19,
           col = rgb(0.1, 0.7, 0.1, 0.5),  # Semi-transparent green
           main = "Width Distribution by Ad Status",
           ylab = "Ad Status (0 = Non-Ad, 1 = Ad)",
           xlab = "Width")

# Strip Plot: Ratio vs Ad/NoAd
stripchart(ratio ~ target,
           data = ad_data,
           horizontal = TRUE,
           method = "jitter",
           pch = 19,
           col = rgb(0.9, 0.2, 0.2, 0.5),  # Semi-transparent red
           main = "Aspect Ratio Distribution by Ad Status",
           ylab = "Ad Status (0 = Non-Ad, 1 = Ad)",
           xlab = "Aspect Ratio")

# Reset layout
par(mfrow = c(1, 1))

# Select the relevant columns
pair_data <- ad_data[, c("height", "width", "ratio", "target")]

# Convert target to factor for color grouping
pair_data$target <- as.factor(pair_data$target)

# Create pairwise plot
ggpairs(pair_data,
        aes(color = target, alpha = 0.6),
        upper = list(continuous = "points"),
        diag = list(continuous = "densityDiag"),
        lower = list(continuous = "smooth")) +
  theme_minimal()

# Part 4: Logistic regression model

# Logistic regression model for height
model_height <- glm(target ~ height, data = ad_data, family = "binomial")

# Draw distribution chart between 'height' and 'target'
# then draw logistic curve
plot(ad_data$height, ad_data$target,
     xlab = "Height", ylab = "Target",
     main = "Logistic Regression: Target vs Height",
     pch = 16, col = rgb(0, 0, 1, 0.3))  # Distribution chart

curve(predict(model_height, newdata = data.frame(height = x), type = "response"),
      add = TRUE, col = "red", lwd = 2)  # logistic curve

# Logistic regression model for width
model_width <- glm(target ~ width, data = ad_data, family = "binomial")

plot(ad_data$width, ad_data$target,
     xlab = "Width", ylab = "Target",
     main = "Logistic Regression: Target vs Width",
     pch = 16, col = rgb(0, 0, 1, 0.3)) 

curve(predict(model_width, newdata = data.frame(width = x), type = "response"),
      add = TRUE, col = "red", lwd = 2)

# Logistic regression model for ratio
model_ratio <- glm(target ~ ratio, data = ad_data, family = "binomial")

plot(ad_data$ratio, ad_data$target,
     xlab = "Ratio", ylab = "Target",
     main = "Logistic Regression: Target vs Ratio",
     pch = 16, col = rgb(0.7, 0, 0, 0.3))

curve(predict(model_ratio, newdata = data.frame(ratio = x), type = "response"),
      add = TRUE, col = "red", lwd = 2)

# Calculate the correlation matrix between the continuous variables 'height', 'width', 'ratio'
library(corrplot)
cor_matrix <- cor(ad_data[, c("height", "width", "ratio")], use = "complete.obs")

# Draw the correlation matrix plot
corrplot(cor_matrix, method = "circle", type = "upper",
         addCoef.col = "black", tl.col = "black", number.cex = 0.8,
         title = "Correlation Matrix", mar = c(0,0,1,0))

# Part 5: Inferential Statistics
# Reset seed
set.seed(111)

# Divide the dataset into training set and testing set
# 70% - 30%
sample_index <- sample(1:nrow(ad_data), size = 0.7 * nrow(ad_data))
train_data <- ad_data[sample_index, ]
test_data <- ad_data[-sample_index, ]

# Logistic regression model with 'Height' and 'Width' 
model_hw <- glm(target ~ height + width, data = train_data, family = binomial)
summary(model_hw)

# Logistic regression model with only 'Width'
model_hw2 <- glm(target ~ width, data = train_data, family = binomial)
summary(model_hw2)

# Logistic regression model with 'ratio'
model_ratio <- glm(target ~ ratio, data = train_data, family = binomial)
summary(model_ratio)

# Function to evaluate the model
evaluate_model <- function(model, data, label) {
  pred_prob <- predict(model, newdata = data, type = "response")
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  
  actual <- data$target
  accuracy <- mean(pred_class == actual)
  
  cat(paste0(label, ":\n"))
  cat("Accuracy: ", round(accuracy, 4), "\n")
  cat("Confusion Matrix:\n")
  print(table(Predicted = pred_class, Actual = actual))
  cat("\n")
}

# Evaluate the model
evaluate_model(model_hw2, test_data, "Model 1 (width)")
evaluate_model(model_ratio, test_data, "Model 2 (ratio)")

# Print the AIC of models
cat("AIC Model 1 ( width): ", AIC(model_hw2), "\n")
cat("AIC Model 2 (ratio): ", AIC(model_ratio), "\n")

library(pROC)
# Prediction on the test dataset
prob_hw <- predict(model_hw2, newdata = test_data, type = "response")
prob_ratio <- predict(model_ratio, newdata = test_data, type = "response")

# Plot ROC
roc_hw <- roc(test_data$target, prob_hw)
roc_ratio <- roc(test_data$target, prob_ratio)

# Plot comparison chart
plot(roc_hw, col = "blue", lwd = 2, main = "ROC Curve Comparison")
lines(roc_ratio, col = "red", lwd = 2)
legend("bottomright", legend = c("Model 1: width", "Model 2: ratio"),
       col = c("blue", "red"), lwd = 2)

# Print AUC
cat("AUC Model 1 (width): ", auc(roc_hw), "\n")
cat("AUC Model 2 (ratio): ", auc(roc_ratio), "\n")
