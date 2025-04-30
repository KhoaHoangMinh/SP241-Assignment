# Load data (replace "?" with NA)
ad_data <- read.csv("~/HCMUT/242/XSTK/Assignment/dataset/add.csv", na.strings = c("?"))

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

# Check if missingness is correlated between variables
#cor(is.na(ad_data[, c("X0", "X1", "X2")]))

# Handle missing data by imputation
#ad_data$X0[is.na(ad_data$X0)] <- median(ad_data$X0, na.rm = TRUE)
#ad_data$X1[is.na(ad_data$X1)] <- median(ad_data$X1, na.rm = TRUE)
#ad_data$X2[is.na(ad_data$X2)] <- median(ad_data$X2, na.rm = TRUE)

#colSums(is.na(ad_data[, cols_to_show]))

