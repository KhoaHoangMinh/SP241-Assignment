# Load data (replace "?" with NA)
ad_data <- read.csv("~/HCMUT/242/XSTK/Assignment/dataset/add.csv", na.strings = c("?"))

# Convert X0, X1, X2 from character to numeric
#ad_data$X0 <- as.numeric(ad_data$X0)
#ad_data$X1 <- as.numeric(ad_data$X1)
#ad_data$X2 <- as.numeric(ad_data$X2)
ad_data$X0 <- suppressWarnings(as.numeric(ad_data$X0))
ad_data$X1 <- suppressWarnings(as.numeric(ad_data$X1))
ad_data$X2 <- suppressWarnings(as.numeric(ad_data$X2))

# Select the columns
cols_to_show <- c(1:10, ncol(ad_data))
# Display first and last 10 rows
rbind(head(ad_data[, cols_to_show], 10),
      tail(ad_data[, cols_to_show], 10))
# Count number of NA
colSums(is.na(ad_data[, cols_to_show]))