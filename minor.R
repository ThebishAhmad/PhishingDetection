# ------------------------------------------------------------------
# PHISHING DETECTION USING MACHINE LEARNING - DATA PREPROCESSING ONLY
# ------------------------------------------------------------------

# ------------------------------------------------------------
# INSTALL REQUIRED LIBRARIES FOR DATA PREPROCESSING
# ------------------------------------------------------------

install_if_missing <- function(packages) {
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}

# Required libraries (only for preprocessing)
packages <- c(
  "dplyr",        # data wrangling
  "ggplot2",      # visualization
  "VIM",          # missing data visualization
  "caret",        # data partition
  "tidyr",        # for gather()
  "gridExtra"     # arranging multiple plots
)

install_if_missing(packages)

# ------------------------------------------------------------
# 1. Data Import
# ------------------------------------------------------------
train <- read.csv("C:/Users/uv488/Downloads/train.csv")
test <- read.csv("C:/Users/uv488/Downloads/test.csv")

# ------------------------------------------------------------
# 2. Binary Variable Transformation
# ------------------------------------------------------------
binary_vars <- sapply(train, function(x) all(x %in% c(0, 1)))
train[binary_vars] <- lapply(train[binary_vars], factor, levels = c(0, 1), labels = c(0, 1))

binary_vars_test <- sapply(test, function(x) all(x %in% c(0, 1)))
test[binary_vars_test] <- lapply(test[binary_vars_test], factor, levels = c(0, 1), labels = c(0, 1))

# Convert target variable to factor (if present)
if ("status" %in% names(train)) {
  train$status <- factor(train$status, levels = c("phishing", "legitimate"))
}
if ("status" %in% names(test)) {
  test$status <- factor(test$status, levels = c("phishing", "legitimate"))
}

# ------------------------------------------------------------
# 3. Train/Test Split Structure
# ------------------------------------------------------------
if ("status" %in% names(test)) {
  Trainindex <- createDataPartition(y = test$status, p = .95, list = FALSE)
  test <- test[Trainindex,]
  score <- test[-Trainindex,]
} else {
  score <- test
}

# ------------------------------------------------------------
# 4. Check Class Distribution
# ------------------------------------------------------------
if ("status" %in% names(train)) {
  print(table(train$status))
  print(prop.table(table(train$status)))
}

# ------------------------------------------------------------
# 5. Missing Data Check
# ------------------------------------------------------------
# Missing data summary for train
missing_data_train <- train %>%
  summarise_all(~ sum(is.na(.) | . == "")) %>%
  gather(variable, missing_count)
print(missing_data_train)

# Visualize missingness (Train)
aggr(train, col = c('navyblue', 'yellow'),
     numbers = TRUE, sortVars = TRUE,
     cex.axis = .7, gap = 2,
     ylab = c("Histogram of missing data", "Pattern"))

# Missing data summary for test
missing_data_test <- test %>%
  summarise_all(~ sum(is.na(.) | . == "")) %>%
  gather(variable, missing_count)
print(missing_data_test)

# Visualize missingness (Test)
aggr(test, col = c('navyblue', 'yellow'),
     numbers = TRUE, sortVars = TRUE,
     cex.axis = .7, gap = 2,
     ylab = c("Histogram of missing data", "Pattern"))

# ------------------------------------------------------------
# 6. Basic Visualization
# ------------------------------------------------------------
if ("status" %in% names(train)) {
  plot_gg <- function(column) {
    ggplot(data = train, aes(x = {{column}}, fill = status)) +
      geom_bar(position = 'dodge') +
      scale_fill_manual('Legend', values = c("lightblue", "blue")) +
      theme_minimal()
  }
  print(plot_gg(status) + ggtitle("Distribution of Phishing and Legitimate Websites"))
}

# ------------------------------------------------------------
# 7. Remove Problematic Variables (only if they exist)
# ------------------------------------------------------------
cols_to_remove <- c(
  "statistical_report", "ratio_nullHyperlinks", "nb_or", "ratio_intErrors",
  "ratio_intRedirection", "submit_email", "sfh", "url"
)
cols_to_remove <- intersect(cols_to_remove, names(train))
if (length(cols_to_remove) > 0) {
  train <- subset(train, select = -all_of(cols_to_remove))
}

if ("status" %in% names(score)) {
  score <- subset(score, select = -c(status))
}

# ------------------------------------------------------------
# 8. Final Clean Dataset Summary
# ------------------------------------------------------------
cat("\n✅ Structure of Cleaned Train Data:\n")
str(train)
cat("\n✅ Summary of Train Data:\n")
print(summary(train))

# ------------------------------------------------------------
# 9. Save Preprocessed Data
# ------------------------------------------------------------
write.csv(train, "C:/Users/uv488/Downloads/preprocessed_train.csv", row.names = FALSE)
write.csv(test, "C:/Users/uv488/Downloads/preprocessed_test.csv", row.names = FALSE)

cat("\n✅ Data preprocessing complete! Clean datasets saved.\n")
