# Load required libraries
library(xgboost)
library(randomForest)
library(rpart)
library(dplyr)

# Load the dataset
data <- read.csv("C:/Users/Arnab Chakravarty/Documents/UB-CourseWork/Semester1/IE_500(DAPM)/Project/Take2/data.csv")

# Display the first few rows of the dataset
head(data)

# Check for missing values
missing_values <- colSums(is.na(data))
cat("Missing values:\n", missing_values)

# Split data into train, validation, and test sets
set.seed(123)
n <- nrow(data)
train_indices <- sample(1:n, 0.7 * n)
validation_test_indices <- setdiff(1:n, train_indices)
validation_indices <- sample(validation_test_indices, 0.2 * n)
test_indices <- setdiff(validation_test_indices, validation_indices)

train_data <- data[train_indices, ]
validation_data <- data[validation_indices, ]
test_data <- data[test_indices, ]

# Prepare data for modeling
features <- c("thal", "ca", "oldpeak", "exang", "thalach")
X_train <- train_data[, features]
y_train <- train_data$num

X_validation <- validation_data[, features]
y_validation <- validation_data$num

X_test <- test_data[, features]
y_test <- test_data$num

# XGBoost model
xgb_model <- xgboost(data = as.matrix(X_train), label = y_train, nrounds = 100, objective = "binary:logistic")
xgb_pred <- predict(xgb_model, as.matrix(X_validation))
xgb_accuracy <- sum(round(xgb_pred) == y_validation) / length(y_validation)
cat("XGBoost Accuracy:", xgb_accuracy, "\n")

# CART model
cart_model <- rpart(num ~ ., data = train_data[, c(features, "num")], method = "class")
cart_pred <- predict(cart_model, validation_data[, c(features, "num")], type = "class")
cart_accuracy <- sum(cart_pred == validation_data$num) / nrow(validation_data)
cat("CART Accuracy:", cart_accuracy, "\n")

# Random Forest model
train_data <- na.omit(train_data)




# Random Forest model
rf_model <- randomForest(factor(num) ~ ., data = train_data[, c(features, "num")], ntree = 100)
rf_pred <- predict(rf_model, validation_data[, c(features, "num")])
unique(rf_pred)
unique(validation_data$num)
rf_pred <- as.numeric(as.character(rf_pred))
rf_accuracy <- sum(rf_pred == validation_data$num) / nrow(validation_data)
cat("Random Forest Accuracy:", rf_accuracy, "\n")

# Check unique values in rf_pred and validation_data$num
cat("Unique values in rf_pred:", unique(rf_pred), "\n")
cat("Unique values in validation_data$num:", unique(validation_data$num), "\n")

# Print first few elements
cat("First few elements of rf_pred:", head(rf_pred), "\n")
cat("First few elements of validation_data$num:", head(validation_data$num), "\n")
# Check for missing values in rf_pred
cat("Missing values in rf_pred:", sum(is.na(rf_pred)), "\n")


# Remove rows with missing predictions
non_missing_indices <- !is.na(rf_pred)
rf_pred <- rf_pred[non_missing_indices]
validation_data <- validation_data[non_missing_indices, ]

# Random Forest Accuracy
rf_accuracy <- sum(rf_pred == validation_data$num) / length(validation_data$num)
cat("Random Forest Accuracy:", rf_accuracy, "\n")








# Load required library
library(e1071)

# SVM model

# SVM model
svm_model <- svm(num ~ ., data = train_data[, c(features, "num")], kernel = "linear")
svm_pred <- predict(svm_model, newdata = validation_data[, c(features, "num")])



svm_pred <- ifelse(svm_pred < 0.5, 0, 1)



# Convert to character vectors
svm_pred <- as.character(svm_pred)
validation_data$num <- as.character(validation_data$num)

# SVM Accuracy
svm_accuracy <- sum(svm_pred == validation_data$num, na.rm = TRUE) / length(validation_data$num)
cat("SVM Accuracy:", svm_accuracy, "\n")

# Print unique values in validation_data$num
cat("Unique values in validation_data$num:", validation_data$num, "\n")


# Check lengths
cat("Length of svm_pred:", length(svm_pred), "\n")
cat("Number of rows in validation_data:", nrow(validation_data), "\n")

# Check unique values in svm_pred and validation_data$num
cat("Unique values in svm_pred:", unique(svm_pred), "\n")
cat("Unique values in validation_data$num:", unique(validation_data$num), "\n")

# Print first few elements
cat("First few elements of svm_pred:", head(svm_pred), "\n")
cat("First few elements of validation_data$num:", head(validation_data$num), "\n")

# SVM Accuracy
svm_accuracy <- sum(svm_pred == validation_data$num, na.rm = TRUE) / length(validation_data$num)
cat("SVM Accuracy:", svm_accuracy, "\n")









