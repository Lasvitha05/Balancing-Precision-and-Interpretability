install.packages("rpart.plot")


# Load necessary libraries
library(tidyverse)
library(caret)

# Load the dataset
data <- read.csv("C:/Users/Arnab Chakravarty/Documents/UB-CourseWork/Semester1/IE_500(DAPM)/Project/Take2/data.csv")

# Display the first few rows of the dataset
head(data)

# Check for missing values
missing_values <- colSums(is.na(data))
print(missing_values)

# Handling missing values by removing rows with missing values
data <- na.omit(data)

# Display the first few rows after handling missing values
head(data)

# Perform one-hot encoding for the 'num' column
# Convert 'num' to a factor with levels '0' and '1'
data$num <- as.factor(data$num)

# Convert 'num' to a binary matrix (one-hot encoding)
encoded_num <- model.matrix(~num - 1, data = data)

# Assign the one-hot encoded columns to the dataset
data <- cbind(data, encoded_num)

# Display the first few rows after one-hot encoding
head(data)

data$num_binary <- as.numeric(data$num) - 1

# Split the dataset into train (70%), validation (20%), and test (10%) sets
set.seed(123)  # Set seed for reproducibility
train_index <- sample(seq_len(nrow(data)), size = 0.7 * nrow(data))
train_data <- data[train_index, ]
val_test_data <- data[-train_index, ]

# Further split the remaining data into validation (50%) and test (50%) sets
set.seed(456)  # Set another seed for reproducibility
val_index <- sample(seq_len(nrow(val_test_data)), size = 0.5 * nrow(val_test_data))
val_data <- val_test_data[val_index, ]
test_data <- val_test_data[-val_index, ]

# Display the number of observations in each set
cat("Number of observations in the training set:", nrow(train_data), "\n")
cat("Number of observations in the validation set:", nrow(val_data), "\n")
cat("Number of observations in the test set:", nrow(test_data), "\n")


# Train Logistic Regression model
logreg_model <- glm(num ~ thal + ca + oldpeak + exang + thalach, data = train_data, family = "binomial", maxit = 1000)

# Make predictions on the validation set
predictions <- predict(logreg_model, newdata = val_data, type = "response")


# Ensure 'num' in the validation dataset has the same levels as in the training dataset
predicted_class <- factor(val_data$num, levels = levels(train_data$num))

# Evaluate performance metrics using confusion matrix
conf_matrix <- confusionMatrix(predicted_class, val_data$num)
print(conf_matrix)

# Display the confusion matrix plot
plot(conf_matrix$table, col = conf_matrix$byClass, 
     main = paste("Confusion Matrix - Accuracy:", round(conf_matrix$overall["Accuracy"], 3)))







##LOGISRICS REGRESSION



# Import the necessary libraries
library(dplyr)
library(caret)

# Load the dataset
data <- read.csv("C:/Users/Arnab Chakravarty/Documents/UB-CourseWork/Semester1/IE_500(DAPM)/Project/Take2/data.csv")

# Display the first few rows of the dataset
head(data)

# Check for missing values
missing_values <- colSums(is.na(data))
print(missing_values)

# Handling missing values by removing rows with missing values
data <- na.omit(data)

# Display the first few rows after handling missing values
head(data)

# Perform one-hot encoding for the 'num' column
# Convert 'num' to a factor with levels '0' and '1'
data$num <- as.factor(data$num)

# Convert 'num' to a binary matrix (one-hot encoding)
encoded_num <- model.matrix(~num - 1, data = data)

# Assign the one-hot encoded columns to the dataset
data <- cbind(data, encoded_num)

# Display the first few rows after one-hot encoding
head(data)

data$num_binary <- as.numeric(data$num) - 1



index <- createDataPartition(data$num, p = 0.7, list = FALSE)
train_data <- data[index, ]
temp_data <- data[-index, ]

index <- createDataPartition(temp_data$num, p = 0.67, list = FALSE)
val_data <- temp_data[index, ]
test_data <- temp_data[-index, ]

# Check the dimensions of the datasets
dim(train_data)
dim(val_data)
dim(test_data)

# Build logistics regression model
# Use only "thal", "ca", "oldpeak", "exang", "thalach" columns for training the model
logreg_model <- glm(num ~ thal + ca + oldpeak + exang + thalach, data = train_data, family = "binomial")

# Ensure 'num' in the validation dataset has the same levels as in the training dataset
val_data$num <- factor(val_data$num, levels = levels(train_data$num))

# Predict on the validation set
val_predictions <- predict(logreg_model, newdata = val_data, type = "response")

# Convert predicted probabilities to 0 or 1 using a threshold (e.g., 0.5)
library(prediction)

# Predict on the validation set
# Ensure 'num' in the validation dataset has the same levels as in the training dataset
val_data$num <- factor(val_data$num, levels = levels(train_data$num))

# Predict on the validation set
val_predictions <- predict(logreg_model, newdata = val_data, type = "response")

# Evaluate the model performance on the validation set
conf_matrix <- confusionMatrix(factor(ifelse(val_predictions > 0.5, 1, 0), levels = levels(train_data$num)), val_data$num)
conf_matrix

# Display accuracy and other relevant metrics
conf_matrix$overall


##Tree

library(dplyr)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)

heart_data <- data
set.seed(123)  # Set seed for reproducibility
split <- sample.split(heart_data$num, SplitRatio = 0.7)
train_data <- subset(heart_data, split == TRUE)
temp_data <- subset(heart_data, split == FALSE)
split_val_test <- sample.split(temp_data$num, SplitRatio = 0.5)
val_data <- subset(temp_data, split_val_test == TRUE)
test_data <- subset(temp_data, split_val_test == FALSE)

# Build a Decision Tree model
# Use only selected features: "thal", "ca", "oldpeak", "exang", "thalach"
tree_model <- rpart(num ~ thal + ca + oldpeak + exang + thalach, data = train_data, method = "class")

# Visualize the Decision Tree
rpart.plot(tree_model, type = 0, extra = 101, under = TRUE, faclen = 0)

# Convert num column to factor with levels 0 and 1
val_data$num <- as.factor(val_data$num)
# Predict on the validation set
val_predictions <- predict(tree_model, newdata = val_data, type = "class")

# Evaluate the Decision Tree model performance on the validation set
conf_matrix_tree <- confusionMatrix(val_predictions, val_data$num)
conf_matrix_tree


# Evaluate the Decision Tree model performance on the validation set
conf_matrix_tree <- confusionMatrix(val_predictions, val_data$num)
conf_matrix_tree

# Display accuracy and other relevant metrics
conf_matrix_tree$overall






















