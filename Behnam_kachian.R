#install packages
install.packages(c("corrplot","rpart","ipred","rpart.plot","gbm","gbm3","caret","ggplot2","dplyr","GGally","gridExtra","randomForest","ggplot","tree"))

# Data Loading
train <- read.csv("E:/cw2/creditdefault_train.csv")
test <- read.csv("E:/cw2/creditdefault_test.csv")

#Data cleaning
head(train)
dim(train)
names(train)
dim(test)
names(test)
summary(train)

# Check for missing values in each column and print the number of missing values
missing_values <- colSums(is.na(train))
print(missing_values)

missing_values <- sum(is.na(train))
cat("Total Missing Values:", missing_values, "\n")

#Visual Data Exploration
set.seed(123)  # Setting seed for reproducibility
sampled_data <- train[sample(nrow(train), 500), ]

# Create a pairs plot with more columns using the sampled data
pairs(~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data = sampled_data)

# Create a pairs plot with more columns using the sampled data
pairs(~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20, data = sampled_data)

library(GGally)
ggpairs(sampled_data[, c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10")])

#Data Visualization

# Load ggplot2 library
library(ggplot2)

table(train$Y)
# Create a bar plot for the distribution of Y
ggplot(train, aes(x = as.factor(Y))) +
  geom_bar(fill = "salmon", color = "darkblue") +
  labs(title = "Distribution of Y (Credit Card Default)",
       x = "Credit Card Default Payment (1=Yes, 0=No)",
       y = "Count")
       
# Distribution of Amount of the given credit (X1)
ggplot(train, aes(x = X1)) +
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribution of Amount of the given credit (X1)",
       x = "Amount of the given credit",
       y = "Count")

# Distribution of Age (X5)
ggplot(train, aes(x = X5)) +
  geom_histogram(fill = "darkblue", color = "black", bins = 30) +
  labs(title = "Distribution of Age (X5)",
       x = "Age",
       y = "Count")


# Create a directory to save the plots
dir.create("plots", showWarnings = FALSE)
# Loop through each column in the dataset
for (col in names(train)) {
  if (is.numeric(train[[col]])) {
    # For numeric columns, create a histogram with a different color
    plot_obj <- ggplot(train, aes(x = .data[[col]])) +
      geom_histogram(fill = "lightgreen", color = "black", bins = 30) +  # Change histogram color
      labs(title = paste("Distribution of", col),
           x = col,
           y = "Count")
  } else {
    # For categorical columns, create a bar plot with a different color
    plot_obj <- ggplot(train, aes(x = .data[[col]], fill = factor(Y))) +
      geom_bar(position = "dodge", alpha = 0.7, color = "darkorange") +  # Change bar plot color
      labs(title = paste("Bar Plot of", col, "by Credit Card Default (Y)"),
           x = col,
           y = "Count",
           fill = "Credit Card Default") +
      scale_fill_manual(values = c("skyblue", "coral"))  # Customizing fill colors
  }
  
  # Print and save each plot
  print(plot_obj)
  ggsave(paste("plots/", col, "_plot.png", sep = ""), plot_obj, device = "png")
}




table(train$X1)
hist(train$X1, col = "green", border = "black", main = "Histogram of X1", xlab = "Amount of the given credit (NT dollar)", ylab = "Count", breaks = 30)

table(train$X2)
hist(train$X2, col = "red", border = "black", main = "Histogram of X2", xlab = "Gender (1 = male; 2 = female)", ylab = "Count", breaks = 30)

table(train$X3)
hist(train$X3, col = "blue", border = "black", main = "Histogram of X3", xlab = "Education (1 = graduate school; 2 = university; 3 = high school; 4 = others)", ylab = "Count", breaks = 30)

table(train$X4)
hist(train$X4, col = "orange", border = "black", main = "Histogram of X4", xlab = "Marital status (1 = married; 2 = single; 3 = others)", ylab = "Count", breaks = 30)

table(train$X5)
hist(train$X5, col = "green", border = "black", main = "Histogram of X5", xlab = "Age (year)", ylab = "Count", breaks = 30)



# Select columns X6 to X11
repayment_columns <- train[, c("X6", "X7", "X8", "X9", "X10", "X11")]
months <- c("September", "August", "July", "June", "May", "April")
# Create a bar plot for each repayment status
par(mfrow = c(2, 3))  # Set up a 2x3 grid for subplots
for (i in 1:ncol(repayment_columns)) {
  barplot(table(repayment_columns[, i]), 
          col = "darkblue", 
          main = paste("The repayment status in", months[i], "2005"),
          xlab = paste("X", i + 5),
          ylab = "Count")
}

for (i in 6:11) {
  col_name <- paste("X", i, sep = "")
  cat("Table for", col_name, ":\n")
  print(table(train[[col_name]]))
  cat("\n")
}

# Select columns X12 to X17
bill_columns <- train[, c("X12", "X13", "X14", "X15", "X16", "X17")]
months <- c("September", "August", "July", "June", "May", "April")
# Create line plots for each bill statement
par(mfrow = c(2, 3))  # Set up a 2x3 grid for subplots
for (i in 1:ncol(bill_columns)) {
  hist(bill_columns[, i], type = "l", col = "red",
       main = paste("Amount of Bill Statement in", months[i], "2005"),
       xlab = paste("X", i + 11),
       ylab = "Amount")
}

# Select a random sample of 500 rows
set.seed(123)  # Set seed for reproducibility
sample_indices <- sample(1:nrow(train), 500)
sample_data <- train[sample_indices, ]

# Select columns X12 to X17 for the sample
sample_bill_columns <- sample_data[, c("X12", "X13", "X14", "X15", "X16", "X17")]
par(mfrow = c(2, 3))  # Set up a 2x3 grid for subplots
for (i in 1:ncol(sample_bill_columns)) {
  plot(sample_bill_columns[, i], type = "l", col = "red",
       main = paste("Amount of Bill Statement in", months[i], "2005"),
       xlab = paste("X", i + 11),
       ylab = "Amount")
}

# Select columns X18 to X23
bill_columns2 <- train[, c("X18", "X19", "X20", "X21", "X22", "X23")]
months <- c("September", "August", "July", "June", "May", "April")
par(mfrow = c(2, 3)) 
for (i in 1:ncol(bill_columns2)) {
  hist(bill_columns[, i], type = "l", col = "green",
       main = paste("Amount of previous payment (NT dollar)", months[i]),
       xlab = paste("X", i + 17),
       ylab = "Amount")
}

# Select a random sample of 500 rows
set.seed(123)  # Set seed for reproducibility
sample_indices <- sample(1:nrow(train), 1000)
sample_data <- train[sample_indices, ]

# Select columns X12 to X17 for the sample
sample_bill_columns <- sample_data[, c("X18", "X19", "X20", "X21", "X22", "X23")]
par(mfrow = c(2, 3))  # Set up a 2x3 grid for subplots
for (i in 1:ncol(sample_bill_columns)) {
  plot(sample_bill_columns[, i], type = "l", col = "green",
       main = paste("Amount of previous payment (NT dollar)", months[i]),
       xlab = paste("X", i + 17),
       ylab = "Amount")
}
##################################################################
#Correlation Analysis
# Calculate the correlation matrix for numeric variables
cor_matrix <- cor(train[, sapply(train, is.numeric)])
# Define  color palette
my_palette <- colorRampPalette(c("red", "yellow", "blue"))(n = 299)
# Basic heatmap with new color palette and adjusted margins
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

heatmap(cor_matrix, main = "Correlation Matrix", Colv = NA, Rowv = NA, scale = "none",
        col = my_palette,  
        margins = c(5, 5), cexRow = 0.5, cexCol = 0.5)
print(cor_matrix)



#Data Pre processing

# Specify the names of categorical columns
categorical_columns <- c('X2', 'X3', 'X4')
# Specify the names of columns to be scaled (continuous variables)
continuous_columns <- c('X1', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 'X15', 'X16', 'X17', 'X18', 'X19', 'X20', 'X21', 'X22', 'X23')

# Perform one-hot encoding for categorical variables
train_encoded <- train  # Create a new dataframe for encoding
# Convert categorical columns to factors
train_encoded[, categorical_columns] <- lapply(train_encoded[, categorical_columns], as.factor)

# Perform one-hot encoding using model.matrix
encoded_categorical <- model.matrix(~.-1, data=train_encoded[,categorical_columns])
colnames(encoded_categorical) <- sub(".*\\.", "", colnames(encoded_categorical))  # Remove prefix

# Remove the original categorical columns
train_encoded <- cbind(train_encoded[, -which(names(train_encoded) %in% categorical_columns)], encoded_categorical)

# Perform feature scaling (standardization) on the continuous columns
train_scaled <- train_encoded  # Create a new dataframe for scaling

# Standardize the continuous columns
train_scaled[, continuous_columns] <- scale(train_encoded[, continuous_columns])

head(train_scaled)



# Decision Trees Model
library(tree)
attach(train)
#Prepare the Training and Test Data
train$Y <- as.factor(train$Y)
test$Y <- as.factor(test$Y)

#Build the Tree Model:
set.seed(2)  # Ensuring reproducibility
tree_model <- tree(Y ~ ., data = train)
summary(tree_model)
plot(tree_model)
text(tree_model)
tree_model

#Predict on the Test Set
tree_pred <- predict(tree_model, newdata = test, type = "class")
#Evaluate the Model's Performance on Test Data
table(predicted = tree_pred, actual = test$Y)
#Calculate the proportion of correct predictions to assess accuracy
accuracy <- sum(tree_pred == test$Y) / length(test$Y)
print(accuracy)


#Perform Cross-Validation Pruning

set.seed(3) 
cv_tree <- cv.tree(tree_model, FUN=prune.misclass)
names(cv_tree)
print(cv_tree)

#Plot the Error Rate
par(mfrow=c(1,2))
plot(cv_tree$size, cv_tree$dev, type="b")
plot(cv_tree$k, cv_tree$dev, type="b")




# Apply Pruning to Obtain a 4-Node Tree
prune_tree <- prune.misclass(tree_model, best = 4)
plot(prune_tree)
text(prune_tree)

# Evaluate the Pruned Tree on the Test Data
tree_pred_pruned <- predict(prune_tree, newdata = test, type = "class")
matrix_pruned <- table(predicted = tree_pred_pruned, actual = test$Y)
print(matrix_pruned)
accuracy_pruned <- sum(tree_pred_pruned == test$Y) / length(test$Y)
print(paste("Accuracy with Pruned Tree on Test Data:", accuracy_pruned))


#############################
install.packages("rpart.plot")
library(rpart.plot)

train$Y <- as.factor(train$Y)
test$Y <- as.factor(test$Y)

# Build the decision tree model with rpart
rpart_model <- rpart(Y ~ ., data = train, method = "class")
summary(rpart_model)


# Plot the decision tree
rpart.plot(rpart_model)


# Calculate feature importance
importance <- rpart_model$variable.importance
print(importance)


#Tuning Model
library(caret)

# Set up cross-validation control
control <- trainControl(method="cv", number=10)

# Define the grid for tuning the complexity parameter
grid <- expand.grid(.cp=(0:10)*0.01)  # Create a sequence of cp values

# Train the model using caret's train function
set.seed(123)  # For reproducibility
tuned_model <- train(Y ~ ., data=train, method="rpart", trControl=control, tuneGrid=grid)

# Check the results of the tuning process
print(tuned_model$results)

# Find the row with the highest accuracy
best_row <- tuned_model$results[which.max(tuned_model$results$Accuracy),]
best_cp <- best_row$.cp

# Print the best cp value
print(best_cp)


##############################################
# Build Bagging Model
# Convert categorical variables to factors
train$X2 <- as.factor(train$X2)
train$X3 <- as.factor(train$X3)
train$X4 <- as.factor(train$X4)
test$X2 <- as.factor(test$X2)
test$X3 <- as.factor(test$X3)
test$X4 <- as.factor(test$X4)

library(randomForest)
set.seed(1)

num_predictors <- ncol(train) - 1 
bag_model <- randomForest(Y ~ ., data = train, mtry = num_predictors, importance = TRUE)

print(bag_model)
plot(bag_model)

# Predict on the test set
yhat_bag <- predict(bag_model, newdata=test)

# Calculate the mean classification error (substitute your actual test data's Y values)
mean(yhat_bag != test$Y)

# Predict and calculate mean classification error again
yhat_bag <- predict(bag_model, newdata=test)
mean(yhat_bag != test$Y)

bag_predictions <- predict(bag_model, newdata = test)
bag_accuracy <- mean(bag_predictions == test$Y)
print(paste("Bagging Accuracy:", bag_accuracy))

############################################
# Build Random Forest model
set.seed(1) 
rf_model <- randomForest(Y ~ ., data = train, importance = TRUE)

# Print the model summary
print(rf_model)

# Evaluate variable importance
importance(rf_model)
varImpPlot(rf_model)

rf_predictions <- predict(rf_model, newdata = test)
rf_accuracy <- mean(rf_predictions == test$Y)
print(paste("Random Forest Accuracy:", rf_accuracy))



library(ggplot2)
# Get variable importance data
importance_data <- as.data.frame(importance(rf_model))
# Make the variable names a column in the dataframe
importance_data$Variable <- rownames(importance_data)

# Use ggplot2 to create a colored variable importance plot 
ggplot(importance_data, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_col(fill = 'steelblue') +
  coord_flip() +  # Flip the axes to make it horizontal
  labs(x = "Variables", y = "Mean Decrease in Accuracy", title = "Variable Importance") +
  theme_minimal()

# Use ggplot2 to create a colored variable importance plot for MeanDecreaseGini
ggplot(importance_data, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = 'darkorange') +
  coord_flip() +  # Flip the axes to make it horizontal
  labs(x = "Variables", y = "Mean Decrease in Gini", title = "Variable Importance") +
  theme_minimal()

################################################
install.packages ("devtools")
library ("devtools")
install_github ("gbm-developers/gbm3")
library(gbm3)
library(gbm)
install.packages("doParallel")
library(caret)
library(doParallel)

set.seed(1) 
gbm_model <- gbm(Y ~ ., data = train, distribution = "bernoulli", n.trees = 100, interaction.depth = 1, shrinkage = 0.1, verbose = TRUE)
gbm_predictions <- predict(gbm_model, newdata = test, n.trees = 5000, type = "response")
gbm_predictions_class <- ifelse(gbm_predictions > 0.5, "1", "0") # Assuming 'Yes' is coded as '1'
gbm_accuracy <- mean(gbm_predictions_class == test$Y)
print(paste("Gradient Boosting Accuracy:", gbm_accuracy))
accuracy <- sum(tree_pred == test$Y) / length(test$Y)
print(paste("Gradient Boosting Model Accuracy:",accuracy))

#TUNE MODEL
# Register a parallel 
registerDoParallel(cores = parallel::detectCores())

# Define the control using a cross-validation approach with parallel processing
control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

# Create a smaller grid of hyperparameters 
grid <- expand.grid(
  interaction.depth = c(1, 3), # Smaller range for depth of variable interactions
  n.trees = c(100, 500),       # Fewer options for number of trees
  shrinkage = c(0.01, 0.1),    # Learning rate
  n.minobsinnode = c(10)       # Fewer options for min number of observations
)

# Fit the model
set.seed(1)
gbm_tuned_model <- train(
  Y ~ ., data = train,
  method = "gbm",
  trControl = control,
  tuneGrid = grid,
  verbose = FALSE,
  distribution = "bernoulli"
)
summary(gbm_tuned_model)
plot(gbm_tuned_model)

# Stop parallel processing
stopImplicitCluster()

# Check the best hyperparameters found
print(gbm_tuned_model$bestTune)

# Evaluate the tuned model on the test set
gbm_tuned_predictions <- predict(gbm_tuned_model, newdata = test)
gbm_tuned_accuracy <- mean(gbm_tuned_predictions == test$Y)
print(paste("Tuned Gradient Boosting Model Accuracy:", gbm_tuned_accuracy))

###################################################
# Compare model performances
accuracy <- sum(tree_pred == test$Y) / length(test$Y)
print(paste("Bagging Accuracy:", accuracy))
bag_accuracy <- mean(bag_predictions == test$Y)
print(paste("Bagging Accuracy:", bag_accuracy))
print(paste("Random Forest Model Accuracy:", rf_accuracy))
print(paste("Tuned Gradient Boosting Model Accuracy:", gbm_tuned_accuracy))


