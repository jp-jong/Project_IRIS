# Install and load the required packages
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Load the iris dataset
data(iris)

# Split the data into training and test sets
set.seed(123)
train_indices <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

# Generate the decision tree
decision_tree <- rpart(Species ~ ., data = train_data, method = "class")

# Show the importance of the variables
importance <- as.data.frame(decision_tree$variable.importance)
importance$Variable <- rownames(importance)
importance <- importance[order(-importance[,1]), ]
colnames(importance)[1] <- "Importance"
print("Variable Importance:")
print(importance)

# Visualize the tree
rpart.plot(decision_tree, main = "Decision Tree for Iris Dataset")

# Optional: Predict on test data and calculate accuracy
predictions <- predict(decision_tree, newdata = test_data, type = "class")
confusion_matrix <- table(Predicted = predictions, Actual = test_data$Species)
print("Confusion Matrix:")
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

### added more trees and pruning
# Install and load the required packages
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Load the iris dataset
data(iris)

# Split the data into training and test sets
set.seed(123)
train_indices <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

# Generate a deeper decision tree
decision_tree <- rpart(Species ~ ., data = train_data, method = "class", 
                       control = rpart.control(minsplit = 2, cp = 0))

# Print the CP table to inspect the complexity parameter values
printcp(decision_tree)

# Prune the tree using the optimal cp value
optimal_cp <- decision_tree$cptable[which.min(decision_tree$cptable[,"xerror"]), "CP"]
pruned_tree <- prune(decision_tree, cp = optimal_cp)

# Show the importance of the variables
importance <- as.data.frame(pruned_tree$variable.importance)
importance$Variable <- rownames(importance)
importance <- importance[order(-importance[,1]), ]
colnames(importance)[1] <- "Importance"
print("Variable Importance:")
print(importance)

# Visualize the pruned tree
rpart.plot(pruned_tree, main = "Pruned Decision Tree for Iris Dataset")

# Optional: Predict on test data and calculate accuracy
predictions <- predict(pruned_tree, newdata = test_data, type = "class")
confusion_matrix <- table(Predicted = predictions, Actual = test_data$Species)
print("Confusion Matrix:")
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

