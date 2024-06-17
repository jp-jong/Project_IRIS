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
decision_tree <- rpart(Species ~ ., data = train_data, method = "class")

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

# Identify the least important feature
least_important_feature <- rownames(importance)[which.min(importance$Importance)]
print(paste("Excluding the least important feature:", least_important_feature))

# Exclude the least important feature from the dataset
train_data_reduced <- train_data[, !(names(train_data) %in% least_important_feature)]
test_data_reduced <- test_data[, !(names(test_data) %in% least_important_feature)]

# Generate the decision tree again with the reduced dataset
decision_tree_reduced <- rpart(Species ~ ., data = train_data_reduced, method = "class", 
                               control = rpart.control(minsplit = 2, cp = 0))

# Print the CP table to inspect the complexity parameter values for the reduced tree
printcp(decision_tree_reduced)

# Prune the reduced tree using the optimal cp value
optimal_cp_reduced <- decision_tree_reduced$cptable[which.min(decision_tree_reduced$cptable[,"xerror"]), "CP"]
pruned_tree_reduced <- prune(decision_tree_reduced, cp = optimal_cp_reduced)

# Visualize the pruned tree for the reduced dataset
rpart.plot(pruned_tree_reduced, main = "Pruned Decision Tree for Iris Dataset (Reduced Features)")

# Predict on test data with the reduced model and calculate accuracy
predictions_reduced <- predict(pruned_tree_reduced, newdata = test_data_reduced, type = "class")
confusion_matrix_reduced <- table(Predicted = predictions_reduced, Actual = test_data$Species)
print("Confusion Matrix (Reduced Features):")
print(confusion_matrix_reduced)

accuracy_reduced <- sum(diag(confusion_matrix_reduced)) / sum(confusion_matrix_reduced)
print(paste("Accuracy (Reduced Features):", round(accuracy_reduced * 100, 2), "%"))
