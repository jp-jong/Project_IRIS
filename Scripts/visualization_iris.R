# Plotting the summaries for an overview
library(reshape2)
library(ggplot2)
iris.df = melt(iris, id.vars = c('Species'))
head(iris.df)

ggplot(iris.df, aes(x= Species, y = value, fill = variable)) + 
  geom_boxplot() +
  xlab('Species') + ylab('Values') + theme_bw() + 
  theme(text = element_text(size=16), 
        axis.text.x = element_text(angle=0, hjust=0.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  ggtitle("Measurements by group") +
  scale_fill_discrete(name = "Characteristics", 
                      labels = c("Sepal Length", "Sepal Width","Petal Length", 
                                 "Petal Width"))


## Multivariate analysis 
## PCA
#PCA
install.packages("factoextra")
library(factoextra)
iris.data <- iris[,1:4]
iris.name < iris[,5]
system.time(p <- prcomp(iris.data, scale=TRUE)) # user:0.001 system:0.001 elapsed: 0.005
names(p)
summary(p)

# Plot PCA with labels
fviz_pca_ind(p,
             geom.ind = "point", # Show points only (no text)
             col.ind = iris.name, # Color by species
             addEllipses = FALSE, # Add concentration ellipses
             legend.title = "Species") + 
  theme_minimal() + 
  labs(title = "PCA of Iris Dataset", center=TRUE,
       x = "PC1",
       y = "PC2") +
  theme(plot.title = element_text(hjust = 0.5)) # Center the title


# t-SNE
set.seed(1)
library(Rtsne)
iris_d_tsne <- as.matrix(iris[,1:4])
system.time(iris.rtsne <- Rtsne(iris_d_tsne, check_duplicates = FALSE, verbose = F)) # user: 0.321 system:0.00 elapsed:0.322

# plot
# Create a data frame for the t-SNE results
tsne_df <- data.frame(iris.rtsne$Y, Species = iris$Species)
colnames(tsne_df) <- c("tSNE1", "tSNE2", "Species")

# Plot t-SNE results
ggplot(tsne_df, aes(x = tSNE1, y = tSNE2, color = Species)) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "t-SNE of Iris Dataset",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2") +
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

### Support Vector Machine
library('e1071')

index <- c(1:nrow(iris))
test.index <- sample(index, size = (length(index)/3))
train <- iris[-test.index,]
test <- iris[test.index,]

svm.model.linear <- svm(Species ~., data = train, kernel = 'linear')
table(Prediction = predict(svm.model.linear, train),Truth = train$Species)

svm.model.poly <- svm(Species ~ ., data = train, kernel = 'polynomial')
table(Prediction = predict(svm.model.poly, train),Truth = train$Species)

tuned.svm <- tune.svm(Species~., data = train, kernel = 'linear',
                      gamma = seq(1/2^nrow(iris),1, .01), cost = 2^seq(-6, 4, 2))
tuned.svm

tuned.svm <- svm(Species ~ . , data = train, kernel = 'linear', gamma = 7.006492e-46, cost = 0.25)
table(Prediction = predict(tuned.svm, train),Truth = train$Species)

best.svm <- best.svm(Species~. , data = train, kernel = 'linear')
best.svm
table(Prediction = predict(best.svm, train), Truth = train$Species)

best.svm.pred <- predict(best.svm, test)
table(Prediction = best.svm.pred, Truth = test$Species)

sum(test$Species == best.svm.pred)/50

# ChatGPT's SVM
# Install and load the required package
install.packages("e1071")
library(e1071)

# Load the iris dataset
data(iris)

# Split the data into training and test sets
set.seed(123)
train_indices <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

# Perform SVM with hyperparameter tuning using best.svm
tuned_svm <- tune.svm(Species ~ ., data = train_data,
                      gamma = 10^(-6:-1), cost = 10^(1:2))

# Get the best model
best_svm <- tuned_svm$best.model

# Print the best parameters
print(best_svm)

# Make predictions on the test set
svm_predictions <- predict(best_svm, newdata = test_data)

# Confusion matrix
confusion_matrix <- table(Predicted = svm_predictions, Actual = test_data$Species)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))



