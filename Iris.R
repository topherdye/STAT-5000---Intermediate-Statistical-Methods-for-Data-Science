# Set Working Directory 
setwd("/Users/christopherdye/Desktop/School/Fall 2024/STAT 5000/Presenation")


# Uploading built in Iris Flower dataset 
data("iris")


# Load libraries 
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(nnet)


# Subsetting data by Iris species 
setosa <- subset(iris, iris$Species == "setosa")
versicolor <- subset(iris, iris$Species == "versicolor")
virginica <- subset(iris, iris$Species == "virginica")


### Visual Presentation of Data -----------------------------------------------
## Boxplots 
par(mfrow = c(1,3)) 
p1_if <- ggplot(iris, aes(x = iris$Species, y = iris$Sepal.Length, fill = iris$Species)) + geom_boxplot()  
p2_if <- ggplot(iris, aes(x = iris$Species, y = iris$Sepal.Width, fill = iris$Species)) + geom_boxplot()  
p3_if <- ggplot(iris, aes(x = iris$Species, y = iris$Petal.Length, fill = iris$Species)) + geom_boxplot()
p4_if <- ggplot(iris, aes(x = iris$Species, y = iris$Petal.Width, fill = iris$Species)) + geom_boxplot()
grid.arrange(p1_if, p2_if, p3_if, p4_if, ncol=4)

# Summary Statistics 
summary(setosa) 
summary(versicolor)
summary(virginica)

# Histograms 
par(mfrow = c(3,4))
hist(setosa$Sepal.Length, 
     col = "red", 
     xlab = "Sepal Length", 
     main = "") 
hist(setosa$Sepal.Width, 
     col = "red2", 
     xlab = "Sepal Width", 
     main = "") 
hist(setosa$Petal.Length, 
     col = "red3", 
     xlab = "Petal Length", 
     main = "") 
hist(setosa$Petal.Width, 
     col = "red4", 
     xlab = "Petal Width", 
     main = "") 
hist(versicolor$Sepal.Length, 
     col = "green", 
     xlab = "Sepal Length", 
     main = "")
hist(versicolor$Sepal.Width, 
     col = "green2", 
     xlab = "Sepal Width", 
     main = "")
hist(versicolor$Petal.Length, 
     col = "green3", 
     xlab = "Petal Length", 
     main = "")
hist(versicolor$Petal.Width, 
     col = "green4", 
     xlab = "Petal Width", 
     main = "")
hist(virginica$Sepal.Length, 
     col = "steelblue", 
     xlab = "Sepal Length", 
     main = "") 
hist(virginica$Sepal.Width, 
     col = "steelblue1", 
     xlab = "Sepal Width", 
     main = "") 
hist(virginica$Petal.Length, 
     col = "steelblue2", 
     xlab = "Petal Length", 
     main = "") 
hist(virginica$Petal.Width, 
     col = "steelblue3", 
     xlab = "Petal Width", 
     main = "") 



## Scatter plot Matrix
pairs(iris[1:4],
      panel = function(x, y) {
        points(x, y, pch = 21, bg = c("red", "green", "blue")[iris$Species])
        species_levels <- unique(iris$Species)
        for (s in species_levels) {
          subset_x <- x[iris$Species == s]
          subset_y <- y[iris$Species == s]
          abline(lm(subset_y ~ subset_x), col = c("red", "green", "blue")[which(species_levels == s)], lwd = 2)}},
      upper.panel = NULL, 
      labels = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width"),
      main = "Scatter Plot Matrix of Iris Flower Dataset")



### KNN  --------------------------------------
# Bootstrapping for training models 
knn_resultsP_if <- matrix(nrow = 50, ncol = 1000) 
knn_accuracy_mean_if <- c()
for (k in 1:50){
  for (i in 1:1000) {
    n.train.if <- sample(1:150, replace=TRUE) 
    train_iris <- iris[n.train.if, ] 
    classified.if <- knn(train = train_iris[ , 1:4],
                          test = iris[, 1:4], 
                          cl = train_iris$Species, 
                          k = k) 
    error.knn.if <- mean(classified.if != iris$Species)
    accuracy.knn.if <- 1 - error.knn.if
    knn_results_if[k, i] <- accuracy.knn.if 
  }
  knn_accuracy_mean_if[k] <- mean(knn_results_if[k, ])
}
knn_accuracy_mean_if <- rbind(1:50, knn_accuracy_mean_if) 

# Inverting data frame for plotting
knn_accuracy_data_if <- data.frame(K = knn_accuracy_mean_if[1, ], Accuracy = knn_accuracy_mean_if[2, ])

# Plotting accuracy means 
ggplot(knn_accuracy_data_if, aes(x = K, y = Accuracy)) +
  geom_line(color = "lightblue", size = 1) +
  geom_point(color = "lightgreen", size = 3) +
  labs(title = "Model Accuracy for Different K Values",
       x = "Number of Neighbors (K)",
       y = "Accuracy") +
  theme_minimal()


# Comparing histograms of accuracy at various k values 
par(mfrow = c(4,1))
hist(knn_results_if[1, ], 
     col = "red2", 
     xlab = "Accuracy", 
     main = "Accuracy of 1000 bootstrap samples from Iris at k = 1") 
hist(knn_results_if[4, ], 
     col = "steelblue1", 
     xlab = "Accuracy", 
     main = "Accuracy of 1000 bootstrap samples from Iris at k = 4")
hist(knn_results_if[13, ], 
     col = "green3", 
     xlab = "Accuracy", 
     main = "Accuracy of 1000 bootstrap samples from Iris at k = 13") 
hist(knn_results_if[30, ], 
     col = "purple3", 
     xlab = "Accuracy", 
     main = "Accuracy of 1000 bootstrap samples from Iris at k = 30") 


### Multinomial Logistic Regression ------------------------------------------
# Bootstrapping for training models 
logit_results_if <- c()
for (i in 1:1000) {
  n.train.if <- sample(1:150, replace=TRUE) 
  train_iris <- iris[n.train.if, ] 
  model_if <- multinom(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, train_iris)
  predictions.if <- predict(model_if, iris[ , 1:4], type = "class") 
  error.logit.if <- mean(predictions.if != iris$Species)
  accuracy.logit.if <- 1 - error.logit.if
  logit_results_if[i] <- accuracy.logit.if
}
logit_accuracy_mean_if <- mean(logit_results_if)
summary(model_if)

# Evaluate Accuracy 
hist(logit_results_if, 
     col = "purple3", 
     xlab = "Accuracy", 
     main = "Accuracy of 1000 bootstrap samples from Iris of Multinomial Logistic Regression") 

# Build Model 
fit_if <- multinom(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                   data = iris, 
                   family = multinomial) 
summary(fit_if)


