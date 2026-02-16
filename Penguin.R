# Set Working Directory 
setwd("/Users/christopherdye/Desktop/School/Fall 2024/STAT 5000")

# Installing Palmer Penguins data 
install.packages("palmerpenguins")
library(palmerpenguins)
data("penguins") 


# Loading Packages 
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(nnet)


# Sub-setting data by species 
penguins_clean <- penguins[order(penguins$species),]
penguins_clean <- penguins_clean[ , c(1, 2:5)]
penguins_clean <- na.omit(penguins_clean)
adelie <- subset(penguins_clean, penguins_clean$species == "Adelie")
gentoo <- subset(penguins_clean, penguins_clean$species == "Gentoo")
chinstrap <- subset(penguins_clean, penguins_clean$species == "Chinstrap")


### Visual Presentation of Data -----------------------------------------------
## Boxplots 
par(mfrow = c(1,3)) 
p1_pp <- ggplot(penguins_clean, aes(x = penguins_clean$species, y = penguins_clean$bill_length_mm, fill = penguins_clean$species)) + geom_boxplot(fill = c("orange", "#069494", "purple"))  
p2_pp <- ggplot(penguins_clean, aes(x = penguins_clean$species, y = penguins_clean$bill_depth_mm, fill = penguins_clean$species)) + geom_boxplot(fill = c("orange", "#069494", "purple"))  
p3_pp <- ggplot(penguins_clean, aes(x = penguins_clean$species, y = penguins_clean$flipper_length_mm, fill = penguins_clean$species)) + geom_boxplot(fill = c("orange", "#069494", "purple"))
p4_pp <- ggplot(penguins_clean, aes(x = penguins_clean$species, y = penguins_clean$body_mass_g, fill = penguins_clean$species)) + geom_boxplot(fill = c("orange", "#069494", "purple"))
grid.arrange(p1_pp, p2_pp, p3_pp, p4_pp, ncol=4)

# Summary Statistics 
summary(adelie) 
summary(chinstrap)
summary(gentoo)

# Histograms 
par(mfrow = c(3,4))
hist(adelie$bill_length_mm, 
     col = "orange", 
     xlab = "Bill Length (mm)", 
     main = "") 
hist(adelie$bill_depth_mm, 
     col = "orange1", 
     xlab = "Bill Depth (mm)", 
     main = "") 
hist(adelie$flipper_length_mm, 
     col = "orange2", 
     xlab = "Flipper Length (mm)", 
     main = "") 
hist(adelie$body_mass_g, 
     col = "orange3", 
     xlab = "Body Mass (g)", 
     main = "") 
hist(chinstrap$bill_length_mm, 
     col = "#9EFCFC", 
     xlab = "Bill Length (mm)", 
     main = "")
hist(chinstrap$bill_depth_mm, 
     col = "#13D4D4", 
     xlab = "Bill Depth (mm)", 
     main = "")
hist(chinstrap$flipper_length_mm, 
     col = "#069494", 
     xlab = "Flipper Length (mm)", 
     main = "")
hist(chinstrap$body_mass_g, 
     col = "#176161", 
     xlab = "Body Mass (g)", 
     main = "")
hist(gentoo$bill_length_mm, 
     col = "purple", 
     xlab = "Bill Length (mm)", 
     main = "") 
hist(gentoo$bill_depth_mm, 
     col = "purple1", 
     xlab = "Bill Depth (mm)", 
     main = "") 
hist(gentoo$flipper_length_mm, 
     col = "purple2", 
     xlab = "Flipper Length (mm)", 
     main = "") 
hist(gentoo$body_mass_g, 
     col = "purple3", 
     xlab = "Body Mass (g)", 
     main = "") 



## Scatter plot Matrix
pairs(penguins_clean[2:5],
      panel = function(x, y) {
        points(x, y, pch = 21, bg = c("orange", "#069494", "purple")[penguins_clean$species])
        species_levels <- unique(penguins_clean$species)
        for (s in species_levels) {
          subset_x <- x[penguins_clean$species == s]
          subset_y <- y[penguins_clean$species == s]
          abline(lm(subset_y ~ subset_x), col = c("orange", "#069494", "purple")[which(species_levels == s)], lwd = 2)}},
      upper.panel = NULL, 
      labels = c("Bill Length (mm)", "Bill Depth (mm)", "Flipper Length (mm)", "Body Mass (g)"),
      main = "Scatter Plot Matrix of Palmer Penguins Dataset")



### KNN  --------------------------------------
# Bootstrapping for training models 
knn_results_pp <- matrix(nrow = 50, ncol = 1000) 
knn_accuracy_mean_pp <- c()
for (k in 1:50){
  for (i in 1:1000) {
    n.train.pp <- sample(1:342, replace=TRUE) 
    train_penguins <- penguins_clean[n.train.pp, ] 
    classified.pp <- knn(train = train_penguins[ , 2:5],
                      test = penguins_clean[, 2:5], 
                      cl = train_penguins$species, 
                      k = k) 
    error.knn.pp <- mean(classified.pp != penguins_clean$species)
    accuracy.knn.pp <- 1 - error.knn.pp
    knn_results_pp[k, i] <- accuracy.knn.pp 
  }
  knn_accuracy_mean_pp[k] <- mean(knn_results_pp[k, ])
}
knn_accuracy_mean_pp <- rbind(1:50, knn_accuracy_mean_pp) 

# Inverting data frame for plotting
knn_accuracy_data_pp <- data.frame(K = knn_accuracy_mean_pp[1, ], Accuracy = knn_accuracy_mean_pp[2, ])

# Plotting
ggplot(knn_accuracy_data_pp, aes(x = K, y = Accuracy)) +
  geom_line(color = "lightblue", size = 1) +
  geom_point(color = "lightgreen", size = 3) +
  labs(title = "Model Accuracy for Different K Values",
       x = "Number of Neighbors (K)",
       y = "Accuracy") +
  theme_minimal()


# Comparing histograms of accuracy at varios k values 
par(mfrow = c(4,1))
hist(knn_results_pp[1, ], 
     col = "red2", 
     xlab = "Accuracy", 
     main = "Accuracy of 1000 bootstrap samples from penguins at k = 1") 
hist(knn_results_pp[7, ], 
     col = "steelblue1", 
     xlab = "Accuracy", 
     main = "Accuracy of 1000 bootstrap samples from penguins at k = 7")
hist(knn_results_pp[30, ], 
     col = "green3", 
     xlab = "Accuracy", 
     main = "Accuracy of 1000 bootstrap samples from penguins at k = 30") 
hist(knn_results_pp[50, ], 
     col = "purple3", 
     xlab = "Accuracy", 
     main = "Accuracy of 1000 bootstrap samples from penguins at k = 50") 


### Multinomial Logistic Regression ------------------------------------------
# 
logit_results_pp <- c()
for (i in 1:1000) {
  n.train.pp <- sample(1:342, replace=TRUE) 
  train_penguins <- penguins_clean[n.train.pp, ] 
  model_pp <- multinom(species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, train_penguins)
  predictions.pp <- predict(model_pp, penguins_clean[ , 2:5], type = "class") 
  error.logit.pp <- mean(predictions.pp != penguins_clean$species)
  accuracy.logit.pp <- 1 - error.logit.pp
  logit_results_pp[i] <- accuracy.logit.pp
}
logit_accuracy_mean_pp <- mean(logit_results_pp)

# Evaluate Accuracy 
hist(logit_results_pp, 
     col = "purple3", 
     xlab = "Accuracy", 
     main = "Accuracy of 1000 bootstrap samples from penguins of Multinomial Logistic Regression") 

# Build Model 
fit_pp <- multinom(species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, 
            data = penguins_clean, 
            family = multinomial) 
summary(fit_pp)



