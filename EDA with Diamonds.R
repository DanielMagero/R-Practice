# Pre-processing ----
## loading packages and libraries ----
library(tidyverse)
library(caret)
library(mice)
library(ggplot2)
library(corrplot)
library(randomForest)
library(xgboost)

## loading the data ----
data <- read.csv('pricingOfDiamonds.csv')

#review a few rows of the data
head(data)
View(data)

#review  the structure of the data
str(data)

#summary statistics
summary(data)

## Handling missing values ----
## approach one

missn_values <- sapply(data, function(x) sum(is.na(x)))
missn_values

## approach two 
missn_values2 <- data %>% summarise(numeric_missing = sum(is.na(.)), categorical_missing = sum(is.na(as.character((.))))) 
missn_values2

##approach three
missn_value3 <- data %>% summarise_all(funs(sum(is.na(.))))
missn_value3

## separating your data into categorical and numeric ----
numeric_data <- sapply(data, is.numeric)
par(mfrow = c(2, 1))

## plotting to show distribution
hist(data$price, col = "blue", main = "Histogram of price")
hist(data$depth, col = "blue", main = "Histogram for depth")
hist(data$x, col ="blue", main = "Histogram of x")

lines(density(data$price), col = "blue", lwd = 2)

hist(numeric_data, breaks = 10, col = "blue", border = "black", xlab = "Value", ylab = "frequency",
     main = "Histogram for Data")

lines(density(data), col = "blue", lwd = 2)
  
## dealing with Missing value ----
## Drop missing values ----
new_data <- na.omit(data)

## Impute for missing values ----
## Approach 1
#normal distribution numeric
impute_mean <- function(y) replace(y, is.na(y), mean(y, na.rm = TRUE))
impute_mean(data$depth)

#skewed numeric
impute_median <- function(z) replace(z, is.na(z), median(z, na.rm = TRUE))
impute_median(data$price)
#cat var
impute_mode <- function(d) {
  model_value <- as.numeric(names(table(sort(d), decreasing = TRUE)[1]))
  replace(d, is,na(d), model_value)
}
impute_mode(data$colour)

## Approach 2
data$price[is.na(data$price)] <- median(data$price, na.rm = TRUE)
data$price[is.na(data$price)] <- mean(data$price, na.rm = TRUE)

## Approach 3
imputed_data <- mice(data, m = 5, method = "pmm", maxit = 50, seed = 500)
data2 <- complete(imputed_data, 1)

## Handling Outliers ----

numeric_data <- sapply(data, is.numeric)
numeric_data2 <- data %>% select_if(is.numeric)


par(mfrow = c(ceiling(sqrt(ncol(numeric_data2))), ceiling(sqrt(ncol(numeric_data2)))))
for (i in 1:ncol(numeric_data2)){
  boxplot(numeric_data2[, i], main = colnames(numeric_data2)[i])
}

#removing categorical
data <- data[, !names(data) %in% c("ID")]

#using ggplot2
boxplot_gg <- function(data, columns) {
  for (column in columns){
    p <- ggplot(data, mapping = aes(x = "", y = .data[[column]])) +
      geom_boxplot() +
      labs(title = paste("Boxplot for ", column))
  }
}

hist_gg <- function(data, columns) {
  for (column in columns){
    p <- ggplot(data, mapping = aes(x = "", y = .data[[column]])) +
      geom_histogram() +
      labs(title = paste("Histogram for ", column))
  }
}

boxplot_gg(data, c("carat", "depth", "price", "x", "y"))

hist_gg(data, c("carat", "depth", "price", "x", "y"))


# relationships ----
## cat and cat ----
