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


#removing outliers

#function approach 1
remove_outliers <- function (data, columns) {
  for (column in columns) {
    iqr_feature <- IQR(data[[column]])
    
    quantiles <- quantile(data[[column]], probs = c(0.25, 0.75), na.rm = TRUE)
    lower_boundary <- quantiles[1] - 1.5 * iqr_feature
    upper_boundary <- quantiles[2] + 1.5 * iqr_feature
    
    data <- data[which((data[[column]] >= lower_boundary) & (data[[column]] <= upper_boundary)), ]
  }
  return (data)

}

clean_data <- remove_outliers(data, c("carat", "depth", "price", "x", "y"))

##function approach 2
'''
remove_outliers_two <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  x[ x < (q1 - 1.5*iqr) | x > (q3 + 1.5*iqr)] <- NA
  return(x)
}
'''
##Visualizing the outliers
boxplot(clean_data$price)
numeric_data3 <- clean_data %>% select_if(is.numeric)
par(mfrow = c(ceiling(sqrt(ncol(numeric_data3))), ceiling(sqrt(ncol(numeric_data3)))))
for (i in 1:ncol(numeric_data3)) {
  boxplot(numeric_data3[, i], main = colnames(numeric_data3)[i])
}

## MACHINE LEARNING ----
# relationships ----
#cont vs cont - corr matrix, scatter plots
#cont vs cat - boxplots, ANOVA test
#cat vs cat - barplot, chi square test

## Target variable - price (cont variable) ----

#grouping our cat data
categorical_variables <- clean_data %>% select_if(is.character)
head(categorical_variables)

#performing anova test
perform_anova <- function(data, cont_var, cat_vars) {
  for (cat_var in cat_vars) {
  anova_result <- aov(as.formula(paste(cont_var, '~', cat_var)), data = data)
  summary_anova <- summary(anova_result)
  p_value <- summary_anova[[1]]["Pr(>F)"][1, ]
  
  if (p_value <= 0.05) {
    cat("There is a significant relationship found between ", cont_var, "and ", cat_var, "with a p-value of ", p_value, "\n")
  }
  else {
    cat("No significant relationship found between ", cont_var, "and", cat_var, "with a p-value of ", p_value, "\n")
  }
  }
}

perform_anova(data, "price", c("cut", "colour", "clarity", "P", "PC"))

## cont vs cont ----
##approach 1
correlation_matrix <- cor(numeric_data3)
correlation_matrix

##approach 2
#visualization 
pairs(clean_data[, c("price", "depth", "x", "y", "carat")])

##approach 3
corrplot(correlation_matrix, method = "circle")


## cat and cat ----

perform_chisq <- function(data, cat_v1, cat_v2s) {
  for (cat_v2 in cat_v2s) {
  chisq_result <- chisq.test(table(clean_data[[cat_v1]], clean_data[[cat_v2]]))
  p_value <- chisq_result$p.value
  
  if (p_value <= 0.05) {
    cat("Significant relationship found between ", cat_v1, "and ", cat_v2, "with p-value ", p_value, "\n")
  }
  else {
    cat("Significant relationship not found between ", cat_v1, "and ", cat_v2, "with p-value ", p_value, "\n")
    
  }
  }
}

perform_chisq(clean_data, "colour", c("cut", "clarity", "P", "PC"))

###create machine learning database ----
ml_data <- clean_data[, !names(clean_data) %in% c("depth", "P", "PC")]
head(ml_data)


##model building

