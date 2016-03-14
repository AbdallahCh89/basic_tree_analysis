
## In this section, we provide a basic tree classification method
## For both soft classification and hard classification
## Discussion and conclusions are provided at the end

## This example can be found online 
# https://archive.ics.uci.edu/ml/datasets/Car+Evaluation


## Read a File
#############################################################
## To Read a File in R (First you need to set the directory)
setwd("C:\\Users\\chehade\\Desktop\\Lab_5")

# Read a csv file that is seperated by a ","
# Header  goes if each of the columns have a header
car_data <- read.csv(file="car_eval.csv", header=FALSE, sep=",")
#############################################################




## Add a Header
#############################################################
# It is always a good idea to add a header to your dataset
# I got the headers from the discription file
names(car_data) <- c("buying","maint","doors","persons","lug_boot","safety","label")
# Read only the first two instances (rows)
car_data[c(1,2),]
#############################################################




## Split the dataset into testing and training
#############################################################
# How many instances and how many attributes do we have?
m <- dim(car_data)[1] # 1728 instances and 6 attributes (the last one is the class label)

# Split the dataset into training and testing (70% training and 30% testing)
# Randomly select 30% of the instances to be a testing instances
r <- round(0.3*m)
x_test <- sample(1:m, r, replace=F) # Indices of the 30% instances
x_train <- setdiff(1:m,x_test) # The remaining indices (The Set Difference)

test_data <- car_data[x_test,1:6] # In a testing data it is assumed the label is not known
test_resp <- car_data[x_test,7]
train_data <- car_data[x_train,]
#############################################################




## Generate a basic tree (Help for more information about tree)
#############################################################
# Details will be discussed in lectures
library(tree)
# The frmla can be easily written as the following because of the header:
frmla = label ~ maint + buying + doors + persons + lug_boot + safety
# How does the function understand which of the columns is the label?
# Simply the label is the left handside of the formula (label in our case)
tr = tree(frmla,data=train_data)
summary(tr)
plot(tr); text(tr);
#############################################################




## Use the tree to predict the labels for the testing data
#############################################################

# Soft classification
test_pred <- predict(tr,test_data) # By Default Probability

# Hard classification (Our Interest in this course)
test_lbl <- predict(tr, test_data, type ="class") # This would return the most probable class

# Accuracy in (%)
# 1) test_lbl == test_resp (Compare the predicted with the actual)
# 2) Sum up the matches and divide by the total number of testing instances
# 3) Multiply by 100 for (%)
test_acc <- 100*sum(1*(test_lbl == test_resp))/length(test_resp)
# Error (Simply 100 - Accuracy (%) and 1 - Accuracy in probability)
test_err <- 100 - test_acc
#############################################################




## Discussion and Conclusions
#############################################################
## Conclusions:
# 1) Soft classification provides a probability distribution
# 2) Hard classification provides the class with highest probability
# 3) High training error ==> Tree is not reliable (Not good for the training set)
# 4) Low training error  ==> Good sign but still we need to check for the testing error
# 5) High testing error ==> The tree is an overfitted model for the training set
# 6) Low testing error ==> Great job provided low training error as well

## Method:
# 1) Split your dataset into training and testing
# 2) Design a tree that performs well over the training set without overfitting
# 3) Check the training classification error (if that is good done)

## How to fight overfitting:
# Later in the course. Internal cross-validation.
# Luckily, the tree function we just used already applies cross-validation by default.

## Does the tree always guarantee good results?
# Not really! But, it performs well for categorical data.
#############################################################