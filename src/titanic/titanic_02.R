# Written by Martin Jørgensen - 25/11-2015
# Used as part of https://www.kaggle.com/c/titanic/

# Clear memory
rm(list=ls())

# Set working directory and import data-sets
setwd("S:/Dropbox/6th year/Blok 2 - VP/src/titanic")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# By looking at the proportion of survivors based on sex, using
# prop.table(table(train$Sex, train$Survived),1)
# We see that  ~74% of all women survived, where only ~18% of men did.
# So lets try to just predict all men as dead and all women as alive.

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

# Save needed data to a file.
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file="titanic_02_prediction.csv", row.names=F)

# Result: 0.76555
