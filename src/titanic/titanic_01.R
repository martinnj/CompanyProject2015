# Written by Martin Jørgensen - 25/11-2015
# Used as part of https://www.kaggle.com/c/titanic/

# Clear memory
rm(list=ls())


# Set working directory and import data-sets
setwd("S:/Dropbox/6th year/Blok 2 - VP/src/titanic")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Since
# > prop.table(table(train$Survived))
# shows that most people die, lets just try and set everyone as perished and see
# what happens with that result.
test$Survived <- rep(0, 418)

# Save needed data to a file.
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file="titanic_01_prediction.csv", row.names=F)

# Result: 0.62679
