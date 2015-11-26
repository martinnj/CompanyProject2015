# Written by Martin Jørgensen - 25/11-2015
# Used as part of https://www.kaggle.com/c/titanic/

#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')

# Needed libraries.
library(rpart) # Recursive Partitioning and Regression Trees
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Clear memory
rm(list=ls())

# Set working directory and import data-sets
setwd("S:/Dropbox/6th year/Blok 2 - VP/src/titanic")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Lets fit a decision tree.
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")

# The tree fitting can be controlled using this call instead:
#fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
#             data=train,
#             method="class",
#             control=rpart.control( your controls ))
# Where "your controls" can be set with the attributes found with
#?rport.control

# We can plot the decision tree to get a better grip of the model.
fancyRpartPlot(fit)

# But let's actually predict something!
Prediction <- predict(fit, test, type = "class")

# Save needed data to a file.
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file="titanic_04_prediction.csv", row.names=F)

# Result: 0.78469
