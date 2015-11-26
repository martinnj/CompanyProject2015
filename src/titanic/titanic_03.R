# Written by Martin Jørgensen - 25/11-2015
# Used as part of https://www.kaggle.com/c/titanic/

# Clear memory
rm(list=ls())

# Set working directory and import data-sets
setwd("S:/Dropbox/6th year/Blok 2 - VP/src/titanic")
train <- read.csv("train.csv")
test <- read.csv("test.csv")



# Figure out statistics about the age and create a field for children.
# summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1

# Sow how many survived based on sex and being a child or not?
#aggregate(Survived ~ Child + Sex, data=train, FUN=length)

# Or more usefull, what was the proportions of the survivors based on those
# criteria?
#aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
# That gave us nothing new, sex seemed to still be more significant.


# Lets try to make some prediction based on fare, since fare is continues,
# we will make some ranges.
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

# Lets see the data.
aggregate(Survived ~ Fare2 + Pclass + Sex,
          data=train,
          FUN=function(x) {sum(x)/length(x)})

# It seems like people from third class with a fare of 20 or above seemed to do
# worse than other females, let's roll with that.
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' &
              test$Pclass == 3 &
              test$Fare >= 20] <- 0

# Save needed data to a file.
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file="titanic_03_prediction.csv", row.names=F)

# Result: 0.77990
