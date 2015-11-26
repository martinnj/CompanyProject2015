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


# To do feature extraction, it is nice to have the datasets in one.
# So we create a Survived column in test and use rbind to bind it based on rows.
test$Survived <- NA
combi <- rbind(train, test)

# Since names are stored as factors, lets turn them into strings which we can
# work with.
combi$Name <- as.character(combi$Name)

# Finding the title should be a matter of splitting on '.' and ',' and taking
# the pieces we want.
combi$Title <- sapply(combi$Name,
                      FUN=function(x) {
                        strsplit(x, split='[,.]')[[1]][2]
                      })
combi$Title <- sub(' ', '', combi$Title)

# Inspecting the titles with 'table(combi$Title)' reveals that some titles are
# very unique and not likely to give us any good data, they most likely will
# result in highly specialised rules and overfitting, let's combine some that
# makes sense. Even if it might have offended those that held the titles. :)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

# Now we are done working on the titles we can convert them back to factors.
combi$Title <- factor(combi$Title)

# We can also attempt to combine SibSb and Parch into FamilySize, maybe mom and
# dad have a hard time tracking down their children when trying to leave the
# sinking ship.
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# We can also try to group families together since they might want to try and
# escape together, which could potentially affect the outcome.
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

# In order to keep common names from bunching up with other families, let's
# include the family size and call it an ID.
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

# To avoid greating ID's for people travelling alone or just with one companion
# we create a 'small' ID as well.
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

# Looks like som small groupings made it through, maybe they had different
# names? Anyway, we can fix that.
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

# We should now be ready to split our combined data back into the two sets.
train <- combi[1:891,]
test <- combi[892:1309,]

# Lets fit a decision tree.
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked +
                        Title + FamilySize + FamilyID,
             data=train,
             method="class")


# We can plot the decision tree to get a better grip of the model.
fancyRpartPlot(fit)

# But let's actually predict something!
Prediction <- predict(fit, test, type = "class")

# Save needed data to a file.
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file="titanic_05_prediction.csv", row.names=F)

# Result: 0.79426
