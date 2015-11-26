# Written by Martin Jørgensen - 25/11-2015
# Used as part of https://www.kaggle.com/c/titanic/

#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
#install.packages('randomForest')
#install.packages('party')

# Needed libraries.
library(rpart) # Recursive Partitioning and Regression Trees
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(party)

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

# Since we want to grow a random forrest, we need to fill in missing variables.
# Starting with age, we create a decision tree to guess the age of a person
# based on their other data.
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked +
                  Title + FamilySize,
                data=combi[!is.na(combi$Age),],
                method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

# Embarked also have some blank spaces, not NA, but since we are cleaning, lets
# just get rid of them. Most people got on at Southampton we'll just use that.
combi$Embarked[which(combi$Embarked == '')] = "S"
combi$Embarked <- factor(combi$Embarked)

# There also seem to be some missing fare, we can replace that with the median
# fare.
combi$Fare[which(is.na(combi$Fare))] <- median(combi$Fare, na.rm=TRUE)


# We should now be ready to split our combined data back into the two sets.
train <- combi[1:891,]
test <- combi[892:1309,]



# And now to make a forrest of conditional inference trees.
# First, seed the random generator, then grow the forrest.
set.seed(1337)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + Fare +
                 Embarked + Title + FamilySize + FamilyID,
               data = train,
               controls=cforest_unbiased(ntree=2000,
                                         mtry=3))


# But let's actually predict something!
Prediction <- predict(fit, test, OOB=TRUE, type = "response")

# Save needed data to a file.
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file="titanic_08_prediction.csv", row.names=F)

# Result: 0.81340 from Pclass + Sex + Age + Fare + Embarked + Title + FamilySize + FamilyID
