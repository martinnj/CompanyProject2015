#install.packages('randomForest'); install.packages('party'); install.packages('rattle'); install.packages('rpart.plot'); install.packages('RColorBrewer'); install.packages('curl'); library(devtools); install_github('krlmlr/kimisc');

library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest); library(rpart); library(party)
library(kimisc) # For dataset sampling.

source("tools.R")

dataset_train <- read_tsv("dataset_train.tsv")
dataset_test <- read_tsv("dataset_test.tsv")


################################################################################
# get same random seed each time for reproducibility
set.seed(1337)

test_ctree(iscjretained ~ . , dataset_train, dataset_test, max_depth = 4, TRUE )
test_ctree(iscjretained ~ . , dataset_train, dataset_test, max_depth = 6, TRUE )
test_ctree(iscjretained ~ . , dataset_train, dataset_test, max_depth = 8, TRUE )


################################################################################
# get same random seed each time for reproducibility
set.seed(1337)
dataset_train2 <- subset(dataset_train, TRUE, c(-logins14))
dataset_test2 <- subset(dataset_test, TRUE, c(-logins14))

test_ctree(iscjretained ~ . , dataset_train2, dataset_test2, max_depth = 4, TRUE )
test_ctree(iscjretained ~ . , dataset_train2, dataset_test2, max_depth = 6, TRUE )
test_ctree(iscjretained ~ . , dataset_train2, dataset_test2, max_depth = 8, TRUE )
