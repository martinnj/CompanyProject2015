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

dataset <- dataset_test

# Supress warnings while we run the tests. This is because we get a lot of closed
# sessions.
warn_level <- getOption("warn")
options(warn=-1)
print("=======================================")
print("ctree max depth 4:")
results <- ctree_k_fold_test(dataset, iscjretained ~ . , max_depth = 4)
k_fold_result_means(results)

print("rpart max depth 4:")
results <- rpart_k_fold_test(dataset, iscjretained ~ . , max_depth = 4)
k_fold_result_means(results)


print("=======================================")
print("ctree max depth 6:")
results <- ctree_k_fold_test(dataset, iscjretained ~ . , max_depth = 6)
k_fold_result_means(results)

print("rpart max depth 6:")
results <- rpart_k_fold_test(dataset, iscjretained ~ . , max_depth = 6)
k_fold_result_means(results)


print("=======================================")
print("ctree max depth 8:")
results <- ctree_k_fold_test(dataset, iscjretained ~ . , max_depth = 8)
k_fold_result_means(results)

print("rpart max depth 8:")
results <- rpart_k_fold_test(dataset, iscjretained ~ . , max_depth = 8)
k_fold_result_means(results)

# Restore warnings
options(warn=warn_level)

