#install.packages('randomForest'); install.packages('party'); install.packages('rattle'); install.packages('rpart.plot'); install.packages('RColorBrewer'); install.packages('curl'); library(devtools); install_github('krlmlr/kimisc'); install.packages('caret');

library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest); library(rpart); library(party)
library(kimisc) # For dataset sampling.
library(caret)

source("tools.R")

dataset_train <- read_tsv("dataset_train.tsv")
dataset_test <- read_tsv("dataset_test.tsv")


################################################################################
# get same random seed each time for reproducibility
set.seed(1337)

# We wan't to try and see if the trees, look any different if we change the
# proportions of retained to non-retained customers.

# Seperate true and false values
all_true <- dataset_train[dataset_train$iscjretained == TRUE,]
all_false <- dataset_train[dataset_train$iscjretained == FALSE,]

# Find the largest
equality_size <- min(dim(all_true)[1], dim(all_false)[1])



dataset_equal <- rbind(sample.rows(all_true, size = equality_size),
                       sample.rows(all_false, size = equality_size))


################################################################################
# get same random seed each time for reproducibility
set.seed(1337)

# Disable warnings to avoid spam inside the loop.
warn_level <- getOption("warn")
options(warn=-1)

for(j in c(4,6,8)) {
  print("=======================================")
  print("=======================================")
  print(paste("DEPTH  : ", j))

  print("=======================================")
  print("Full training dataset:")
  results <- ctree_k_fold_test(dataset_train, iscjretained ~ . , max_depth = j)
  result <- k_fold_result_means(results)
  print(result)

  print("=======================================")
  print("Equal dataset:")
  results <- ctree_k_fold_test(dataset_equal, iscjretained ~ . , max_depth = j)
  result <- k_fold_result_means(results)
  print(result)
  NULL
}

# Restore warnings
options(warn=warn_level)

################################################################################
# get same random seed each time for reproducibility
set.seed(1337)

test_ctree(iscjretained ~. , dataset_equal, dataset_test, max_depth = 4, TRUE )
test_ctree(iscjretained ~. , dataset_equal, dataset_test, max_depth = 6, TRUE )
test_ctree(iscjretained ~. , dataset_equal, dataset_test, max_depth = 8, TRUE )
