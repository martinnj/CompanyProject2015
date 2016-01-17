#install.packages('randomForest'); install.packages('party'); install.packages('rattle'); install.packages('rpart.plot'); install.packages('RColorBrewer'); install.packages('curl'); library(devtools); install_github('krlmlr/kimisc'); install.packages('foreach');

library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest); library(rpart); library(party)
library(kimisc) # For dataset sampling.
library(foreach)

source("tools.R")

dataset_train <- read_tsv("dataset_train.tsv")
dataset_test <- read_tsv("dataset_test.tsv")


################################################################################
# get same random seed each time for reproducibility
set.seed(1337)

#formulas <- generate_formulas(dataset_train, "iscjretained")
formulas <- c(iscjretained ~ . ,
              iscjretained ~ edits14 ,
              iscjretained ~ logins14 ,
              iscjretained ~ edits14 + logins14)


# Supress warnings while we run the tests. This is because we get a lot of
# closed sessions which spam inbetween iterations.
warn_level <- getOption("warn")
options(warn=-1)

num_formulas = length(formulas)

foreach(i = 1:num_formulas) %do% {
  print("=======================================")
  print("=======================================")
  print(paste("FORMULA: ", formulas[i]))

  for(j in c(4,6,8)) {
    print("=======================================")
    print(paste("DEPTH  : ", j))
    results <- ctree_k_fold_test(dataset_train, formulas[[i]] , max_depth = j)
    result <- k_fold_result_means(results)
    print(result)
    NULL
  }
  NULL
}

# Restore warnings
options(warn=warn_level)
