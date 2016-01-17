#install.packages('foreach'); install.packages('doParallel'); install.packages('caret'); install.packages('e1071'); install.packages('rpart'); install.packages('party'); library(devtools); install_github('cran/gregmisc');

library(foreach)
library(doParallel)
library(caret)
library(gregmisc)


################################################################################
######################## BEGIN UTILITY

read_tsv <- function(file) {
  dataset <- read.table(file,
                        sep="\t",
                        header=TRUE)

  dataset$isedit30m <- factor(dataset$isedit30m)
  dataset$isedit24h <- factor(dataset$isedit24h)
  dataset$isaddpage30m <- factor(dataset$isaddpage30m)
  dataset$isaddpage24h <- factor(dataset$isaddpage24h)
  dataset$isimgupload30m <- factor(dataset$isimgupload30m)
  dataset$isimgupload24h <- factor(dataset$isimgupload24h)
  dataset$iseditdesign30m <- factor(dataset$iseditdesign30m)
  dataset$iseditdesign24h <- factor(dataset$iseditdesign24h)
  dataset$iscjretained <- factor(dataset$iscjretained)
  dataset$isimgupload1d <- factor(dataset$isimgupload1d)
  dataset$iseditdesign1d <- factor(dataset$iseditdesign1d) # Was design edited within the first 24 hours, including first 30 minutes
  dataset$isaddpage1d <- factor(dataset$isaddpage1d)
  dataset$isedit1d <- factor(dataset$isedit1d)
  dataset
}


# Removes variables stored by foreach and registers back to the sequential backend.
unregister <- function(cl) {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
  stopCluster(cl)
  registerDoSEQ()
}

simple_plot <- function(tree, title = "Ctree") {
  plot(tree,
       title = "Ctree",
       type ="simple",           # no terminal plots
       inner_panel=node_inner(tree,
                              pval = FALSE, # no p-values
                              id = FALSE),  # no id of node

       terminal_panel=node_terminal(tree,
                                    abbreviate = TRUE,
                                    digits = 1,                   # few digits on numbers
                                    fill = c("white"),            # make box white not grey
                                    id = FALSE)
  )
}

# Generates a list of all permutations of formulas for a certain
# target variable/column name.
# Arguments:
#   dataset : The dataset to generate data from.
#   target  : A string with the target variables name.
generate_formulas <- function (dataset, target) {
  vars <- names(dataset)
  vars <- vars[! vars %in% target]
  indexes<-unique(apply(combinations(length(vars), length(vars), repeats=T), 1, unique))
  gen.form<-function(x) as.formula(paste(paste(target, ' ~ '),paste( vars[x],collapse='+')))
  formulas<-lapply(indexes, gen.form)
  formulas
}

######################## END UTILITY
################################################################################



################################################################################
######################## BEGIN K-FOLD VALIDATION

# Takes a list of outputs from the K-Fold tests below and gets the mean
# accuracy as well as the prediction / reference results.
k_fold_result_means <- function(input) {
  input_length <- length(input)
  accs <-
  foreach(i = 1:input_length) %do% {
    input[[i]][[1]]
  }
  mean_acc <- mean(unlist(accs))

  ttable <- input[[1]][[2]]
  foreach(i = 2:input_length) %do% {
    ttable = ttable + input[[i]][[2]]
  }
  mean_table = ttable / input_length
  ret <- list(mean_acc, mean_table)
  names(ret) <- c("MeanAccuracy", "MeanPredictions")
  ret
}

# Generates K sets of indices between 1 and Nobs (number of observations)
# Each set have a $train and $test attribute that contain the appropriate indices.
# From https://stackoverflow.com/questions/7402313/generate-sets-for-cross-validation-in-r
f_K_fold <- function(Nobs,K=5){
  rs <- runif(Nobs)
  id <- seq(Nobs)[order(rs)]
  k <- as.integer(Nobs*seq(1,K-1)/K)
  k <- matrix(c(0,rep(k,each=2),Nobs),ncol=2,byrow=TRUE)
  k[,1] <- k[,1]+1
  l <- lapply(seq.int(K),function(x,k,d)
    list(train=d[!(seq(d) %in% seq(k[x,1],k[x,2]))],
         test=d[seq(k[x,1],k[x,2])]),k=k,d=id)
  return(l)
}

# Uses K-fold cross validation to determine the accuracy of a given formula
# using ctrees.
# Arguments:
#   dataset     - The dataset to partition and validate against.
#   formula     - Formula to use for ctree construction.
#   K           - how many folds to use for K-fold cross validation. Default = 5.
#   max_depth   - The maximum depth of the ctree. Default = 4
#   num_threads - Maximum number of threads for parallelisation. Default = Number of physical cores in system.
ctree_k_fold_test <- function(dataset, formula ,K = 5, max_depth = 4, num_threads = parallel:::detectCores()) {
  cl <- makeCluster(num_threads)
  registerDoParallel(cl)

  nobs <- dim(dataset)[1]
  idx_sets <- f_K_fold(nobs, K)

  accs <-
  foreach(i = 1:length(idx_sets)) %dopar% {
    library(party) # When using %dopar% you will need to load the libraries in each thread.
    library(caret)
    idx_set <- idx_sets[[i]]
    test_idx <- idx_set$test
    train_idx <- idx_set$train

    dataset_test <- dataset[test_idx,]
    dataset_train <- dataset[train_idx,]

    fit <- ctree(formula ,
                 data=dataset_train ,
                 controls = ctree_control(maxdepth = max_depth))
    predictions <- predict(fit, dataset_test)
    values <- dataset_test$iscjretained
    mat <- confusionMatrix(predictions, values, positive = 'TRUE')
    list(mat$overall[1], mat$table)
  }
  # Go back to sequential backend and return the data.
  unregister(cl)
  accs
}


# Uses K-fold cross validation to determine the accuracy of a given formula
# using decision trees from rpart.
# Arguments:
#   dataset     - The dataset to partition and validate against.
#   formula     - Formula to use for ctree construction.
#   K           - how many folds to use for K-fold cross validation. Default = 5.
#   max_depth   - The maximum depth of the ctree. Default = 4
#   num_threads - Maximum number of threads for parallelisation. Default = Number of physical cores in system.
rpart_k_fold_test <- function(dataset, formula ,K = 5, max_depth = 4, num_threads = parallel:::detectCores()) {
  cl <- makeCluster(num_threads)
  registerDoParallel(cl)

  nobs <- dim(dataset)[1]
  idx_sets <- f_K_fold(nobs, K)

  accs <-
    foreach(i = 1:length(idx_sets)) %dopar% {
      library(rpart) # When using %dopar% you will need to load the libraries in each thread.
      library(caret)
      idx_set <- idx_sets[[i]]
      test_idx <- idx_set$test
      train_idx <- idx_set$train

      dataset_test <- dataset[test_idx,]
      dataset_train <- dataset[train_idx,]

      fit <- rpart(formula ,
                   data=dataset_train ,
                   method="class",
                   control = rpart.control(maxdepth = max_depth))
      predictions <- predict(fit, dataset_test, type = "class")
      values <- dataset_test$iscjretained
      mat <- confusionMatrix(predictions, values, positive = 'TRUE')
      list(mat$overall[1], mat$table)
    }
  # Go back to sequential backend and return the data.
  unregister(cl)
  accs
}

######################## END K-FOLD VALIDATION
################################################################################



################################################################################
######################## BEGIN TREE TEST / WORK

# Will train a ctree model using the given formula and training dataset,
# test it with the test set and report accuracy.
# Arguments:
#   formula       - Formula to use for model.
#   dataset_train - Training dataset.
#   dataset_test  - Testing dataset, is expected to also contain target column iscjretained.
#   max_depth     - The maximum depth of the ctree, to let the algorithm decide, use a big number.
test_ctree <- function(formula, dataset_train, dataset_test, max_depth, plot_tree = FALSE) {
  fit <- ctree(formula ,
               data=dataset_train ,
               controls = ctree_control(maxdepth = max_depth))
  if (plot_tree) { simple_plot(fit) }
  predictions <- predict(fit, dataset_test)
  values <- dataset_test$iscjretained
  mat <- confusionMatrix(predictions, values, positive = 'TRUE')
  list(mat$overall[1], mat$table)
}

######################## END TREE TEST / WORK
################################################################################
