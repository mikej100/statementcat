library(dplyr)
library(naivebayes)
library(openxlsx)
library(purrr)
library(stringr)
library(tidyr)
library(usethis)

 use_test()

# List files in the data folder held outside the repository.
file_list <- function(pattern=".*\\.xls[mx]"){
  base_path <- "../not_in_repo/data"
  list.files(base_path, pattern = pattern) |>
    map_chr( \(x) file.path(base_path, x))
}

load_wb_data <- function (filepath){
  tx_raw <- read.xlsx(
    filepath,
    sheet = "Txns"
  )
}

# From raw spreadsheet transactions set up source data and split into
# test, train and unlabelled subsets
# sample_rate can be used to get a randomised subset
get_source_txns <- function (txns_raw,
                             sample_rate=1.0, train_prop=0.8){
  tx1 <- txns_raw |>
    mutate(row = 2:(nrow(txns_raw) + 1))  |>
    filter(Account == 2102) |>
    mutate(nolabel = is.na(Category) | is.na(Type) ) |>
    select(row, Date, Amount, Description, Type, Category, nolabel)

  set.seed(NULL)
  buckets <- c("train", "test", "drop")
  sample <- sample( buckets, nrow(tx1), replace=TRUE,
                    prob=c( sample_rate * train_prop,
                            sample_rate * (1 - train_prop),
                            1 - sample_rate
                    ))

  source_txn <- list (
    "train" = tx1[ !tx1$nolabel & sample == "train", ],
    "test" = tx1[ !tx1$nolabel & sample == "test", ],
    "nolabel" = tx1[ tx1$nolabel & sample != "drop",  ]
  )
}

prep_data <- function (raw) {
  prep <-  raw |>
    mutate(class = paste(Type, Category, sep = "-"))  |>
    mutate(words = str_split(str_squish(Description), " "))

  prep$features <- make_word_incidence_table(prep$words)
  return(prep)
}

make_word_incidence_table <- function (words) {
  word_list <- unique(unlist(words)) |>
    keep( \(w) str_length(w) > 0)

  target_in_words <- function(target, fwords) {
    map_lgl(fwords, \(ws) target %in% ws )
  }

  map( word_list, \(t) target_in_words(t, words) ) |>
    set_names(word_list) |>
    as_tibble()
}

make_model_1 <- function (training_data) {
  model <- naive_bayes(x = training_data$features,
                       y = training_data$class, laplace = 1)
}

# Tabulate results of predictionss vs actuals, row in original table
make_test_results_table <- function(test, pred) {
   table <- tibble(
     row = test$row,
     actual = test$class,
     predicted = pred$class,
     correct = actual == predicted ,
     prob = imap_dbl(pred$class, \(c,i) pred$prob[i,c]),
     description = test$Description
   )
}

# Calculate cumulative performance stats for a given probability p.
cum_test_results <- function(res, p) {
  res |>
    filter(prob >= p) |>
    summarise(prob = p,
              covered = n(),
              right = sum(correct),
              wrong = covered - right,
              accuracy = mean(correct),
              coverage = covered/length(res[[1]]),
              discovery_rate = right/length(res[[1]])
            )
}

# Make table for model performance for range of probabilities.
make_performance_table <- function(res) {
  map(seq(from=.01 ,to = .99, length.out = 100), ~ cum_test_results(res, .x)) |>
    list_rbind()
}

# 'Extract key performance measures
# 'For two accuracy levels and the overall accuracy, give discovery and
# coverage rate.
#'
#' @param perf_table
#'
#' @return
#' @export
#'
#' @examples
get_perf_measures <- function (perf_table) {
  accuracy_breakpoints <- c(0.9, 0.8, first(perf_table$accuracy))
  perf_measures <- list(
    accuracy = accuracy_breakpoints,
    discovery = (approx(x = perf_table$accuracy,
                 y = perf_table$discovery_rate,
                 xout = accuracy_breakpoints) )[[2]],
    coverage = (approx(x = perf_table$accuracy,
                 y = perf_table$coverage,
                 xout = accuracy_breakpoints) )[[2]]
  )
  return(perf_measures)
}

#===================
# Functions which compose function steps.

# Build and test model for givem source file
build_and_test_model <- function(source_file, sample_rate=1.0){
  txns <- load_wb_data(source_file) |>
    get_source_txns(sample_rate)
  model <- make_model(txns$train)
  perf <- test_model(model, txns$test)
}
make_model <- function(txn) {
  txn |>
    prep_data() |>
    make_model_1()
}


#' Test the model
#'
#' @param model
#' @param txn Test features and classes
#'
#' @return List: table of test detailed test results; summary stats
#' @export
#'
#' @examples
test_model <-function(model, txn) {
  test <- prep_data(txn)
  pred_class <- predict(model, test$features, type = "class" )
  pred_prob <- predict(model, test$features, type =  "prob" )
  pred <- list(class = pred_class, prob = pred_prob)
  results_table <- make_test_results_table (test, pred)
  performance_table <- make_performance_table (results_table)
  performance_measure <- get_perf_measures(performance_table)
  performance <- list(table = performance_table, measure = performance_measure)
}
