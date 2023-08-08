library(dplyr)
library(naivebayes)
library(openxlsx)
library(purrr)
library(stringr)
library(tidyr)
library(usethis)

# use_test()



load_wb_data <- function (filepath){
  tx_raw <- read.xlsx(
    filepath,
    sheet = "Txns"
  )
}

get_source_txns <- function (txns_raw, sample_rate=1.0){
  train_prop = 0.8
  tx1 <- txns_raw |>
    mutate(row = 2:(length(txns_raw[[1]]) + 1) ) |>
    filter(Account == 2102) |>
    filter(!is.na(Category)) |>
    select(row, Date, Amount, Description, Type, Category)

  set.seed(2)
  buckets <- c("train", "test", "drop")
  sample <- sample( buckets, nrow(txns_raw), replace=TRUE,
                    prob=c( sample_rate * train_prop,
                            sample_rate * (1 - train_prop),
                            1 - sample_rate
                    ))

  source_txn <- list (
    "train"  = tx1[sample=="train", ],
    "test" = tx1[sample=="test", ]
  )
}

prep_data <- function (raw) {
  raw |>
    drop_na(Type, Category) |>
    mutate(class = paste(Type, Category, sep = "-"))  |>
    mutate(words = str_split(str_squish(Description), " "))

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

make_model_1 <- function (features, class) {
  model <- naive_bayes(x = features, y = class, laplace = 1)
}
# Tabulate results of predictionss vs actuals, row in original table
make_results_table <- function(test, pred) {
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
cum_results <- function(res, p) {
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
  map(seq(from=.01 ,to = .99, length.out = 100), ~ cum_results(res, .x) ) |>
    list_rbind()
}
# Extract key performance measures. For two accuracy levels give discovery and
# coverage rate.
get_perf_measures <- function (perf_table) {
  accuracy_breakpoints <- c(0.9, 0.8)
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

build_measure_model <- function(source_file){
  txns <- load_wb_data(source_file) |>
    get_source_txns()

  train <- prep_data(txns$train)
  train$features <- make_word_incidence_table(train$words)
  model <- make_model_1(train$features, train$class)

  test <- prep_data(txns$test)
  test$features <- make_word_incidence_table(test$words)
  pred_class <- predict(model, test$features, type = "class" )
  pred_prob <- predict(model, test$features, type =  "prob" )
  pred <- list(class = pred_class, prob = pred_prob)

  results_table <- make_results_table (test, pred)
  performance_table <- make_performance_table (results_table)
  performance_measure <- get_perf_measures(performance_table)
  return (performance_measure)
}
