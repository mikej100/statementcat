library(tidyverse)
library(ggplot2)
library(logger)
library(testthat)
library(usethis)

log_appender(appender_file(test_path("../../log/log.txt")))
testflag_synth_data <- FALSE

test_that("train naive Bayes model",{
  model <<- make_model_1(train)
  # expect word in bbag of words to have probability less than 1.
  expect_lte( tables(model, which=(names(train$features[1])) )[[1]][1], 1.0)
})

test_that("Predict from test data ",{
  test <<- prep_data(txns$test)
  pred <<- make_predictions(model, test)
  expect_gt (sum(pred$class == test$class)/ length(test$class), 0.18,
             "Prediction accuracy")
})
test_that("Build test results table",{
  results  <<- test_model(model, test)
  # Expect a number of probabilities in the results table
  expect_gt (length(results$table$prob), exp_min_train *.8 * sample_rate  )
  # Expect overall discovery rate in range 0.2 to 1
  overall_discovery_rate <- last( results$measure$accuracy)
  expect_gt(overall_discovery_rate, 0.2)
  expect_lt(overall_discovery_rate, 0.9)
})


test_that("train and predict with all labelled data",{
  source_file <-  last( file_list("../not_in_repo/data", pattern="0930") )
  txns <- get_source_txns(source_file, sample_rate=1, train_prop = 1)
  train <<- prep_data(txns$train, word_length = 4)
  model <<- make_model_1(train, laplace = 0.1)
  pred <<- make_predictions(model, train)
  perf <- measure_model (pred, train)
  plot_results_by_accuracy(perf)
  plot_results_by_prob(perf)
})

wrongungs <- perf$results_table |>
  filter(correct == FALSE) |>
  filter(prob >= 0.5)

test_that("train and test predict with labelled data",{
  source_file <-  last( file_list("../not_in_repo/data", pattern="0930") )
  txns <<- get_source_txns(source_file, sample_rate=1, train_prop = 0.8)
  word_length <- 1
  train <<- prep_data(txns$train, word_length = word_length)
  test <<- prep_data(txns$test, word_length = word_length)
  model <<- make_model_1(train, laplace = 0.1)
  pred <<- make_predictions(model, test)
  perf <- measure_model (pred, test)
  plot_results_by_accuracy(perf)
  plot_results_by_prob(perf)
})

# Optimise the laplace ----
# On 2023-11-08, the optimal value shown to be in range 0.03- 0.1
metrics <- map( seq(0, 0.5, 0.03), \(l) {
    make_model_1(train, laplace = l) |>
  make_predictions(test) |>
  measure_model (test) |>
    pluck("metrics") |>
    append(list(laplace = l))
})

mt <- transpose(metrics)
laplace <- unlist( mt$laplace )
acc_overall <- map_dbl(mt$accuracy, \(m) m[[3]])
laplace_table <- tibble(laplace, acc_overall)


  ggplot(laplace_table, aes (x=laplace ) ) +
    geom_line( aes(y=acc_overall), color = "blue") +
#    geom_line( aes(y=coverage), color = "green") +
#    geom_line( aes(y=accuracy), color = "red") +
    labs(title="Effect of Laplace on accuracy") +
    theme(legend.position = "bottom")

# optimise minimum word length ----
# 2023-11-08 optimal value shown to be 1 or 4, by  small margin
  source_file <-  last( file_list("../not_in_repo/data", pattern="0930") )
  txns <<- get_source_txns(source_file, sample_rate=1, train_prop = 0.8)
  metrics_wl <- map (seq(1,6), \(wl) {
    train <- prep_data(txns$train, word_length = wl)
    test <- prep_data(txns$test, word_length = wl)
    train |>
      make_model_1(laplace = 0.1) |>
      make_predictions(test) |>
      measure_model (test) |>
      pluck("metrics") |>
      append(list(word_length = wl))
  }
  )

test_that("train and test predict  unlabelled data",{
  source_file <-  last( file_list("../not_in_repo/data", pattern="0930") )
  txns <- get_source_txns(source_file, sample_rate=1, train_prop = 1)
  word_length <- 4
  train <- prep_data(txns$train, word_length = word_length)
  predict <- prep_data(txns$nolabel, word_length = word_length)
  model <- make_model_1(train, laplace = 0.1)
  pred <- make_predictions(model, predict)
})



