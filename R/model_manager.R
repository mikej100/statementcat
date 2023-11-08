library(tidyverse)
library(naivebayes)
library(openxlsx)
library(usethis)

make_model_1 <- function (training_data, laplace=1) {
  model <- naive_bayes(x = training_data$features,
                       y = training_data$class, laplace = laplace
                       )
}

make_predictions <- function(model, to_predict) {
  pred_table <- array_tree( predict(model, to_predict$features, type =  "prob" ) )
  pred_class <- map_chr(pred_table, \(r) names(which.max(r)))
  pred_prob <- map_dbl(pred_table, \(r) max(unlist(r)))
  pred <- tibble (row=to_predict$row, orig_desc=to_predict$Description,
                  pred_class, pred_prob)
}

# Tabulate results of predictionss vs actuals, row in original table
make_test_results_table <- function(test, pred) {
   table <- tibble(
     row = test$row,
     actual = test$class,
     predicted = pred$pred_class,
     correct = actual == predicted ,
     prob = pred$pred_prob,
     description = test$Description
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
get_perf_metrics <- function (perf_table) {
  accuracy_breakpoints <- c(0.9, 0.8, first(perf_table$accuracy))
  perf_metrics <- list(
    accuracy = accuracy_breakpoints,
    discovery = (approx(x = perf_table$accuracy,
                 y = perf_table$discovery_rate,
                 xout = accuracy_breakpoints) )[[2]],
    coverage = (approx(x = perf_table$accuracy,
                 y = perf_table$coverage,
                 xout = accuracy_breakpoints) )[[2]]
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


# Functions which compose function steps ----

# Build and test model for givem source file
build_and_test_model <- function(source_file, sample_rate=1.0){
  txns <- load_wb_data(source_file) |>
    get_source_txns(sample_rate)
  model <- make_model(txns$train)
  perf <- test_model(model, prep_data(txns$test))
}


#' Test the model
#'
#' @param model model to test
#' @param txn transactions to use in the test, already processed by prep_data
#'
#' @return List: table of test detailed test results; summary stats
#' @export
#'
#' @examples
measure_model <-function(pred, test) {
  results_table <- make_test_results_table (test, pred)
  performance_table <- make_performance_table (results_table)
  performance_metrics <- get_perf_metrics(performance_table)
  performance <- list(results_table = results_table,
                      perf_table = performance_table,
                      metrics = performance_metrics)
}


#' Plot performance against accuracy
#'
#' For a given minimum accuracy of prediction, show the rate of discovery of
#' good predictions (blue) and the overal coverage of all the data (green).
#'
#' @param results Results table and metrics calculated by test_model function
#' @param subtitle Optional subtitle to identify the test conditions
#'
#' @return Generates plot
#' @export
#'
#' @examples
plot_results_by_accuracy <- function(results, subtitle="") {
  ggplot(results$perf_table, aes (x=accuracy ) ) +
    geom_line( aes(y=discovery_rate), color = "blue") +
    geom_line( aes(y=coverage), color = "green") +
    labs(title="Discovery rate and coverage vs accuracy",
         subtitle = subtitle)
}

#' Plot performance against probability cut-off
#'
#' For a given cut-off value for prediction probability,
#' show the rate of discovery of
#' good predictions (blue) and the overal coverage of all the data (green)
#' and the accuracy of the results found.
#'
#' @param results Results table and metrics calculated by test_model function
#' @param subtitle Optional subtitle to identify the test conditions
#'
#' @return Generates plot
#' @export
#'
#' @examples
plot_results_by_prob <- function(results, subtitle="") {
  ggplot(results$perf_table, aes (x=prob ) ) +
    geom_line( aes(y=discovery_rate), color = "blue") +
    geom_line( aes(y=coverage), color = "green") +
    geom_line( aes(y=accuracy), color = "red") +
    labs(title="Performance against probability cut-off",
         subtitle = subtitle) +
    theme(legend.position = "bottom")
}

ggplot()
