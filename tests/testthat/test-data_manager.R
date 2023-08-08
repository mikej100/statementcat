library(testthat)
library(ggplot2)
library(usethis)

# use_r()


file_list <- function(){
  base_path <- "../not_in_repo"
  list.files(base_path) |>
    map_chr( \(x) file.path(base_path, x))
}




test_that("Reads transactions from test file", {
  source_file <- file_list()[str_detect(file_list(), "0803")]
#  source_file <- file_list()[str_detect(file_list(), "0516")]
  df <- load_wb_data(source_file)
  txns <<- get_source_txns(df)
  expect_gt(length(txns$train$Date), 1000)
})

# test_that("Load subset of transactions from test file", {
#   source_file <- "../not_in_repo/Accounting_20230516.xlsm"
#   df <- load_wb_data(source_file)
#   txns <<- get_source_txns(df, 0.2)
#   expect_gt(length(txns$train$Date), 100)
#   expect_lt(length(txns$train$Date), 1000)
# })

test_that("Prepare data",{
  train <<- prep_data(txns$train)
  expect_gt(length(train$class), 50)
  expect_gt(length(train$words), 50)
})

test_that("Create table of word incidence",{
  incidence_table <<- make_word_incidence_table(train$words)
  expect_gt( length(incidence_table), 0.9 * length(unique(train$class)) )
  expect_equal(length(incidence_table[[1]]),  length(train$class))
})

test_that("train naive Bayes model",{
  train_class <- train$class
  model <<- make_model_1(incidence_table, train_class)
  expect_lte( tables(model, which=(names(incidence_table[1])) )[[1]][1], 1.0)
})

test_that("Predict from test data ",{
  test <<- prep_data(txns$test)
  test$features <- make_word_incidence_table(test$words)
  pred_class <- predict(model, test$features, type = "class" )
  pred_prob <- predict(model, test$features, type =  "prob" )
  pred <<- list(class = pred_class, prob = pred_prob)
  expect_gt (sum(pred$class == test$class)/ length(test$class), 0.5,
             "Prediction accuracy")
})
test_that("Build results table",{
  results_table <<- make_results_table (test, pred)
  ggplot(results, aes(x=prob, fill=correct) ) +
    geom_histogram(bins=9) +
    coord_cartesian(xlim=c(0.5,1), ylim=c(0, length(results$prob)))
})
results_perf <- make_performance_table (results)

  ggplot(results_ana, aes (x=prob ) ) +
    geom_line( aes(y=discovery_rate), color = "blue") +
    geom_line( aes(y=coverage), color = "green") +
    geom_line( aes(y=accuracy), color = "red") +
    labs(title="Performance against probability cut-off") +
    theme(legend.position = "bottom")

  ggplot(d2, aes (x=accuracy ) ) +
    geom_line( aes(y=discovery_rate), color = "blue") +
    geom_line( aes(y=coverage), color = "green") +
    labs(title="Discovery rate and coverage vs accuracy")


hiprob_misses <- results |> filter(prob > 0.94, correct==FALSE )
midprob_misses <- results |> filter(prob > 0.7, prob<0.94,   correct==FALSE )
midprob_quantile <- results |> filter(prob > 0.7, correct==FALSE ) |>
  summarise(rows = n(), hits = sum(correct))
loprob_misses <- results |> filter(prob < 0.5, correct==FALSE )
loprob_all <- results |> filter(prob < 0.5)

source_files <- file_list()[str_detect(file_list(), "Accounting")]
measure <- build_measure_model(source_files[1])
file_versions_measures <- map(source_files, ~ build_measure_model(.x))
