library(testthat)
library(usethis)
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

use_r()

test_that("Reads transactions from test file", {
  source_file <- "../not_in_repo/Accounting_20230516.xlsm"
  df <- load_wb_data(source_file)
  txns <<- get_source_txns(df)
  expect_gt(length(txns$train$Date), 1000)
})

test_that("Load subset of transactions from test file", {
  source_file <- "../not_in_repo/Accounting_20230516.xlsm"
  df <- load_wb_data(source_file)
  txns <<- get_source_txns(df, 0.2)
  expect_gt(length(txns$train$Date), 100)
  expect_lt(length(txns$train$Date), 1000)
})

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

test_that("Predict from test data ")
  test <- prep_data(txns$test)
  test_features <- make_word_incidence_table(test$words)
  pred <- predict(model, test_features, type = "class")
  prob <- predict(model, test_features, type = "prob")
results_table <- tibble(
  actual = test$class,
  pred = pred,
  correct = actual == pred,
  prob = imap_dbl(pred, \(p,i) prob[i,p])
)

