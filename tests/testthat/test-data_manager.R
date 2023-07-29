library(testthat)
library(usethis)
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

use_r()

test_that("Reads transactions from test file", {
  source_file <- "../not_in_repo/Accounting_20230516.xlsm"
  df <- load_wb_data(source_file)
  txns <- get_source_txns(df)
  expect_gt(length(txns$train$Date), 1000)
  train_raw <<- txns$train
})

test_that("Prepare data",{
  train <<- prep_data(train_raw)
  expect_gt(length(train$class), 50)
  expect_gt(length(train$words), 50)
})

word_list <- unique(unlist(train$words)) |>
  keep( ~ str_length(.x) > 0)

word_in_descr <- function(d, w) {
  any(d, ~ str_match(.x, fixed(w)) )
}


words_df <- map(word_list, ~ ), fixed(.x)) ) |>
  set_names(word_list) |>
  as_tibble()

test_that("train naive Bayes model",{
  model <- make_model_1(train$class, train$words)
})
