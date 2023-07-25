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
  train <<- txns$train
})

test_that

test_that("tokenise the description text",{

})
