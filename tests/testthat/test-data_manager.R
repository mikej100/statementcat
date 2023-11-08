library(tidyverse)
library(ggplot2)
library(logger)
library(testthat)
library(usethis)

# use_r()

log_appender(appender_file(test_path("../../log/log.txt")))
# log_info( " test-data_manager: getwd {getwd()}" )
# log_info( " test-data_manager: test_path {test_path()}" )

testflag_synth_data <- FALSE

# skip_if(! testflag_synth_data)
test_that("Create synthetic data", {
  fname <- paste0("test_data_",strftime(Sys.time(),"%Y%m%dT%H%M"),".xlsx")
  dir_path <- test_path("temp_dir")
  fpath <- file.path(dir_path, fname)
  log_info("test create synthetid data about to generate file {file.path(dir_path, fname)}")
  result <- generate_synthetic_data(file.path(dir_path, fname))

  found <- file_list(dir_path=dir_path, pattern=fname)
  full_path <- file.path(getwd(),found)
  log_info("test create synthetid data filename found{found}")
  df <- load_wb_data(found)
  expect_gt(nrow(df), 1000)
} )

test_that("Reads transactions from transactions file, and splits into subsets", {
  if(testflag_synth_data){
    source_file <- file_list( test_path("temp_dir"))
  } else {
    source_file <- last( file_list("../not_in_repo/data",  "0930") )
  }

  sample_rate <<- 1
  train_prop <<- 1
  exp_min_train <<- 999
#  tx1 <- get_source_txns(source_file, sample_rate, train_prop = 1)
#  expect_gt(nrow(tx1$train), exp_min_train * sample_rate)
#  expect_lt(nrow(tx1$test), 1)
#  expect_gt(nrow(tx1$nolabel), exp_min_train * sample_rate)

  txns <<- get_source_txns(source_file, sample_rate )
  expect_gt(nrow(txns$train), exp_min_train* sample_rate)
  expect_gt(nrow(txns$train), exp_min_train * sample_rate)
  expect_gt(nrow(txns$nolabel), exp_min_train * sample_rate)
  })

test_that("Prepare data",{
  train <<- prep_data(txns$train)
  expect_gt(length(train$class), 50)
  expect_gt(length(train$words), 50)
  expect_gt( length(train$features), 0.9 * length(unique(train$class)) )
  expect_equal(length(train$features[[1]]),  length(train$class))
})


