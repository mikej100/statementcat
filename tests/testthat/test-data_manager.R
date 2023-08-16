library(tidyverse)
library(ggplot2)
library(logger)
library(testthat)
library(usethis)

# use_r()

log_appender(appender_file(test_path("../../log/log.txt")))
# log_info( " test-data_manager: getwd {getwd()}" )
# log_info( " test-data_manager: test_path {test_path()}" )



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

test_that("Reads transactions from test file, and splits into subsets", {
  source_file <- file_list( test_path("temp_dir"))
#  source_file <- file_list("../not_in_repo/data",  "0803")
#  source_file <- file_list()[str_detect(file_list(), "0516")]
  df <- load_wb_data(last(source_file))
  sample_rate <<- 0.1
  exp_min_train <<- 999
  tx1 <- get_source_txns(df, sample_rate, train_prop = 1)
  expect_gt(nrow(tx1$train), exp_min_train * sample_rate)
  expect_lt(nrow(tx1$test), 1)
  expect_gt(nrow(tx1$nolabel), exp_min_train * sample_rate)

  txns <<- get_source_txns(df, sample_rate )
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

skip("make_word_incidence table tested in Prepare data test")
test_that("Create table of word incidence",{
  # incidence_table <<- make_word_incidence_table(train$words)
  expect_gt( length(train$features), 0.9 * length(unique(train$class)) )
  expect_equal(length(train$features[[1]]),  length(train$class))
})

test_that("train naive Bayes model",{
  model <<- make_model_1(train)
  # expect word in bbag of words to have probability less than 1.
  expect_lte( tables(model, which=(names(train$features[1])) )[[1]][1], 1.0)
})

test_that("Predict from test data ",{
  test <<- prep_data(txns$test)
  pred_class <- predict(model, test$features, type = "class" )
  pred_prob <- predict(model, test$features, type =  "prob" )
  pred <<- list(class = pred_class, prob = pred_prob)
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


test_that("Put it all together: from file to test results",{
  source_file <- last( file_list(pattern = "Accounting") )
  results <<- build_and_test_model(source_file, sample_rate = 1)

})

results_set <- map(1:3,
                   \(n) build_and_test_model(source_file, sample_rate=1)) |>
  map(\(x) x$measure)

  ggplot(results$table, aes (x=accuracy ) ) +
    geom_line( aes(y=discovery_rate), color = "blue") +
    geom_line( aes(y=coverage), color = "green") +
    labs(title="Discovery rate and coverage vs accuracy")

  ggplot(results$table, aes (x=prob ) ) +
    geom_line( aes(y=discovery_rate), color = "blue") +
    geom_line( aes(y=coverage), color = "green") +
    geom_line( aes(y=accuracy), color = "red") +
    labs(title="Performance against probability cut-off") +
    theme(legend.position = "bottom")


hiprob_misses <- results |> filter(prob > 0.94, correct==FALSE )
midprob_misses <- results |> filter(prob > 0.7, prob<0.94,   correct==FALSE )
midprob_quantile <- results |> filter(prob > 0.7, correct==FALSE ) |>
  summarise(rows = n(), hits = sum(correct))
loprob_misses <- results |> filter(prob < 0.5, correct==FALSE )
loprob_all <- results |> filter(prob < 0.5)

source_files <- file_list()[str_detect(file_list(), "Accounting")]
measure <- build_measure_model(source_files[1])
file_versions_measures <- map(source_files, ~ build_measure_model(.x))


test_that("train and predict unlabelled data",{
  source_file <- last(source_files, sample_rate=0.1)

})



test_that("filter on df columns and external vectors", {
  n=10
  ext_sample <- rbernoulli(n)
  df <- tibble(
    value = rpois(n,3),
    int_sample = sample( c("train", "test", "drop"), n,
                         replace=TRUE, prob=c(5,2,2))
  )
  df[ext_sample, ]
  ss<-df[df$int_sample == "train", ]
  df[ df$int_sample == "train", ]
  df[ df$int_sample == "train"  && ext_sample, ]
})
