library(testthat)
library(ggplot2)
library(usethis)
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# use_r()

test_that("Reads transactions from test file", {
  source_file <- "../not_in_repo/Accounting_20230731.xlsm"
#  source_file <- "../../../not_in_repo/Accounting_20230731.xlsm"
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
  results <<- make_results_table (test, pred)
  ggplot(results, aes(x=prob, fill=correct) ) +
    geom_histogram(bins=100)

  cum_correct <- function(p) {
    results |> filter(prob > p) |>
      summarise(prob = p,
                right = sum(correct),
                wrong = n() - right,
                mean = mean(correct))
  }
  d2 <- map(seq(from=.9 ,to = .99, length.out = 10), ~ cum_correct(.x) ) |>
    list_rbind()
  ggplot(d2, aes (x=prob, wrong ) ) +
    geom_col()

  uiesuts_cut <- cut_interval(results, 10)
  ggplot(results, aes(x=cut_interval(prob,11), y=n(correct)) )+
    geom_line()
})

hiprob_misses <- results |> filter(prob > 0.94, correct==FALSE )
