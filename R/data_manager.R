library(dplyr)
library(naivebayes)
library(openxlsx)
library(purrr)
library(stringr)
library(tidyr)
library(usethis)

use_test()

load_wb_data <- function (filepath){
  tx_raw <- read.xlsx(
    filepath,
    sheet = "Txns"
  )

}

get_source_txns <- function (txns){
  tx1 <- txns |>
    filter(Account == 2102) |>
    filter(!is.na(Category)) |>
    select(Date, Amount, Description, Type, Category)

  set.seed(1)
  train_prop = 0.8
  sample <- sample(c(TRUE, FALSE), nrow(txns), replace=TRUE,
                   prob=c(train_prop, 1 - train_prop))
  # could do improve perf by partitioning in one step
  source_txn <- list (
    "train"  = tx1[sample, ],
    "test" = tx1[!sample, ]
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
    keep( ~ str_length(.x) > 0)

  target_in_words <- function(target, fwords) {
    map(fwords, \(ws) target %in% ws )
  }

  word_table <- map( word_list, \(t) target_in_words(t, words) )
}

make_model_1 <- function (classes, features) {
  model <- naive_bayes(classes, features, laplace = 1 )
}
