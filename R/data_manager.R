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

word_in_descr <- function(d, w) {
  any(d, ~ str_match(.x, fixed(w)) )
}

# any (list("AMNESTY", "b"), ~ str_detect("AMNESTY", .x) )
# map(word_list[1:3], ~ any (str_detect(train$words[[1]], fixed(.x) )
# any(train$words[[1]], ~str_match(.x), word_list[[1 ]])
# q <- any(c("A","B"), ~ str_match(.x), "A")

any(train$words[1], ~ .x == "AMNESTY")

word_table <- map( train$words, ~ word_list %in% .x)



make_words_
  map(word_list, ~ ), fixed(.x)) ) |>
  set_names(word_list) |>
  as_tibble()
raw$words


make_model_1 <- function (classes, features) {
  model <- naive_bayes(classes, features, laplace = 1 )
}
