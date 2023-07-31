library(dplyr)
library(naivebayes)
library(openxlsx)
library(purrr)
library(stringr)
library(tidyr)
library(usethis)

# use_test()

load_wb_data <- function (filepath){
  tx_raw <- read.xlsx(
    filepath,
    sheet = "Txns"
  )
}

get_source_txns <- function (txns_raw, sample_rate=1.0){
  train_prop = 0.8
  tx1 <- txns_raw |>
    mutate(row = 2:(length(txns_raw[[1]]) + 1) ) |>
    filter(Account == 2102) |>
    filter(!is.na(Category)) |>
    select(row, Date, Amount, Description, Type, Category)

  set.seed(1)
  buckets <- c("train", "test", "drop")
  sample <- sample( buckets, nrow(txns_raw), replace=TRUE,
                    prob=c( sample_rate * train_prop,
                            sample_rate * (1 - train_prop),
                            1 - sample_rate
                    ))

  source_txn <- list (
    "train"  = tx1[sample=="train", ],
    "test" = tx1[sample=="test", ]
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
    keep( \(w) str_length(w) > 0)

  target_in_words <- function(target, fwords) {
    map_lgl(fwords, \(ws) target %in% ws )
  }

  map( word_list, \(t) target_in_words(t, words) ) |>
    set_names(word_list) |>
    as_tibble()
}

make_model_1 <- function (features, class) {
  model <- naive_bayes(x = features, y = class, laplace = 1)
}

make_results_table <- function(test, pred) {
   table <- tibble(
     row = test$row,
     actual = test$class,
     predicted = pred$class,
     correct = actual == predicted ,
     prob = imap_dbl(pred$class, \(c,i) pred$prob[i,c]),
     description = test$Description
   )
}
