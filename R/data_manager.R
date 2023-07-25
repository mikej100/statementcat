library(openxlsx)
library(usethis)
library(dplyr)

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

