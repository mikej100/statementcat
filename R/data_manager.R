library(tidyverse)
library(naivebayes)
library(openxlsx)
library(usethis)

 #use_test()

generate_synthetic_data <- function (filepath) {
  # dir_path <- system.file("data", package="statementcat")
  n = 4000
  no_label_rate = 0.3
  log_info("generate_sythentic_data function, filepath: {filepath}")

  account <- rep("2102", n)

  date <- sample( seq(as.Date('2022/01/01'), as.Date('2023/06/01'),by="day"),
                  n, replace=TRUE)

  text <- c("WATFORD OXFORD LONDON BARNSTABLE LLOYDS PAYPAL SANTANDER
  GROSNI BARRACUDA NEW YORK RESTAURANT BAR CAFE HOTEL Finch Buzzard AS AN BE GF
  GREAT SMALL MID OVER UNDER BLUE GREY RED GREEN BEIGE MAROON TAU MUON SPINOR
  PAULI ERENFEST NOETHER SOMMERVILLE LOVELACE")
  words <- str_split_1( str_squish(text) , pattern = " ")
  word_count <- rpois(n,3) + 1
  description <- map_chr(
    word_count,
    \(wc) str_flatten(sample(words, wc , replace = T), collapse = " ")
    )

  amount <- rchisq(n, 2) * 10

  types  <- c("Monthly", "Annual")
  type_prob <- c(3, 1)
  type = sample( types, n, p = type_prob, replace = T)

  categories <- c("Bills", "Trips", "Domestic", "Groceries")
  cat_prob <- c(1, 1, 2, 3)
  category <- sample(categories, n, cat_prob, replace=T )

  synth_df <- tibble(
    Account = account,
    Date = date,
    Description = description,
    Amount = amount,
    Type = type,
    Category = category
  )

  mask <- sample(1:n, n * no_label_rate,)
  synth_df$Type[mask] = NA
  synth_df$Category[mask] = NA

  result <- write.xlsx(
    synth_df,
    filepath,
    overwrite = TRUE,
    sheetName = "Txns"
  )


}


# List files paths in the data folder held outside the repository.
file_list <- function(
    dir_path="./data",
    pattern=".*\\.xls[mx]"
    ){
  list.files(dir_path, pattern = pattern) |>
    map_chr( \(x) file.path(dir_path, x))
}

# From raw spreadsheet transactions set up source data and split into
# test, train and unlabelled subsets
# sample_rate can be used to get a randomised subset
#' From raw spreadsheet, set up test, train and unlablled subsets
#'
#' @param txns_raw dataframe of transactions as read from spreadsheet
#' @param since Date since which transactions are retained "yyyy-mm-dd"
#' @param sample_rate Proportion of transactions to include, 1 to use all
#' @param train_prop Proportion of transations to use in training set
#'
#' @return list of three lists of transactions $train, $test, $nolabel
#' @export
#'
#' @examples datasets <- get_source_txs (txns_raw, since="2019-04-01" )
get_source_txns <- function (filepath,
                             since = "2022-04-01",
                             sample_rate=1.0,
                             train_prop=0.8){
  log_info("Source file: {filepath}")

  tx_raw <- read.xlsx(
    filepath,
    sheet = "Txns"
  )
  tx1 <- tx_raw |>
    mutate(row = 2:(nrow(tx_raw) + 1))  |>
    filter(Account == 2102) |>
    mutate(rdate = convertToDate(Date)) |>
    filter(rdate >= as.Date(since)) |>
    mutate(nolabel = is.na(Category) | is.na(Type) ) |>
    select(row, rdate, Amount, Description, Type, Category, nolabel)

  set.seed(NULL)
  buckets <- c("train", "test", "drop")
  sample <- sample( buckets, nrow(tx1), replace=TRUE,
                    prob=c( sample_rate * train_prop,
                            sample_rate * (1 - train_prop),
                            1 - sample_rate
                    ))

  source_txn <- list (
    "train" = tx1[ !tx1$nolabel & sample == "train", ],
    "test" = tx1[ !tx1$nolabel & sample == "test", ],
    "nolabel" = tx1[ tx1$nolabel & sample != "drop",  ]
  )
}

prep_data <- function (raw, word_length = 2) {
  prep <-  raw |>
    mutate(class = paste(Type, Category, sep = "-"))  |>
    mutate(words = str_split(str_squish(Description), " "))

  prep$features <- make_word_incidence_table(prep$words, word_length)
  return(prep)
}

make_word_incidence_table <- function (words, word_length) {
  word_list <- unique(unlist(words)) |>
    keep( \(w) str_length(w) > word_length - 1)

  target_in_words <- function(target, fwords) {
    map_lgl(fwords, \(ws) target %in% ws )
  }

  map( word_list, \(t) target_in_words(t, words) ) |>
    set_names(word_list) |>
    as_tibble()
}

write_predictions <- function(filepath, predictions) {
  predictions1 <- predictions |>
    mutate(Type = str_extract(pred_class, "[^-]+")) |>
    mutate(Category = str_extract(pred_class, "-(.+)", group = 1 )) |>
    select(Type, Category, pred_prob)

  # pred_expanded <- tibble(row = seq(2, last(predictions)$row)) |>
  #   left_join(predictions) |>
  #   select( !row)

  wb <- loadWorkbook(filepath)
  for( i in seq_along(predictions$row)) {
    writeData(wb, sheet="Txns", x = predictions1[i,],
              startCol = 6, startRow = predictions$row[[i]], keepNA = FALSE,
              colNames = FALSE)
  }
  saveWorkbook(wb, filepath, overwrite = TRUE)
}
