parse_csv_page <- function(csv_page) {
  meta_length <- 7

  csv_str <- stringr::str_c(csv_page, collapse = "\n")

  meta <- parse_meta(csv_page, meta_length)
  meta_dots <- extract_meta_dots(meta)

  col_names <- readr::read_csv(csv_str, skip = meta_length, n_max = 0) %>%
    names()
  i <- col_names != "[EMPTY]"
  col_names <- col_names[i]
  index_name <- col_names[1]
  col_names[1] <- "index"

  col_types = rep("n", length(i))
  col_types[1] <- "c"
  col_types[!i] <- "_"
  col_types <- stringr::str_c(col_types, collapse = "")

  df <- readr::read_csv(csv_str, col_names = col_names, col_types = col_types,
                        na = "?", skip = meta_length + 1) %>%
    tidyr::gather_("key", "value", col_names[-1]) %>%
    dplyr::mutate_(~index_name, .dots = meta_dots) %>%
    dplyr::select_(~index, ~index_name, ~everything())
  df
}

parse_meta <- function(csv_page, meta_length) {
  meta <- stringr::str_c(csv_page, collapse = "\n") %>%
    readr::read_csv(col_names = FALSE, n_max = meta_length) %>%
    t() %>%
    as.vector() %>%
    na.omit()
  i <- meta != ""
  meta[i]
}
