parse_csv_page <- function(csv_page) {
  if (identical(csv_page, "")) return(NULL)

  meta_length <- 7

  meta <- parse_meta(csv_page, meta_length)
  meta_dots <- extract_meta_dots(meta)

  df <- read.csv(text = csv_page, na.strings = "?", colClasses = "character",
                 skip = meta_length, check.names = FALSE, encoding = "UTF-8")
  df <- dplyr::tbl_df(df[names(df) != ""])

  index_name <- names(df)[1]
  names(df)[1] <- "index"

  df <- df %>%
    tidyr::gather_("key", "value", names(df)[-1], factor_key = TRUE) %>%
    dplyr::mutate_(value = ~stringr::str_replace_all(value, ",", "") %>%
                     as.numeric()) %>%
    dplyr::mutate_(~index_name, .dots = meta_dots) %>%
    dplyr::select_(~index, ~index_name, ~dplyr::everything())

  df
}

parse_meta <- function(csv_page, meta_length) {
  meta <- csv_page %>%
    readr::tokenize(n_max = meta_length) %>%
    purrr::flatten_chr()

  i <- meta != "[EMPTY]"
  meta <- meta[i]

  Encoding(meta) <- "UTF-8"

  meta
}

parse_trafikkverdier <- function(df) {
  df <- df %>%
    dplyr::rename_(Trafikktype = ~key) %>%
    dplyr::mutate_(Aar = ~as.integer(index_name),
                   index = ~forcats::fct_inorder(index),
                   Maanedsintervall = ~meta18 %>%
                     stringr::str_replace_all(" \\d{4}", "")) %>%
    dplyr::select_(~-index_name, ~-dplyr::matches("^meta|^(sub)?type$")) %>%
    dplyr::select_(~Aar, ~dplyr::everything()) %>%
    tidyr::spread_("index", "value")

  df <- df %>%
    dplyr::rename_("\u00c5r" = ~Aar,
                   "M\u00e5nedsintervall" = ~Maanedsintervall)

  df
}

parse_produksjon <- function(df, total) {
  if (total) {
    df <- df %>%
      dplyr::filter_(~index == "Total")
  } else {
    df <- df %>%
      dplyr::filter_(~index != "Total")
  }

  df <- df %>%
    dplyr::rename_(Maaned = ~index) %>%
    dplyr::mutate_(Aar = ~as.integer(index_name),
                   Maaned = ~Maaned %>%
                     stringr::str_extract("^\\w{3}") %>%
                     factor(maaneder),
                   Dato = ~lubridate::make_date(Aar, Maaned)) %>%
    dplyr::select_(~-index_name, ~-dplyr::matches("^meta|^(sub)?type$")) %>%
    dplyr::select_(~Aar, ~Maaned, ~dplyr::everything()) %>%
    tidyr::spread_("key", "value")

  df <- df %>%
    dplyr::rename_("\u00c5r" = ~Aar,
                   "M\u00e5ned" = ~Maaned)

  df
}

parse_sonefordeling <- function(df, total) {
  if (total) {
    df <- df %>%
      dplyr::filter_(~index == "Total") %>%
      dplyr::mutate_(index = NA)
  } else {
    df <- df %>%
      dplyr::filter_(~index != "Total")
  }

  df <- df %>%
    dplyr::rename_(Sone = ~index) %>%
    dplyr::mutate_(Sone = ~as.integer(Sone),
                   Aar = ~meta17 %>%
                     stringr::str_extract("\\d{4}") %>%
                     as.integer(),
                   Maanedsintervall = ~meta17 %>%
                     stringr::str_replace_all(" \\d{4}", "")) %>%
    dplyr::select_(~-index_name, ~-dplyr::matches("^meta|^(sub)?type$")) %>%
    dplyr::select_(~Aar, ~dplyr::everything()) %>%
    tidyr::spread_("key", "value")

  df <- df %>%
    dplyr::rename_("\u00c5r" = ~Aar,
                   "M\u00e5nedsintervall" = ~Maanedsintervall)

  df
}
