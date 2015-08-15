parse_csv_page <- function(csv_page) {
  if (identical(csv_page, "")) return(NULL)

  meta_length <- 7

  meta <- parse_meta(csv_page, meta_length)
  meta_dots <- extract_meta_dots(meta)

  col_names <- readr::read_csv(csv_page, skip = meta_length, n_max = 0) %>%
    names()

  Encoding(col_names) <- "UTF-8"

  i <- col_names != "[EMPTY]"
  col_names <- col_names[i]
  index_name <- col_names[1]
  col_names[1] <- "index"

  col_types = rep("n", length(i))
  col_types[1] <- "c"
  col_types[!i] <- "_"
  col_types <- stringr::str_c(col_types, collapse = "")

  df <- readr::read_csv(csv_page, col_names = col_names, col_types = col_types,
                        na = "?", skip = meta_length + 1)

  Encoding(df$index) <- "UTF-8"

  df <- df %>%
    tidyr::gather_("key", "value", col_names[-1]) %>%
    dplyr::mutate_(~index_name, .dots = meta_dots) %>%
    dplyr::select_(~index, ~index_name, ~everything())

  df
}

parse_meta <- function(csv_page, meta_length) {
  meta <- csv_page %>%
    readr::read_csv(col_names = FALSE, n_max = meta_length) %>%
    t() %>%
    as.vector() %>%
    na.omit()

  i <- meta != ""
  meta <- meta[i]

  Encoding(meta) <- "UTF-8"

  meta
}

parse_trafikkverdier <- function(df) {
  checkmate::assertDataFrame(df, ncols = 15)

  df <- df %>%
    dplyr::rename_(Trafikktype = ~key) %>%
    dplyr::mutate_(Aar = ~as.integer(index_name),
                   index = ~factor(index, unique(index)),
                   Periode = ~meta18 %>%
                     stringr::str_replace_all(" \\d{4}", "")) %>%
    dplyr::select_(~-index_name, ~-matches("^meta|^(sub)?type$")) %>%
    dplyr::select_(~Aar, ~everything()) %>%
    tidyr::spread_("index", "value")

  df <- df %>%
    dplyr::rename_("\u00c5r" = ~Aar)

  df
}

parse_produksjon <- function(df, total) {
  checkmate::assertDataFrame(df, ncols = 14)

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
                   Dato = ~stringr::str_c(Aar, "-", as.integer(Maaned),
                                          "-01") %>% as.Date()) %>%
    dplyr::select_(~-index_name, ~-matches("^meta|^(sub)?type$")) %>%
    dplyr::select_(~Aar, ~Maaned, ~everything()) %>%
    tidyr::spread_("key", "value")

  df <- df %>%
    dplyr::rename_("\u00c5r" = ~Aar,
                   "M\u00e5ned" = ~Maaned)

  df
}
