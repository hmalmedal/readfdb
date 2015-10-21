parse_csv_page <- function(csv_page) {
  if (identical(csv_page, "")) return(NULL)

  meta_length <- 7

  meta <- parse_meta(csv_page, meta_length)
  meta_dots <- extract_meta_dots(meta)

  i <- readr::tokenize(csv_page)[[meta_length + 1]] == "[EMPTY]"

  col_types <- rep("n", length(i))
  col_types[1] <- "c"
  col_types[i] <- "_"
  col_types <- stringr::str_c(col_types, collapse = "")

  df <- readr::read_csv(csv_page, col_types = col_types, na = "?",
                        skip = meta_length, progress = FALSE)

  index_name <- names(df)[1]
  names(df)[1] <- "index"

  df <- df %>%
    tidyr::gather_("key", "value", names(df)[-1]) %>%
    dplyr::mutate_(~index_name, .dots = meta_dots) %>%
    dplyr::select_(~index, ~index_name, ~everything())

  df
}

parse_meta <- function(csv_page, meta_length) {
  meta <- csv_page %>%
    readr::tokenize(n_max = meta_length) %>%
    unlist()

  i <- meta != "[EMPTY]"
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
