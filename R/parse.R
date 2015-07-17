parse_csv_page <- function(csv_page) {
  meta_length <- 7

  csv_str <- stringr::str_c(csv_page, collapse = "\n")

  meta <- parse_meta(csv_page, meta_length)
  checkmate::assertCharacter(meta, len = 19)
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
                        skip = meta_length + 1) %>%
    tidyr::gather_("key", "value", col_names[-1]) %>%
    dplyr::mutate_(~index_name, .dots = meta_dots) %>%
    dplyr::mutate_(meta = ~meta[19])
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

parse_dognvariasjon <- function(df) {
  names(df)[3] <- df$meta[1] %>%
    stringr::str_replace("\\s*\\d{4}$", "")

  df <- df %>%
    dplyr::filter_(~index != "Total") %>%
    dplyr::rename_(Dag = ~key, Kl = ~index) %>%
    dplyr::mutate_(Aar = ~meta %>%
                     stringr::str_extract("\\d{4}$") %>%
                     as.integer(),
                   Uke = ~index_name %>%
                     stringr::str_extract("\\d{1,2}$") %>%
                     as.integer(),
                   Kl = ~factor(Kl)) %>%
    dplyr::select_(~-index_name, ~-meta, ~-type, ~-subtype) %>%
    dplyr::select_(~Aar, ~Uke, ~Dag, ~Kl, ~everything())

  min_uke <- min(df$Uke[df$Uke > 1])
  if (min_uke >= 52) warning("Check week!")
  aar <- df$Aar[1]

  mandag <- stringr::str_c(aar, "-W",
                           stringr::str_pad(min_uke, 2, pad = "0"),
                           "-1") %>%
    ISOweek::ISOweek2date() %>%
    lubridate::ymd()
  i <- min(which(df$Uke == min_uke))
  timer <- lubridate::hours(seq_along(df$Uke) - i)

  df <- df %>%
    dplyr::mutate_(Tid = ~mandag + timer,
                   Dato = ~as.Date(Tid),
                   Ukedato = ~ISOweek::date2ISOweek(Dato)) %>%
    dplyr::filter_(~lubridate::year(Dato) == aar)

  names(df)[1] <- "\u00c5r" # År
  df
}
