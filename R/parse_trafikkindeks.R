parse_trafikkindeks_aarsindeks <- function(df) {
  type <- unique(df$type)
  begrenset <- identical(type, "Begrenset trafikkindeks")
  if (begrenset) df$meta18 <- NULL

  checkmate::assertDataFrame(df, ncols = 14)

  df <- df %>%
    dplyr::filter_(~index != "Total") %>%
    dplyr::rename_(Maaned = ~index) %>%
    dplyr::mutate_(Basisaar = ~index_name %>%
                     stringr::str_extract("^\\d{4}") %>%
                     as.integer(),
                   Indeksaar = ~index_name %>%
                     stringr::str_extract("\\d{4}$") %>%
                     as.integer(),
                   Maaned = ~factor(Maaned, maaneder),
                   Basisdato = ~stringr::str_c(Basisaar, "-",
                                               as.integer(Maaned), "-01") %>%
                     as.Date(),
                   Indeksdato = ~stringr::str_c(Indeksaar, "-",
                                                as.integer(Maaned), "-01") %>%
                     as.Date())

  if (begrenset) {
    df <- df %>%
      tidyr::separate_("meta17", c("mkey", "mvalue"), sep = ": ") %>%
      dplyr::mutate_(mvalue = ~readr::parse_numeric(mvalue))
  }

  df <- df %>%
    dplyr::select_(~-index_name, ~-matches("^meta|type$")) %>%
    dplyr::select_(~Basisaar, ~Indeksaar, ~Maaned, ~everything()) %>%
    tidyr::spread_("key", "value") %>%
    dplyr::arrange_(~Basisdato)

  if (begrenset) {
    df <- df %>%
      tidyr::spread_("mkey", "mvalue") %>%
      dplyr::arrange_(~Basisdato)
  }

  names(df)[1] <- "Basis\u00e5r" # Basisår
  names(df)[2] <- "Indeks\u00e5r" # Indeksår
  names(df)[3] <- "M\u00e5ned" # Måned
  df
}

parse_trafikkindeks_kvartalsindeks <- parse_trafikkindeks_aarsindeks

parse_trafikkindeks_siste_12_maaneder <- function(df) {
  type <- unique(df$type)
  begrenset <- identical(type, "Begrenset trafikkindeks")
  if (begrenset) df$meta18 <- NULL

  checkmate::assertDataFrame(df, ncols = 14)

  df <- df %>%
    dplyr::filter_(~index != "Total") %>%
    tidyr::separate_("index", c("Maaned", "Basisaar", "Indeksaar"),
                     sep = " |/") %>%
    dplyr::mutate_(Maaned = ~factor(Maaned, maaneder),
                   Basisaar = ~as.integer(Basisaar) + 2000,
                   Indeksaar = ~as.integer(Indeksaar) + 2000,
                   Basisdato = ~stringr::str_c(Basisaar, "-",
                                               as.integer(Maaned), "-01") %>%
                     as.Date(),
                   Indeksdato = ~stringr::str_c(Indeksaar, "-",
                                                as.integer(Maaned), "-01") %>%
                     as.Date())

  if (begrenset) {
    df <- df %>%
      tidyr::separate_("meta17", c("mkey", "mvalue"), sep = ": ") %>%
      dplyr::mutate_(mvalue = ~readr::parse_numeric(mvalue))
  }

  df <- df %>%
    dplyr::select_(~-index_name, ~-matches("^meta|type$")) %>%
    dplyr::select_(~Basisaar, ~Indeksaar, ~Maaned, ~everything()) %>%
    tidyr::spread_("key", "value") %>%
    dplyr::arrange_(~Basisdato)

  if (begrenset) {
    df <- df %>%
      tidyr::spread_("mkey", "mvalue") %>%
      dplyr::arrange_(~Basisdato)
  }

  names(df)[1] <- "Basis\u00e5r" # Basisår
  names(df)[2] <- "Indeks\u00e5r" # Indeksår
  names(df)[3] <- "M\u00e5ned" # Måned
  df
}
