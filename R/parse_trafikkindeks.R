parse_trafikkindeks_aarsindeks <- function(df) {
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
                     as.Date()) %>%
    dplyr::select_(~-index_name, ~-matches("^meta|type$")) %>%
    dplyr::select_(~Basisaar, ~Indeksaar, ~Maaned, ~everything()) %>%
    tidyr::spread_("key", "value") %>%
    dplyr::arrange_(~Basisdato)

  names(df)[1] <- "Basis\u00e5r" # Basisår
  names(df)[2] <- "Indeks\u00e5r" # Indeksår
  names(df)[3] <- "M\u00e5ned" # Måned
  df
}

parse_trafikkindeks_kvartalsindeks <- parse_trafikkindeks_aarsindeks
