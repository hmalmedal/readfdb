parse_trafikkindeks_aarsindeks <- function(df, total) {
  type <- unique(df$type)
  begrenset <- identical(type, "Begrenset trafikkindeks")

  if (total) {
    df <- df %>%
      dplyr::filter(.data$index == "Total")
  } else {
    df <- df %>%
      dplyr::filter(.data$index != "Total")
  }

  df <- df %>%
    dplyr::rename(Maaned = .data$index) %>%
    dplyr::mutate(Basisaar = .data$index_name %>%
                    stringr::str_extract("^\\d{4}") %>%
                    as.integer(),
                  Indeksaar = .data$index_name %>%
                    stringr::str_extract("\\d{4}$") %>%
                    as.integer(),
                  Maaned = factor(.data$Maaned, maaneder),
                  Basisdato = lubridate::make_date(.data$Basisaar,
                                                   .data$Maaned),
                  Indeksdato = lubridate::make_date(.data$Indeksaar,
                                                    .data$Maaned))

  if (begrenset) {
    df <- df %>%
      tidyr::separate(.data$meta17, c("mkey", "mvalue"), sep = ": ") %>%
      dplyr::mutate(mvalue = readr::parse_number(.data$mvalue))
  }

  df <- df %>%
    dplyr::select(-.data$index_name, -matches("^meta|^(sub)?type$")) %>%
    dplyr::select(.data$Basisaar, .data$Indeksaar, .data$Maaned,
                  everything()) %>%
    tidyr::spread(.data$key, .data$value)

  if (begrenset) {
    df <- df %>%
      tidyr::spread(.data$mkey, .data$mvalue)
  }

  df <- df %>%
    dplyr::arrange(.data$Basisdato)

  df <- df %>%
    dplyr::rename("Basis\u00e5r" = .data$Basisaar,
                  "Indeks\u00e5r" = .data$Indeksaar,
                  "M\u00e5ned" = .data$Maaned)

  df
}

parse_trafikkindeks_kvartalsindeks <- parse_trafikkindeks_aarsindeks

parse_trafikkindeks_siste_12_maaneder <- function(df, total) {
  type <- unique(df$type)
  begrenset <- identical(type, "Begrenset trafikkindeks")

  if (total) {
    df <- df %>%
      dplyr::filter(.data$index == "Total")
  } else {
    df <- df %>%
      dplyr::filter(.data$index != "Total")
  }

  df <- df %>%
    tidyr::separate(.data$index, c("Maaned", "Basisaar", "Indeksaar"),
                     sep = " |/", extra = "drop") %>%
    dplyr::mutate(Maaned = factor(.data$Maaned, maaneder),
                  Basisaar = as.integer(.data$Basisaar) + 2000,
                  Indeksaar = as.integer(.data$Indeksaar) + 2000,
                  Basisdato = lubridate::make_date(.data$Basisaar,
                                                   .data$Maaned),
                  Indeksdato = lubridate::make_date(.data$Indeksaar,
                                                    .data$Maaned))

  if (begrenset) {
    df <- df %>%
      tidyr::separate(.data$meta17, c("mkey", "mvalue"), sep = ": ") %>%
      dplyr::mutate(mvalue = readr::parse_number(.data$mvalue))
  }

  df <- df %>%
    dplyr::select(-.data$index_name, -matches("^meta|^(sub)?type$")) %>%
    dplyr::select(.data$Basisaar, .data$Indeksaar, .data$Maaned,
                  everything()) %>%
    tidyr::spread(.data$key, .data$value)

  if (begrenset) {
    df <- df %>%
      tidyr::spread(.data$mkey, .data$mvalue)
  }

  df <- df %>%
    dplyr::arrange(.data$Basisdato)

  df <- df %>%
    dplyr::rename("Basis\u00e5r" = .data$Basisaar,
                  "Indeks\u00e5r" = .data$Indeksaar,
                  "M\u00e5ned" = .data$Maaned)

  df
}
