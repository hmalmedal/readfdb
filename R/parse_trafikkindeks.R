parse_trafikkindeks_aarsindeks <- function(d, total) {
  type <- unique(d$type)
  begrenset <- identical(type, "Begrenset trafikkindeks")

  if (total) {
    d <- d %>%
      dplyr::filter(.data$index == "Total")
  } else {
    d <- d %>%
      dplyr::filter(.data$index != "Total")
  }

  d <- d %>%
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
    d <- d %>%
      tidyr::separate(.data$meta17, c("mkey", "mvalue"), sep = ": ") %>%
      dplyr::mutate(mvalue = readr::parse_number(.data$mvalue))
  }

  d <- d %>%
    dplyr::select(-.data$index_name, -matches("^meta|^(sub)?type$")) %>%
    dplyr::select(.data$Basisaar, .data$Indeksaar, .data$Maaned,
                  everything()) %>%
    tidyr::pivot_wider(names_from = .data$key, values_from = .data$value)

  if (begrenset) {
    d <- d %>%
      tidyr::pivot_wider(names_from = .data$mkey, values_from = .data$mvalue)
  }

  d <- d %>%
    dplyr::arrange(.data$Basisdato)

  d <- d %>%
    dplyr::rename("Basis\u00e5r" = .data$Basisaar,
                  "Indeks\u00e5r" = .data$Indeksaar,
                  "M\u00e5ned" = .data$Maaned)

  d
}

parse_trafikkindeks_kvartalsindeks <- parse_trafikkindeks_aarsindeks

parse_trafikkindeks_siste_12_maaneder <- function(d, total) {
  type <- unique(d$type)
  begrenset <- identical(type, "Begrenset trafikkindeks")

  if (total) {
    d <- d %>%
      dplyr::filter(.data$index == "Total")
  } else {
    d <- d %>%
      dplyr::filter(.data$index != "Total")
  }

  d <- d %>%
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
    d <- d %>%
      tidyr::separate(.data$meta17, c("mkey", "mvalue"), sep = ": ") %>%
      dplyr::mutate(mvalue = readr::parse_number(.data$mvalue))
  }

  d <- d %>%
    dplyr::select(-.data$index_name, -matches("^meta|^(sub)?type$")) %>%
    dplyr::select(.data$Basisaar, .data$Indeksaar, .data$Maaned,
                  everything()) %>%
    tidyr::pivot_wider(names_from = .data$key, values_from = .data$value)

  if (begrenset) {
    d <- d %>%
      tidyr::pivot_wider(names_from = .data$mkey, values_from = .data$mvalue)
  }

  d <- d %>%
    dplyr::arrange(.data$Basisdato)

  d <- d %>%
    dplyr::rename("Basis\u00e5r" = .data$Basisaar,
                  "Indeks\u00e5r" = .data$Indeksaar,
                  "M\u00e5ned" = .data$Maaned)

  d
}
