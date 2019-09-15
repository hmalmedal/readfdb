parse_csv_page <- function(csv_page) {
  if (identical(csv_page, "")) return(NULL)

  meta_length <- 7

  meta <- parse_meta(csv_page, meta_length)

  d <- read.csv(text = csv_page, na.strings = "?", colClasses = "character",
                skip = meta_length, check.names = FALSE, encoding = "UTF-8")
  d <- tibble::as_tibble(d[names(d) != ""])

  index_name <- names(d)[1]
  names(d)[1] <- "index"

  d <- d %>%
    tidyr::pivot_longer(-1, names_to = "key", values_to = "value") %>%
    dplyr::mutate(value = stringr::str_remove_all(.data$value, ",") %>%
                    as.numeric()) %>%
    dplyr::mutate(index_name, !!!meta)

  d
}

parse_meta <- function(csv_page, meta_length) {
  meta <- csv_page %>%
    readr::tokenize(n_max = meta_length) %>%
    purrr::flatten_chr()

  i <- meta != "[EMPTY]"
  meta <- meta[i]

  Encoding(meta) <- "UTF-8"

  names(meta) <- stringr::str_c("meta", seq_along(meta))
  names(meta)[1:2] <- c("type", "subtype")

  i <- which(stringr::str_detect(meta, ":$"))
  names(meta)[i + 1] <- stringr::str_remove(meta[i], ":$")
  meta <- meta[-i]

  meta
}

parse_trafikkverdier <- function(d) {
  d <- d %>%
    dplyr::rename(Trafikktype = .data$key) %>%
    dplyr::mutate(Aar = as.integer(.data$index_name),
                  index = forcats::as_factor(.data$index),
                  Maanedsintervall = .data$meta18 %>%
                    stringr::str_remove_all(" \\d{4}")) %>%
    dplyr::select(-.data$index_name, -matches("^meta|^(sub)?type$")) %>%
    dplyr::select(.data$Aar, everything()) %>%
    tidyr::pivot_wider(names_from = .data$index, values_from = .data$value)

  d <- d %>%
    dplyr::rename("\u00c5r" = .data$Aar,
                  "M\u00e5nedsintervall" = .data$Maanedsintervall)

  d
}

parse_produksjon <- function(d, total) {
  if (total) {
    d <- d %>%
      dplyr::filter(.data$index == "Total")
  } else {
    d <- d %>%
      dplyr::filter(.data$index != "Total")
  }

  d <- d %>%
    dplyr::rename(Maaned = .data$index) %>%
    dplyr::mutate(Aar = as.integer(.data$index_name),
                  Maaned = .data$Maaned %>%
                    stringr::str_extract("^\\w{3}") %>%
                    factor(maaneder),
                  Dato = lubridate::make_date(.data$Aar, .data$Maaned)) %>%
    dplyr::select(-.data$index_name, -matches("^meta|^(sub)?type$")) %>%
    dplyr::select(.data$Aar, .data$Maaned, everything()) %>%
    tidyr::pivot_wider(names_from = .data$key, values_from = .data$value)

  d <- d %>%
    dplyr::rename("\u00c5r" = .data$Aar,
                  "M\u00e5ned" = .data$Maaned)

  d
}

parse_sonefordeling <- function(d, total) {
  if (total) {
    d <- d %>%
      dplyr::filter(.data$index == "Total") %>%
      dplyr::mutate(index = NA)
  } else {
    d <- d %>%
      dplyr::filter(.data$index != "Total")
  }

  d <- d %>%
    dplyr::rename(Sone = .data$index) %>%
    dplyr::mutate(Sone = as.integer(.data$Sone),
                  Aar = .data$meta17 %>%
                    stringr::str_extract("\\d{4}") %>%
                    as.integer(),
                  Maanedsintervall = .data$meta17 %>%
                    stringr::str_remove_all(" \\d{4}")) %>%
    dplyr::select(-.data$index_name, -matches("^meta|^(sub)?type$")) %>%
    dplyr::select(.data$Aar, everything()) %>%
    tidyr::pivot_wider(names_from = .data$key, values_from = .data$value)

  d <- d %>%
    dplyr::rename("\u00c5r" = .data$Aar,
                  "M\u00e5nedsintervall" = .data$Maanedsintervall)

  d
}
