parse_csv_page <- function(csv_page) {
  if (identical(csv_page, "")) return(NULL)

  meta_length <- 7

  meta <- parse_meta(csv_page, meta_length)

  df <- read.csv(text = csv_page, na.strings = "?", colClasses = "character",
                 skip = meta_length, check.names = FALSE, encoding = "UTF-8")
  df <- dplyr::as_data_frame(df[names(df) != ""])

  index_name <- names(df)[1]
  names(df)[1] <- "index"

  df <- df %>%
    tidyr::gather("key", "value", -1, factor_key = TRUE) %>%
    dplyr::mutate(value = stringr::str_replace_all(.data$value, ",", "") %>%
                    as.numeric()) %>%
    dplyr::mutate(index_name, !!!meta) %>%
    dplyr::select(.data$index, .data$index_name, dplyr::everything())

  df
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
  names(meta)[i + 1] <- stringr::str_replace(meta[i], ":$", "")
  meta <- meta[-i]

  meta
}

parse_trafikkverdier <- function(df) {
  df <- df %>%
    dplyr::rename(Trafikktype = .data$key) %>%
    dplyr::mutate(Aar = as.integer(.data$index_name),
                  index = forcats::as_factor(.data$index),
                  Maanedsintervall = .data$meta18 %>%
                    stringr::str_replace_all(" \\d{4}", "")) %>%
    dplyr::select(-.data$index_name, -dplyr::matches("^meta|^(sub)?type$")) %>%
    dplyr::select(.data$Aar, dplyr::everything()) %>%
    tidyr::spread(.data$index, .data$value)

  df <- df %>%
    dplyr::rename("\u00c5r" = .data$Aar,
                  "M\u00e5nedsintervall" = .data$Maanedsintervall)

  df
}

parse_produksjon <- function(df, total) {
  if (total) {
    df <- df %>%
      dplyr::filter(.data$index == "Total")
  } else {
    df <- df %>%
      dplyr::filter(.data$index != "Total")
  }

  df <- df %>%
    dplyr::rename(Maaned = .data$index) %>%
    dplyr::mutate(Aar = as.integer(.data$index_name),
                  Maaned = .data$Maaned %>%
                    stringr::str_extract("^\\w{3}") %>%
                    factor(maaneder),
                  Dato = lubridate::make_date(.data$Aar, .data$Maaned)) %>%
    dplyr::select(-.data$index_name, -dplyr::matches("^meta|^(sub)?type$")) %>%
    dplyr::select(.data$Aar, .data$Maaned, dplyr::everything()) %>%
    tidyr::spread(.data$key, .data$value)

  df <- df %>%
    dplyr::rename("\u00c5r" = .data$Aar,
                  "M\u00e5ned" = .data$Maaned)

  df
}

parse_sonefordeling <- function(df, total) {
  if (total) {
    df <- df %>%
      dplyr::filter(.data$index == "Total") %>%
      dplyr::mutate(index = NA)
  } else {
    df <- df %>%
      dplyr::filter(.data$index != "Total")
  }

  df <- df %>%
    dplyr::rename(Sone = .data$index) %>%
    dplyr::mutate(Sone = as.integer(.data$Sone),
                  Aar = .data$meta17 %>%
                    stringr::str_extract("\\d{4}") %>%
                    as.integer(),
                  Maanedsintervall = .data$meta17 %>%
                    stringr::str_replace_all(" \\d{4}", "")) %>%
    dplyr::select(-.data$index_name, -dplyr::matches("^meta|^(sub)?type$")) %>%
    dplyr::select(.data$Aar, dplyr::everything()) %>%
    tidyr::spread(.data$key, .data$value)

  df <- df %>%
    dplyr::rename("\u00c5r" = .data$Aar,
                  "M\u00e5nedsintervall" = .data$Maanedsintervall)

  df
}
