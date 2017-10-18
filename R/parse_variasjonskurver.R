parse_variasjonskurver_dognvariasjon <- function(df, total) {
  kl <- stringr::str_pad(0:24, 2, pad = "0")
  kl <- stringr::str_c(kl[-25], kl[-1], sep = "-")

  if (total) {
    df <- df %>%
      dplyr::filter(.data$index == "Total")
  } else {
    df <- df %>%
      dplyr::filter(.data$index != "Total")
  }

  meta_aar <- df$meta19 %>%
    stringr::str_extract("\\d{4}$") %>%
    as.integer()

  if (all(is.na(meta_aar))) {
    meta_aar <- df$meta20 %>%
      stringr::str_extract("\\d{4}$") %>%
      as.integer()
    meta_key <- df$meta20 %>%
      stringr::str_replace("\\s*\\d{4}$", "")
  } else {
    meta_key <- df$meta19 %>%
      stringr::str_replace("\\s*\\d{4}$", "")
  }

  if (any(is.na(meta_aar))) stop("Unknown error")

  df <- df %>%
    dplyr::rename(Dag = .data$key, Kl = .data$index) %>%
    dplyr::mutate(Aar = meta_aar,
                  Uke = .data$index_name %>%
                    stringr::str_extract("\\d{1,2}$") %>%
                    as.integer(),
                  Kl = factor(.data$Kl, kl),
                  key = meta_key) %>%
    dplyr::select(-.data$index_name, -dplyr::matches("^meta|^(sub)?type$")) %>%
    dplyr::select(.data$Aar, .data$Uke, .data$Dag, .data$Kl,
                  dplyr::everything())

  min_uke <- min(df$Uke[df$Uke > 1])
  if (min_uke >= 52) warning("Check week!")
  aar <- df$Aar[1]

  min_uke0 <- stringr::str_pad(min_uke, 2, pad = "0")

  mandag <- stringr::str_c(aar, "-W", min_uke0, "-1") %>%
    ISOweek::ISOweek2date() %>%
    as.POSIXct()
  attr(mandag, "tzone") <- "UTC"

  i <- min(which(df$Uke == min_uke))
  timer <- lubridate::hours(seq_along(df$Uke) - i)
  dager <- lubridate::days(seq_along(df$Uke) - i)

  if (total) {
    df <- df %>%
      dplyr::mutate(Tid = mandag + dager)
  } else {
    df <- df %>%
      dplyr::mutate(Tid = mandag + timer)
  }

  df <- df %>%
    dplyr::mutate(Dato = as.Date(.data$Tid),
                  Ukedato = ISOweek::date2ISOweek(.data$Dato)) %>%
    dplyr::filter(lubridate::year(.data$Dato) == aar) %>%
    tidyr::spread(.data$key, .data$value) %>%
    dplyr::arrange(.data$Tid)

  df <- df %>%
    dplyr::rename("\u00c5r" = .data$Aar)

  df
}

parse_variasjonskurver_ukesvariasjon <- function(df) {
  df <- df %>%
    dplyr::filter(.data$index != "Total") %>%
    dplyr::rename(Dag = .data$index) %>%
    dplyr::mutate(Aar = .data$index_name %>%
                    stringr::str_extract("^\\d{4}") %>%
                    as.integer(),
                  Uke = .data$index_name %>%
                    stringr::str_extract("\\d{1,2}$") %>%
                    as.integer(),
                  Dag = factor(.data$Dag, ukedager)) %>%
    dplyr::select(-.data$index_name, -dplyr::matches("^meta|^(sub)?type$")) %>%
    dplyr::select(.data$Aar, .data$Uke, .data$Dag, dplyr::everything())

  min_uke <- min(df$Uke[df$Uke > 1])
  if (min_uke >= 52) warning("Check week!")
  aar <- df$Aar[1]

  min_uke0 <- stringr::str_pad(min_uke, 2, pad = "0")
  mandag <- stringr::str_c(aar, "-W", min_uke0, "-1") %>%
    ISOweek::ISOweek2date()

  df <- df %>%
    dplyr::group_by(.data$key, .data$Fra, .data$Til) %>%
    dplyr::mutate(Dato = mandag +
                    lubridate::days(seq_along(.data$Uke) -
                                      min(which(.data$Uke == min_uke))),
                  Ukedato = ISOweek::date2ISOweek(.data$Dato)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(.data$key, .data$value) %>%
    dplyr::arrange(.data$Dato)

  df <- df %>%
    dplyr::rename("\u00c5r" = .data$Aar)

  df
}

parse_variasjonskurver_aarsvariasjon <- function(df, total) {
  if (total) {
    df <- df %>%
      dplyr::filter(.data$index == "Total")
  } else {
    df <- df %>%
      dplyr::filter(.data$index != "Total")
  }

  if (identical(unique(df$index_name), "\u00c5r")) {
    df <- df %>%
      dplyr::rename(Aar = .data$index) %>%
      dplyr::mutate(Aar = as.integer(.data$Aar),
                    Maaned = factor(NA, maaneder),
                    Dato = lubridate::make_date(.data$Aar))
  } else {
    df <- df %>%
      dplyr::rename(Maaned = .data$index) %>%
      dplyr::mutate(Aar = .data$index_name %>%
                      as.integer(),
                    Maaned = factor(.data$Maaned, maaneder),
                    Dato = lubridate::make_date(.data$Aar, .data$Maaned))
  }

  df <- df %>%
    dplyr::select(-.data$index_name, -dplyr::matches("^meta|^(sub)?type$")) %>%
    dplyr::select(.data$Aar, .data$Maaned, dplyr::everything()) %>%
    tidyr::spread(.data$key, .data$value) %>%
    dplyr::arrange(.data$Dato)

  df <- df %>%
    dplyr::rename("\u00c5r" = .data$Aar,
                  "M\u00e5ned" = .data$Maaned)

  df
}
