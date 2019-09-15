parse_variasjonskurver_dognvariasjon <- function(d, total) {
  kl <- stringr::str_pad(0:24, 2, pad = "0")
  kl <- stringr::str_c(kl[-25], kl[-1], sep = "-")

  if (total) {
    d <- d %>%
      dplyr::filter(.data$index == "Total")
  } else {
    d <- d %>%
      dplyr::filter(.data$index != "Total")
  }

  meta_aar <- d$meta19 %>%
    stringr::str_extract("\\d{4}$") %>%
    as.integer()

  if (all(is.na(meta_aar))) {
    meta_aar <- d$meta20 %>%
      stringr::str_extract("\\d{4}$") %>%
      as.integer()
    meta_key <- d$meta20 %>%
      stringr::str_remove("\\s*\\d{4}$")
  } else {
    meta_key <- d$meta19 %>%
      stringr::str_remove("\\s*\\d{4}$")
  }

  if (any(is.na(meta_aar))) stop("Unknown error")

  d <- d %>%
    dplyr::rename(Dag = .data$key, Kl = .data$index) %>%
    dplyr::mutate(Aar = meta_aar,
                  Uke = .data$index_name %>%
                    stringr::str_extract("\\d{1,2}$") %>%
                    as.integer(),
                  Kl = factor(.data$Kl, kl),
                  key = meta_key) %>%
    dplyr::select(-.data$index_name, -matches("^meta|^(sub)?type$")) %>%
    dplyr::select(.data$Aar, .data$Uke, .data$Dag, .data$Kl, everything())

  min_uke <- min(d$Uke[d$Uke > 1])
  if (min_uke >= 52) warning("Check week!")
  aar <- d$Aar[1]

  min_uke0 <- stringr::str_pad(min_uke, 2, pad = "0")

  mandag <- stringr::str_c(aar, "-W", min_uke0, "-1") %>%
    ISOweek::ISOweek2date() %>%
    as.POSIXct()
  attr(mandag, "tzone") <- "UTC"

  i <- min(which(d$Uke == min_uke))

  if (total) {
    d <- d %>%
      dplyr::group_by(.data$Fra, .data$Til) %>%
      dplyr::mutate(Tid = mandag +
                      lubridate::days(seq_along(.data$Uke) - i))
  } else {
    d <- d %>%
      dplyr::group_by(.data$Fra, .data$Til) %>%
      dplyr::mutate(Tid = mandag +
                      lubridate::hours(seq_along(.data$Uke) - i))
  }

  d <- d %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Dato = as.Date(.data$Tid),
                  Ukedato = ISOweek::date2ISOweek(.data$Dato)) %>%
    dplyr::filter(lubridate::year(.data$Dato) == aar) %>%
    tidyr::pivot_wider(names_from = .data$key, values_from = .data$value) %>%
    dplyr::arrange(.data$Tid)

  d <- d %>%
    dplyr::rename("\u00c5r" = .data$Aar)

  d
}

parse_variasjonskurver_ukesvariasjon <- function(d) {
  d <- d %>%
    dplyr::filter(.data$index != "Total") %>%
    dplyr::rename(Dag = .data$index) %>%
    dplyr::mutate(Aar = .data$index_name %>%
                    stringr::str_extract("^\\d{4}") %>%
                    as.integer(),
                  Uke = .data$index_name %>%
                    stringr::str_extract("\\d{1,2}$") %>%
                    as.integer(),
                  Dag = factor(.data$Dag, ukedager)) %>%
    dplyr::select(-.data$index_name, -matches("^meta|^(sub)?type$")) %>%
    dplyr::select(.data$Aar, .data$Uke, .data$Dag, everything())

  min_uke <- min(d$Uke[d$Uke > 1])
  if (min_uke >= 52) warning("Check week!")
  aar <- d$Aar[1]

  min_uke0 <- stringr::str_pad(min_uke, 2, pad = "0")
  mandag <- stringr::str_c(aar, "-W", min_uke0, "-1") %>%
    ISOweek::ISOweek2date()

  d <- d %>%
    dplyr::group_by(.data$key, .data$Fra, .data$Til) %>%
    dplyr::mutate(Dato = mandag +
                    lubridate::days(seq_along(.data$Uke) -
                                      min(which(.data$Uke == min_uke))),
                  Ukedato = ISOweek::date2ISOweek(.data$Dato)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = .data$key, values_from = .data$value) %>%
    dplyr::arrange(.data$Dato)

  d <- d %>%
    dplyr::rename("\u00c5r" = .data$Aar)

  d
}

parse_variasjonskurver_aarsvariasjon <- function(d, total) {
  if (total) {
    d <- d %>%
      dplyr::filter(.data$index == "Total") %>%
      dplyr::mutate(index = NA)
  } else {
    d <- d %>%
      dplyr::filter(.data$index != "Total")
  }

  if (identical(unique(d$index_name), "\u00c5r")) {
    d <- d %>%
      dplyr::rename(Aar = .data$index) %>%
      dplyr::mutate(Aar = as.integer(.data$Aar),
                    Maaned = factor(NA, maaneder),
                    Dato = lubridate::make_date(.data$Aar))
  } else {
    d <- d %>%
      dplyr::rename(Maaned = .data$index) %>%
      dplyr::mutate(Aar = .data$index_name %>%
                      as.integer(),
                    Maaned = factor(.data$Maaned, maaneder),
                    Dato = lubridate::make_date(.data$Aar, .data$Maaned))
  }

  d <- d %>%
    dplyr::select(-.data$index_name, -matches("^meta|^(sub)?type$")) %>%
    dplyr::select(.data$Aar, .data$Maaned, everything()) %>%
    tidyr::pivot_wider(names_from = .data$key, values_from = .data$value) %>%
    dplyr::arrange(.data$Dato)

  d <- d %>%
    dplyr::rename("\u00c5r" = .data$Aar,
                  "M\u00e5ned" = .data$Maaned)

  d
}
