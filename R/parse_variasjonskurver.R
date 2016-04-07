parse_variasjonskurver_dognvariasjon <- function(df, total) {
  checkmate::assertDataFrame(df, min.cols = 15)

  kl <- stringr::str_pad(0:24, 2, pad = "0")
  kl <- stringr::str_c(kl[-25], kl[-1], sep = "-")

  if (total) {
    df <- df %>%
      dplyr::filter_(~index == "Total")
  } else {
    df <- df %>%
      dplyr::filter_(~index != "Total")
  }

  df <- df %>%
    dplyr::rename_(Dag = ~key, Kl = ~index) %>%
    dplyr::mutate_(Aar = ~meta19 %>%
                     stringr::str_extract("\\d{4}$") %>%
                     as.integer(),
                   Uke = ~index_name %>%
                     stringr::str_extract("\\d{1,2}$") %>%
                     as.integer(),
                   Kl = ~factor(Kl, kl),
                   key = ~meta19 %>%
                     stringr::str_replace("\\s*\\d{4}$", "")) %>%
    dplyr::select_(~-index_name, ~-matches("^meta|^(sub)?type$")) %>%
    dplyr::select_(~Aar, ~Uke, ~Dag, ~Kl, ~everything())

  min_uke <- min(df$Uke[df$Uke > 1])
  if (min_uke >= 52) warning("Check week!")
  aar <- df$Aar[1]

  min_uke0 <- stringr::str_pad(min_uke, 2, pad = "0")
  mandag <- stringr::str_c(aar, "-W", min_uke0, "-1") %>%
    ISOweek::ISOweek2date() %>%
    lubridate::ymd(tz = "UTC")
  i <- min(which(df$Uke == min_uke))
  timer <- lubridate::hours(seq_along(df$Uke) - i)
  dager <- lubridate::days(seq_along(df$Uke) - i)

  if (total) {
    df <- df %>%
      dplyr::mutate_(Tid = ~mandag + dager)
  } else {
    df <- df %>%
      dplyr::mutate_(Tid = ~mandag + timer)
  }

  df <- df %>%
    dplyr::mutate_(Dato = ~as.Date(Tid),
                   Ukedato = ~ISOweek::date2ISOweek(Dato)) %>%
    dplyr::filter_(~lubridate::year(Dato) == aar) %>%
    tidyr::spread_("key", "value") %>%
    dplyr::arrange_(~Tid)

  df <- df %>%
    dplyr::rename_("\u00c5r" = ~Aar)

  df
}

parse_variasjonskurver_ukesvariasjon <- function(df) {
  checkmate::assertDataFrame(df, ncols = 15)

  df <- df %>%
    dplyr::filter_(~index != "Total") %>%
    dplyr::rename_(Dag = ~index) %>%
    dplyr::mutate_(Aar = ~index_name %>%
                     stringr::str_extract("^\\d{4}") %>%
                     as.integer(),
                   Uke = ~index_name %>%
                     stringr::str_extract("\\d{1,2}$") %>%
                     as.integer(),
                   Dag = ~factor(Dag, ukedager)) %>%
    dplyr::select_(~-index_name, ~-matches("^meta|^(sub)?type$")) %>%
    dplyr::select_(~Aar, ~Uke, ~Dag, ~everything())

  min_uke <- min(df$Uke[df$Uke > 1])
  if (min_uke >= 52) warning("Check week!")
  aar <- df$Aar[1]

  min_uke0 <- stringr::str_pad(min_uke, 2, pad = "0")
  mandag <- stringr::str_c(aar, "-W", min_uke0, "-1") %>%
    ISOweek::ISOweek2date()

  df <- df %>%
    dplyr::group_by_(~key) %>%
    dplyr::mutate_(Dato = ~mandag +
                     lubridate::days(seq_along(Uke) -
                                       min(which(Uke == min_uke))),
                   Ukedato = ~ISOweek::date2ISOweek(Dato)) %>%
    dplyr::ungroup() %>%
    tidyr::spread_("key", "value") %>%
    dplyr::arrange_(~Dato)

  df <- df %>%
    dplyr::rename_("\u00c5r" = ~Aar)

  df
}

parse_variasjonskurver_aarsvariasjon <- function(df, total) {
  checkmate::assertDataFrame(df, ncols = 15)

  if (total) {
    df <- df %>%
      dplyr::filter_(~index == "Total")
  } else {
    df <- df %>%
      dplyr::filter_(~index != "Total")
  }

  if (identical(unique(df$index_name), "\u00c5r")) {
    df <- df %>%
      dplyr::rename_(Aar = ~index) %>%
      dplyr::mutate_(Aar = ~as.integer(Aar),
                     Maaned = ~factor(NA, maaneder),
                     Dato = ~lubridate::make_datetime(Aar) %>% as.Date())
  } else {
    df <- df %>%
      dplyr::rename_(Maaned = ~index) %>%
      dplyr::mutate_(Aar = ~index_name %>%
                       as.integer(),
                     Maaned = ~factor(Maaned, maaneder),
                     Dato = ~lubridate::make_datetime(Aar, Maaned) %>%
                       as.Date())
  }

  df <- df %>%
    dplyr::select_(~-index_name, ~-matches("^meta|^(sub)?type$")) %>%
    dplyr::select_(~Aar, ~Maaned, ~everything()) %>%
    tidyr::spread_("key", "value") %>%
    dplyr::arrange_(~Dato)

  df <- df %>%
    dplyr::rename_("\u00c5r" = ~Aar,
                   "M\u00e5ned" = ~Maaned)

  df
}
