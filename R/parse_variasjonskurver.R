parse_variasjonskurver_dognvariasjon <- function(df) {
  checkmate::assertDataFrame(df, ncols = 15)

  df <- df %>%
    dplyr::filter_(~index != "Total") %>%
    dplyr::rename_(Dag = ~key, Kl = ~index) %>%
    dplyr::mutate_(Aar = ~meta19 %>%
                     stringr::str_extract("\\d{4}$") %>%
                     as.integer(),
                   Uke = ~index_name %>%
                     stringr::str_extract("\\d{1,2}$") %>%
                     as.integer(),
                   Kl = ~factor(Kl),
                   key = ~meta19 %>%
                     stringr::str_replace("\\s*\\d{4}$", "")) %>%
    dplyr::select_(~-index_name, ~-matches("^meta|^(sub)?type$")) %>%
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
    dplyr::filter_(~lubridate::year(Dato) == aar) %>%
    tidyr::spread_("key", "value") %>%
    dplyr::arrange_(~Tid)

  names(df)[1] <- "\u00c5r" # År
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

  mandag <- stringr::str_c(aar, "-W",
                           stringr::str_pad(min_uke, 2, pad = "0"),
                           "-1") %>%
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

  names(df)[1] <- "\u00c5r" # År
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

  df <- df %>%
    dplyr::rename_(Maaned = ~index) %>%
    dplyr::mutate_(Aar = ~index_name %>%
                     as.integer(),
                   Maaned = ~factor(Maaned, maaneder),
                   Dato = ~stringr::str_c(Aar, "-", as.integer(Maaned),
                                          "-01") %>% as.Date()) %>%
    dplyr::select_(~-index_name, ~-matches("^meta|^(sub)?type$")) %>%
    dplyr::select_(~Aar, ~Maaned, ~everything()) %>%
    tidyr::spread_("key", "value") %>%
    dplyr::arrange_(~Dato)

  names(df)[1] <- "\u00c5r" # År
  names(df)[2] <- "M\u00e5ned" # Måned
  df
}
