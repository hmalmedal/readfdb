extract_meta_dots <- function(meta) {
  meta_dots <- list(~meta[1], ~meta[2], ~meta[4], ~meta[6], ~meta[8],
                    ~meta[10], ~meta[12], ~meta[14], ~meta[16], ~meta[18])
  i <- seq(3, 17, by = 2)
  names(meta_dots) <- c("type",
                        "subtype",
                        meta[i]) %>%
    stringr::str_replace(":$", "")
  meta_dots
}
