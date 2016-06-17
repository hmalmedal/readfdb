extract_meta_dots <- function(meta) {
  meta_dots <- stringr::str_c("~meta[", seq_along(meta), "]") %>%
    purrr::map(as.formula, env = parent.frame(n = 2))

  names(meta_dots) <- stringr::str_c("meta", seq_along(meta))
  names(meta_dots)[1:2] <- c("type", "subtype")

  i <- which(stringr::str_detect(meta, ":$"))
  names(meta_dots)[i + 1] <- stringr::str_replace(meta[i], ":$", "")
  meta_dots <- meta_dots[-i]

  meta_dots
}
