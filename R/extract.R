extract_meta_dots <- function(meta) {
  checkmate::assertCharacter(meta, min.len = 17, max.len = 20)

  meta_dots <- stringr::str_c("~meta[", seq_along(meta), "]") %>%
    lapply(as.formula, env = parent.frame(n = 2))

  names(meta_dots) <- stringr::str_c("meta", seq_along(meta))
  names(meta_dots)[1:2] <- c("type", "subtype")

  i <- which(stringr::str_detect(meta, ":$"))
  names(meta_dots)[i + 1] <- stringr::str_replace(meta[i], ":$", "")
  meta_dots <- meta_dots[-i]

  meta_dots
}
