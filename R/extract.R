extract_meta_dots <- function(meta) {
  checkmate::assertCharacter(meta, min.len = 17, max.len = 19)

  colon_pos <- which(stringr::str_detect(meta, ":$"))
  checkmate::assertInteger(colon_pos, lower = 3, upper = length(meta) - 1,
                           min.len = 7, max.len = 8)

  i <- c(1:2, colon_pos + 1, max(colon_pos) + 2)
  meta_dots <- stringr::str_c("~meta[", i, "]") %>%
    lapply(as.formula, env = parent.frame(n = 2))
  names(meta_dots) <- c("type", "subtype", meta[colon_pos], "meta") %>%
    stringr::str_replace(":$", "")

  meta_dots
}
