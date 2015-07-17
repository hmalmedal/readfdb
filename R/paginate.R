paginate <- function(csv_lines) {
  end <- which(stringr::str_detect(csv_lines, "Side \\d")) - 1
  start <- lag(end, default = -1) + 2
  lapply(seq_along(start), function(i) csv_lines[start[i]:end[i]])
}
