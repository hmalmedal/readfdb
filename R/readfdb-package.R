#' Read FDB Files
#'
#' Read files from \href{http://fdb.triona.no/}{Ferjedatabanken}.
#'
#' @keywords package
#'
#' @importFrom magrittr %>%
#' @importFrom utils read.csv
#' @importFrom rlang .data
#' @importFrom tidyselect matches everything
"_PACKAGE"

ukedager <- c("Man", "Tir", "Ons", "Tor", "Fre", "L\u00f8r", "S\u00f8n")

maaneder <- c("Jan", "Feb", "Mar", "Apr", "Mai", "Jun",
              "Jul", "Aug", "Sep", "Okt", "Nov", "Des")
