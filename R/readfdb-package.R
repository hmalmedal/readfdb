#' Read FDB Files
#'
#' Read files from \href{http://fdb.triona.no/}{Ferjedatabanken}.
#'
#' @name readfdb
#' @docType package
#' @importFrom dplyr %>%
#' @importFrom stats as.formula
#' @importFrom utils read.csv
NULL

ukedager <- c("Man", "Tir", "Ons", "Tor", "Fre", "L\u00f8r", "S\u00f8n")

maaneder <- c("Jan", "Feb", "Mar", "Apr", "Mai", "Jun",
              "Jul", "Aug", "Sep", "Okt", "Nov", "Des")
