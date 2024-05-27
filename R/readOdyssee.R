#' Read Odyssee data base
#'
#' Copied from website: The Odyssee database contains detailed energy
#' consumption by end-use and their drivers as well as energy efficiency and
#' CO2-related indicators. Latest available data is provided by national
#' representatives, such as energy agencies or statistical organization, from
#' all EU countries as well as Norway, Serbia, Switzerland and the United
#' Kingdom.
#'
#' @source https://odyssee.enerdata.net/database/
#' @note To download new data, log into the website, select all items of all
#' levels and download the data 'column-orientated csv'.
#' @note  Variables are labels with the item code but full names can be found in
#' the source data
#'
#' @param subtype database category
#' @returns magpie object
#'
#' @author Pascal Führlich, Robin Hasse
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>% select mutate .data
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export

readOdyssee <- function(subtype = "households") {

  # check subtype
  file <- switch(subtype,
                 households = "Enerdata_Odyssee_241002_090920.csv",
                 services   = "Enerdata_Odyssee_241002_091116.csv",
                 stop("'", subtype, "' is not a valid subtype."))

  # read data
  data <- read.csv(file, na.strings = c("n.a.", "")) %>%
    select(region = "ISO.Code",
           period = "Year",
           variable = "Item.Code",
           value = "Value",
           unit = "Unit") %>%
    mutate(value = as.numeric(.data[["value"]])) %>%
    filter(!is.na(.data[["value"]])) %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
