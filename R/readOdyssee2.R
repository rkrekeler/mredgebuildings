#' Read Odyssee Database
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
#' @return magpie object
#'
#' @author Robin Krekeler
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>% select mutate
#' @importFrom rlang .data
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#'
#' @export

readOdyssee2 <- function(subtype = "households") {

  # check subtype
  availableSubtypes <- gsub("\\.csv$", "", list.files(pattern = "\\.csv$"))
  if (!subtype %in% availableSubtypes) {
    stop("'", subtype, "' is not a valid subtype. Available subtypes: ",
         paste(availableSubtypes, collapse = ", "))
  }

  # read data
  data <- paste0(subtype, ".csv") %>%
    read.csv(skip = 1, na.strings = "n.a.") %>%
    select(region = "ISO.code",
           period = "Year",
           variable = "Item.code",
           value = "Value") %>%
    mutate(value = as.numeric(.data[["value"]])) %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
