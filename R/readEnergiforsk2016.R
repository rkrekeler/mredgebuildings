#' Read European district heating prices
#'
#' @author Robin Hasse
#'
#' @importFrom utils read.csv
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr select matches mutate rename
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export

readEnergiforsk2016 <- function() {
  file <- "european-district-heating-price-series-energiforskrapport-2016-316.csv"
  read.csv(file, skip = 1) %>%
    select(-matches("All\\.countries")) %>%
    pivot_longer(-"Year", names_to = "region", values_to = "value") %>%
    mutate(region = gsub("\\.", " ", .data[["region"]])) %>%
    rename(period = "Year") %>%
    as.quitte() %>%
    as.magpie()
}
