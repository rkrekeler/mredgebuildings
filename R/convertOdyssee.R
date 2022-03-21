#' @importFrom dplyr filter mutate select
#' @importFrom madrat toolCountryFill
#' @importFrom magclass as.magpie
#' @importFrom quitte interpolate_missing_periods
convertOdyssee <- function(x) {
  unit2EJ <- c(Mtoe = 4.1868E-2, ktoe = 4.1868E-5, Tj = 1E-6)
  x %>%
    as.quitte() %>%
    filter(!is.na(.data[["value"]])) %>%
    mutate(value = .data[["value"]] * unit2EJ[.data[["unit"]]],
           unit = factor("EJ")) %>%
    select(-"note", -"title") %>%
    interpolate_missing_periods(expand.values = TRUE) %>%
    as.magpie() %>%
    toolCountryFill(verbosity = 2) %>%
    return()
}
