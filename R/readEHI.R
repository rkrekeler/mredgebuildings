#' Read EHI Heating Market Report
#'
#' @param subtype <report series>.<variable>
#' @returns magpie object
#'
#' @author Robin Hasse
#'
#' @importFrom utils read.csv capture.output
#' @importFrom dplyr %>% select mutate group_by filter rename
#' @importFrom rlang .data
#' @importFrom tidyr gather
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export

readEHI <- function(subtype) {

  # FUNCTIONS ------------------------------------------------------------------


  # PREPARE --------------------------------------------------------------------

  # split subtype
  if (!grepl("^[a-zA-Z0-9]{1,}\\.[a-zA-Z0-9]{1,}$", subtype)) {
    stop("The subtype '", subtype, "' is invalid. ",
         "It has to be of the structure '<report series>.<variable>''.")
  }
  report   <- gsub("\\..*$", "", subtype)
  variable <- gsub("^.*\\.", "", subtype)

  # pick file
  file <- switch(report,
    `2021`    = "heatingMarketReport2021.csv",
    stop("Invalid subtype. The report '", report, "' is not available."))



  # PROCESS DATA ---------------------------------------------------------------

  switch(subtype,
    `2021.stockHeaters` = {
      var <- "^Installed stock of heaters\\|"
      data <- read.csv(file) %>%
        filter(grepl(var, .data[["variable"]])) %>%
        mutate(variable = gsub(var, "", .data[["variable"]]))
    },
    stop("Invalid subtype. The variable '", variable,
         "' is not supported for the report '", report, "'.")
  )

  data <- data %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
