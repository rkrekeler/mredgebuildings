#' Floor space by subsectors
#'
#' Residential and commercial floor space for big world regions from the IEA
#' TCEP report 2014 in billion m2.
#'
#' @return MAgPIE object with floor space
#'
#' @author Robin Krekeler
#'
#' @source https://iea.blob.core.windows.net/assets/416e9555-67f9-49ed-95ed-8afccd71b433/
#' Tracking_clean_energy_progress_2014.pdf
#'
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{ a <- readSource(type = "IEAfloorspace")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom tidyr gather
#' @importFrom dplyr %>%
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie collapseDim
#' @export

readIEAfloorspace <- function() {

  data <- read.csv("TCEP2014_figure_01_41_modified.csv") %>%
    gather("subsector", "value", "Residential", "Services") %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
