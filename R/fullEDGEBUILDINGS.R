#' fullEDGEBUILDINGS
#'
#' Function that produces the complete ISO data set required for the
#' EDGE - Buildings model.
#'
#' @param rev data revision which should be used as input (positive numeric).
#'
#' @author Antoine Levesque, Robin Hasse, Hagen Tockhorn
#'
#' @seealso
#' \code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}}
#' @examples
#' \dontrun{
#' fullEDGEBUILDINGS()
#' }
#'
#' @importFrom madrat calcOutput
#' @export

fullEDGEBUILDINGS <- function(rev = 0) {

  # socio-economic data --------------------------------------------------------
  calcOutput("Population",     aggregate = FALSE, file = "f_pop.cs4r")
  calcOutput("GDP",            aggregate = FALSE, file = "f_gdp.cs4r", average2020 = FALSE)
  calcOutput("Surface",        aggregate = FALSE, file = "f_surface.cs4r")
  calcOutput("Urban",          aggregate = FALSE, file = "f_urban.cs4r")

  # energy ---------------------------------------------------------------------
  # move calcIO to mrcommons
  calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings", aggregate = FALSE, file = "f_edge_buildings.cs4r")
  calcOutput("IOEdgeBuildings", subtype = "output_EDGE",           aggregate = FALSE, file = "f_edge_stationary.cs4r")
  calcOutput("FloorspacePast",                                     aggregate = FALSE, file = "f_floorspace.cs4r")
  calcOutput("FEUE",                                               aggregate = FALSE, file = "f_feue.cs4r")
  calcOutput("FEUEefficiencies",                                   aggregate = FALSE, file = "f_feue_efficiencies.cs4r")
}
