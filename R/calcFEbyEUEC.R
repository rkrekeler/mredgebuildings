#' Historic Final Energy Demand by Carrier and Enduse
#'
#' Takes the historic final energy demand by carriers from IEA and disaggregates
#' it into different end uses.
#'
#' @returns data.frame with historic energy demands
#'
#' @author Hagen Tockhorn, Robin Hasse
#'
#' @importFrom dplyr mutate semi_join right_join left_join as_tibble filter
#'   select %>% .data pull
#' @importFrom madrat toolCountryFill calcOutput toolGetMapping
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export

calcFEbyEUEC <- function() {

  # READ-IN DATA ---------------------------------------------------------------

  # FE Data
  ieaIO <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings",
                      aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE)

  sharesEU <- calcOutput("Shares",
                         subtype = "enduse_nonthermal",
                         aggregate = TRUE,
                         regionmapping = "regionmappingEUshares.csv") %>%
    as.quitte()


  # FE EU Data
  feOdyssee <- calcOutput("ShareOdyssee",
                          subtype = "enduse_carrier",
                          feOnly = TRUE,
                          aggregate = FALSE) %>%
  as.quitte()


  # EU Shares mapping
  regmapping <- toolGetMapping(name  = "regionmappingEUshares.csv",
                               type  = "regional",
                               where = "mredgebuildings") %>%
    select("region", "regionAgg")



  # PARAMETERS -----------------------------------------------------------------

  # Enduse-Carrier combinations which will be systematically excluded
  exclude <- toolGetMapping(name  = "excludeEnduseCarrier.csv",
                            type  = "sectoral",
                            where = "mredgebuildings")



  # PROCESS DATA ---------------------------------------------------------------

  sharesEU <- sharesEU %>%
    select("region", "period", "enduse", "value")

  ieaIO <- ieaIO %>%
    rename(carrier = "variable")


  #--- Prepare toolDisaggregate Input

  # Extract regions with existing disaggregated FE shares
  replaceRegs <- feOdyssee %>%
    filter(!is.na(.data[["value"]])) %>%
    pull("region") %>%
    droplevels() %>%
    unique()

  feOdyssee <- feOdyssee %>%
    select("region", "period", "carrier", "enduse", "value") %>%
    mutate(unit = "fe") %>%
    semi_join(ieaIO, by = c("region", "period", "carrier"))


  # Disaggregate FE with EU/EC Shares
  ieaIODis <- ieaIO %>%
    select("region", "period", "carrier", "value") %>%
    mutate(unit = "fe") %>%
    toolDisaggregate(enduseShares  = sharesEU,
                     exclude       = exclude,
                     dataDisagg    = feOdyssee,
                     regionMapping = regmapping) %>%
    select("region", "period", "unit", "carrier", "enduse", "value")


  data <- rbind(ieaIODis %>%
                  filter(!(.data[["region"]] %in% replaceRegs)),
                feOdyssee %>%
                  filter(.data[["region"]] %in% replaceRegs))



  # RETURN DATA ----------------------------------------------------------------

  # Pack Data
  ieaIO <- ieaIO %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(1, verbosity = 2)

  data <- list(
    x = ieaIO,
    weight = NULL,
    unit = "EJ",
    description = "Historic Final Energy Data from IEA"
  )

  return(data)

}
