#' Historic Final Energy Demand by Carrier and Enduse
#'
#' Takes the historic final energy demand by carriers from IEA and disaggregates
#' it into different end uses.
#'
#' @returns data.frame with historic energy demands
#'
#' @author Hagen Tockhorn
#'
#' @importFrom rlang .data
#' @importFrom dplyr mutate inner_join right_join left_join as_tibble filter select %>%
#' @importFrom madrat toolCountryFill
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#'
#' @export


calcFEbyEUEC <- function() {

  # READ-IN DATA ---------------------------------------------------------------

  # FE Data
  ieaIO <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings", aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE)


  # EU Shares
  sharesEU <- calcOutput("Shares",
                         subtype = "enduse_nonthermal",
                         aggregate = FALSE) %>%
              as.quitte()


  # FE EU Data
  etpEU <- calcOutput("Shares",
                      subtype = "enduse_nonthermal",
                      feOnly = TRUE,
                      aggregate = FALSE) %>%
    as.quitte()


  # Odyssee Data for Share Replacement
  # sharesOdyssee <- calcOutput("ShareOdyssee",
  #                             subtype = "enduse_carrier",
  #                             aggregate = FALSE) %>%
  #                  as.quitte()
  feOdyssee <- calcOutput("ShareOdyssee",
                          subtype = "enduse_carrier",
                          feOnly = TRUE,
                          aggregate = FALSE) %>%
    as.quitte()


  # ETP mapping
  regmapping <- toolGetMapping("regionmappingIEA_ETP.csv", where = "mappingfolder", type = "regional")


  # PARAMETERS -----------------------------------------------------------------

  # Enduse-Carrier combinations which will be systematically excluded
  exclude <- c("appliances-natgas",
               "appliances-petrol",
               "appliances-biomod",
               "appliances-biotrad",
               "appliances-coal",
               "appliances-heat",
               "lighting-biomod",
               "lighting-biotrad",
               "lighting-coal",
               "lighting-heat",
               "cooking-heat",
               "space_cooling-heat",
               "space_cooling-biomod",
               "space_cooling-biotrad",
               "space_cooling-coal",
               "space_cooling-natgas",
               "space_cooling-petrol")


  # PROCESS DATA ---------------------------------------------------------------

  # Reduce the data frames dimensions to the minimal set
  commonRegionsPeriods <- Reduce(inner_join,
                                 list(unique(ieaIO[, c("region", "period")]),
                                      unique(sharesEU[, c("region", "period")]))) %>%
    as_tibble()

  ieaIO <- commonRegionsPeriods %>%
    left_join(ieaIO, by = c("region", "period"))
  sharesEU <- commonRegionsPeriods %>%
    left_join(sharesEU, by = c("region", "period"))


  # Prepare toolDisaggregate Input

  feOdyssee <- feOdyssee %>%
    select("region", "period", "carrier", "enduse", "value")

  regmapping <- regmapping %>%
    mutate(EEAReg = ifelse(.data[["EEAReg"]] == "rest",
                           .data[["OECD"]],
                           .data[["EEAReg"]]))

  etpEU <- select(etpEU, "region", "period", "enduse", "value") %>%
    left_join(regmapping %>%
                select("CountryCode", "EEAReg") %>%
                rename(region = "CountryCode"),
              by = "region")



  # Disaggregate FE with EU/EC Shares
  ieaIO <- ieaIO %>%
    select(-"model", -"scenario") %>%
    mutate(unit = "fe") %>%
    toolDisaggregate(sharesEU,
                     etpEU,
                     exclude = exclude,
                     dataReplace = feOdyssee)



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
