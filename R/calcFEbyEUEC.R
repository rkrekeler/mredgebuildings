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


  # FE EU Data
  feOdyssee <- calcOutput("ShareOdyssee",
                          subtype = "enduse_carrier",
                          feOnly = TRUE,
                          aggregate = FALSE) %>%
  as.quitte()


  # FE IEA EEI data
  feIEAEEI <- calcOutput("IEA_EEI",
                         subtype = "buildings",
                         aggregate = FALSE) %>%
    as.quitte()


  # EU Shares
  sharesEU <- calcOutput("Shares",
                         subtype = "enduse_nonthermal",
                         aggregate = TRUE,
                         regionmapping = "regionmappingEUshares.csv") %>%
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

  # European countries
  eurCountries <- c("FIN", "AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP",
                    "EST", "FRA", "GBR", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU",
                    "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN",
                    "SWE")


  # PROCESS DATA ---------------------------------------------------------------

  sharesEU <- sharesEU %>%
    select("region", "period", "enduse", "value")

  ieaIO <- ieaIO %>%
    rename(carrier = "variable") %>%
    semi_join(sharesEU, by = c("period"))


  #--- Prepare toolDisaggregate Input

  # Prepare already disaggregated data
  feDisagg <- feOdyssee %>%
    left_join(feIEAEEI, by = c("region", "period", "carrier", "enduse")) %>%
    mutate(value = ifelse(is.na(.data[["value.x"]]),
                          .data[["value.y"]],
                          .data[["value.x"]])) %>%
    # mutate(value = max(.data[["value.x"]], .data[["value.y"]])) %>%
    select("region", "period", "carrier", "enduse", "value") %>%
    mutate(unit = "fe") %>%
    semi_join(ieaIO, by = c("region", "period", "carrier"))


  # Lower boundaries for disaggregation
  lowerBounds <- feOdyssee %>%
    select("region", "period", "carrier", "enduse", "value") %>%
    filter(!is.na(.data[["value"]])) %>%
    group_by(across(all_of(c("region", "period", "carrier")))) %>%
    mutate(value = proportions(.data[["value"]]),
           value = replace_na(.data[["value"]], 0)) %>%
    ungroup()


  # Disaggregate FE with EU/EC Shares
  ieaIODis <- ieaIO %>%
    select("region", "period", "carrier", "value") %>%
    mutate(unit = "fe") %>%
    toolDisaggregate(enduseShares  = sharesEU,
                     exclude       = exclude,
                     dataDisagg    = feDisagg,
                     regionMapping = regmapping,
                     lowerBounds   = lowerBounds) %>%
    select("region", "period", "unit", "carrier", "enduse", "value")

  # Identify full data sets region-carrier-enduse combinations
  dataReplace <- feOdyssee %>%
    group_by(across(all_of(c("region", "carrier", "enduse")))) %>%
    filter(all(!is.na(.data[["value"]]))) %>%
    reframe(replaceValue = .data[["value"]],
            period = .data[["period"]])

  data <- ieaIODis %>%
    left_join(dataReplace, by = c("region", "carrier", "enduse", "period")) %>%
    mutate(value = ifelse(!is.na(.data[["replaceValue"]]),
                          .data[["replaceValue"]],
                          .data[["value"]])) %>%
    select(-"replaceValue")

  # data <- rbind(ieaIODis %>%
  #                 filter(!(.data[["region"]] %in% eurCountries)),
  #               feDisagg %>%
  #                 filter(.data[["region"]] %in% eurCountries))


  # CORRECTIONS ----------------------------------------------------------------

  # For unknown reasons, the enduse share of "space_cooling" for region "Africa"
  # is not met and will therefore be corrected. Since "space_cooling" only corresponds
  # to the carrier "elec", the correction is straight-forward.
  # TODO: check if this can be fixed

  elecSpaceCoolingShare <- sharesEU %>%
    filter(region == "Africa",
           enduse == "space_cooling") %>%
    select("period", "value") %>%
    rename("share" = "value")

  dataCorr <- data %>%
    left_join(regmapping, by = c("region")) %>%
    filter(.data[["regionAgg"]] == "Africa") %>%
    left_join(elecSpaceCoolingShare, by = c("period")) %>%
    select(-"regionAgg") %>%
    group_by(across(-all_of(c("enduse", "carrier", "share", "value")))) %>%
    mutate(value = ifelse(.data[["enduse"]] == "space_cooling",
                          ifelse(!(.data[["carrier"]] == "elec"),
                                 .data[["value"]],
                                 sum(.data[["value"]], na.rm = T) * .data[["share"]]),
                          .data[["value"]] * (1 - .data[["share"]]))) %>%
    ungroup() %>%
    select(-"share")

  dataFull <- rbind(dataCorr,
                    data %>%
                      left_join(regmapping, by = c("region")) %>%
                      filter(.data[["regionAgg"]] != "Africa") %>%
                      select(-"regionAgg"))



  # RETURN DATA ----------------------------------------------------------------

  # Pack Data
  dataFull <- dataFull %>%
    mutate(scenario = "history") %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(1, verbosity = 2)

  data <- list(
    x = dataFull,
    weight = NULL,
    unit = "EJ",
    description = "Historic Final Energy Data from IEA"
  )

  return(data)

}
