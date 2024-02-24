#' Converts Data from Primary Fuel and Useful Energy Data Base (PFUDB)
#'
#' PFU Data is disaggregated into different thermal and non-thermal Enduses
#' using FE Shares from Odyssee and IEA_ETP.
#'
#' This was adapted from EDGE function 'getPFUDB.R'.
#'
#' @returns magpie object
#'
#' @author Hagen Tockhorn, Robin Hasse
#'
#' @importFrom madrat readSource toolGetMapping readSource
#' @importFrom quitte aggregate_map as.quitte factor.data.frame revalue.levels
#' @importFrom dplyr %>% .data filter group_by summarise mutate ungroup select
#'   all_of any_of pull
#' @export

calcPFUDB <- function() {
  # FUNCTIONS ------------------------------------------------------------------

  # Sum to Carrier Level
  sumDf <- function(df, variables, newname) {
    carrierSum <- df %>%
      filter(.data[["carrier"]] %in% variables) %>%
      group_by(across(-any_of(c("value", "carrier")))) %>%
      summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop") %>%
      mutate(carrier = newname) %>%
      ungroup() %>%
      select(all_of(colnames(df)))
    df %>%
      filter(!(.data[["carrier"]] %in% variables)) %>%
      rbind(carrierSum)
  }



  # PARAMETERS -----------------------------------------------------------------

  # Rename Carrier Items
  carriersnames <- toolGetMapping("carrierMap_PFUDB.csv", "sectoral",
                                  "mredgebuildings") %>%
    pull("EDGE", "PFUDB")

  # Enduse-Carrier combinations which will be systematically excluded
  exclude <- toolGetMapping("excludeEnduseCarrier.csv", "sectoral",
                            "mredgebuildings")

  # fridge share of Europe (see calcShares)
  fridgeShare <- 0.17



  # READ-IN DATA ---------------------------------------------------------------

  pfu <- readSource("PFUDB") %>%
    as.quitte()


  # Relevant toolDisaggregate Input-Data

  sharesEU <- calcOutput("Shares",
                         subtype = "enduse_thermal",
                         aggregate = FALSE)  %>%
    as.quitte()

  etpEU <- calcOutput("Shares",
                      subtype = "enduse_thermal",
                      feOnly = TRUE,
                      aggregate = FALSE) %>%
              as.quitte()

  feOdyssee <- calcOutput("ShareOdyssee",
                          subtype = "enduse_carrier",
                          feOnly = TRUE,
                          aggregate = FALSE) %>%
    as.quitte()


  # ETP mapping
  regmapping <- toolGetMapping("regionmappingIEA_ETP.csv",
                               "regional",
                               "mappingfolder")



  # PROCESS DATA ---------------------------------------------------------------

  # Generalize Heat Carriers
  pfu <- sumDf(pfu, c("Heat", "Geothermal", "Solar"), "heat")


  # Map Carrier Names and Convert Units
  pfu <- pfu %>%
    revalue.levels(carrier = carriersnames) %>%
    factor.data.frame() %>%
    mutate(value = replace_na(.data[["value"]], 0))


  pfu <- select(pfu, -"model", -"scenario", -"variable")


  ## Disaggregate into Thermal and Non-Thermal Part ====

  # Enduse Mappings
  enduseMapping <- toolGetMapping("enduseMap_PFUDB.csv", "sectoral",
                                  "mredgebuildings")

  # Aggregate uses from PFU to appliances_light and keep the df as Non-thermal
  pfuNonTherm <- pfu %>%
    filter(.data[["enduse"]] != "Low-T heat") %>%
    aggregate_map(mapping = enduseMapping[1:3],
                  by = "enduse",
                  variable = "enduse",
                  forceAggregation = TRUE)


  ## Prepare toolDisaggregate Input ====

  # note: hard-coded the fridge share so that the FE data becomes compliant with
  # the remaining input (can be done more nicely)
  feOdyssee <- feOdyssee %>%
    select("region", "period", "carrier", "enduse", "value") %>%
    filter(.data[["enduse"]] != "lighting") %>%
    mutate(value = .data[["value"]] * ifelse(.data[["enduse"]] != "appliances",
                                             1, fridgeShare),
           enduse = ifelse(.data[["enduse"]] == "appliances",
                           "refrigerators",
                           as.character(.data[["enduse"]])))

  regmapping <- regmapping %>%
    mutate(regionAgg = ifelse(.data[["EEAReg"]] == "rest",
                              .data[["OECD"]],
                              .data[["EEAReg"]])) %>%
    select(region = "CountryCode", "regionAgg")

  etpEU <- select(etpEU, "region", "period", "enduse", "value") %>%
    left_join(regmapping, by = "region")


  # Disaggregate Low-T Heat into different enduses
  pfuTherm <- pfu %>%
    filter(.data[["enduse"]] == "Low-T heat") %>%
    select(-"enduse") %>%
    rename(variable = "carrier") %>%
    toolDisaggregate(sharesEU,
                     etpEU,
                     exclude = exclude,
                     dataReplace = feOdyssee) %>%
    select(colnames(pfuNonTherm))


  # Join Non-Thermal and Thermal Part
  pfuRes <- rbind(pfuTherm, pfuNonTherm)


  # Include "refrigerators" in "appliances_light"
  pfuRes <- rbind(
    pfuRes %>%
      filter(!(.data[["enduse"]] %in% enduseMapping[4:5, "enduse"])),
    pfuRes %>%
      aggregate_map(mapping = enduseMapping[4:5, ],
                    by = "enduse",
                    variable = "enduse",
                    forceAggregation = TRUE))



  # OUTPUT ---------------------------------------------------------------------

  pfuRes <- pfuRes %>%
    as.data.frame() %>%
    as.magpie()


  data <- list(
    x = pfuRes,
    weight = NULL,
    unit = "EJ",
    min = 0,
    description = "Primary Fuel and Useful Energy Data Base"
  )

  return(data)

}
