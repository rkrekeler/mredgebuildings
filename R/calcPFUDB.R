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


  feOdyssee <- calcOutput("ShareOdyssee",
                          subtype = "enduse_carrier",
                          feOnly = TRUE,
                          aggregate = FALSE) %>%
    as.quitte()


  sharesOdyssee <- calcOutput("ShareOdyssee",
                          subtype = "enduse_carrier",
                          feOnly = FALSE,
                          aggregate = FALSE) %>%
    as.quitte()



  # Mappings

  # ETP mapping
  regmapping <- toolGetMapping("regionmappingIEA_ETP.csv",
                               "regional",
                               "mredgebuildings")

  # Enduse Mappings
  enduseMapping <- toolGetMapping("enduseMap_PFUDB.csv", "sectoral",
                                  "mredgebuildings")



  # PROCESS DATA ---------------------------------------------------------------

  pfu <- pfu %>%
    # Generalize Heat Carriers
    sumDf(c("Heat", "Geothermal", "Solar"), "heat") %>%

    # Map Carrier Names and Convert Units
    revalue.levels(carrier = carriersnames) %>%
    factor.data.frame() %>%
    mutate(value = replace_na(.data[["value"]], 0)) %>%
    select("region", "period", "unit", "carrier", "enduse", "value")



  ## Disaggregate into Thermal and Non-Thermal Part ====

  # Aggregate uses from PFU to appliances_light and keep the df as Non-thermal
  pfuNonTherm <- pfu %>%
    filter(.data[["enduse"]] != "Low-T heat") %>%
    aggregate_map(mapping = enduseMapping,
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

  # prepare regional mapping
  regmapping <- regmapping %>%
    mutate(regionAgg = ifelse(.data[["EEAReg"]] == "rest",
                              .data[["OECD"]],
                              .data[["EEAReg"]])) %>%
    select(region = "CountryCode", "regionAgg")

  # Extract regions with existing disaggregated FE shares
  replaceRegs <- sharesOdyssee %>%
    filter(!is.na(.data[["value"]])) %>%
    pull("region") %>%
    droplevels() %>%
    unique()

  # re-aggregate Odyssee shares to carrier level
  sharesOdyssee <- sharesOdyssee %>%
    filter(.data[["region"]] %in% replaceRegs) %>%
    mutate(value = ifelse(is.na(.data[["value"]]),
                          0,
                          .data[["value"]])) %>%
    group_by(across(all_of(c("region", "period", "carrier")))) %>%
    mutate(value = proportions(.data[["value"]])) %>%
    ungroup()


  # Reduce the data frames dimensions to the minimal set
  pfu <- pfu %>%
    semi_join(sharesEU, by = c("region", "period"))
  sharesEU <- sharesEU %>%
    semi_join(pfu, by = c("region", "period"))

  # reaggregate end use shares to ETP regions
  # TODO: This aggregation should be done within calcOutput but for this, we
  # need a full region mapping that doesn't require the post processing done
  # above. I don't know yet, why in come cases, there are different values in
  # one region but we take the mean in this case. To be checked.
  sharesEU <- sharesEU %>%
    left_join(regmapping, by = "region") %>%
    group_by(across(all_of(c("regionAgg", "period", "enduse")))) %>%
    summarise(value = mean(.data[["value"]]), .groups = "drop") %>%
    rename(region = "regionAgg")


  # data excluded from disaggregation
  pfuThermExclude <- pfu %>%
    # exclude regions with existing disaggregated shares
    filter(.data[["enduse"]] == "Low-T heat",
           .data[["region"]] %in% replaceRegs) %>%
    select(-"enduse") %>%

    # disaggregate with existing shares
    left_join(sharesOdyssee %>%
                select("region", "period", "carrier", "enduse", "value") %>%
                rename(share = "value"),
              by = c("region", "period", "carrier")) %>%
    mutate(value = .data[["value"]] * .data[["share"]]) %>%
    select(-"share")



  # Disaggregate Low-T Heat into different enduses
  pfuThermFE <- pfu %>%
    filter(.data[["enduse"]] == "Low-T heat",
           !(.data[["region"]] %in% replaceRegs),
           unit == "fe") %>%
    select(-"enduse") %>%
    toolDisaggregate(sharesEU,
                     exclude,
                     feOdyssee,
                     regmapping) %>%
    select(colnames(pfuNonTherm))

  # Use carrier-enduse distribution to apply on useful energy
  shares <- pfuThermFE %>%
    group_by(across(all_of(c("region", "period", "carrier")))) %>%
    mutate(share = proportions(.data[["value"]])) %>%
    select(-"value", -"unit")

  # disaggregate useful energy with calculated shares
  pfuTherm <- pfu %>%
    filter(.data[["enduse"]] == "Low-T heat",
           .data[["unit"]]   == "ue",
           !(.data[["region"]] %in% replaceRegs)) %>%
    select(-"enduse") %>%
    left_join(shares, by = c("region", "period", "carrier")) %>%
    mutate(value = .data[["value"]] * .data[["share"]]) %>%
    select(-"share") %>%
    rbind(pfuThermFE)


  # Join Non-Thermal and Thermal Part
  pfuRes <- rbind(pfuTherm, pfuThermExclude, pfuNonTherm)


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

  result <- pfuRes %>%
    as.quitte() %>%
    as.magpie()


  data <- list(
    x = result,
    weight = NULL,
    unit = "EJ",
    min = 0,
    description = "Primary Fuel and Useful Energy Data Base"
  )

  return(data)

}
