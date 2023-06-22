#' Converts Data from Primary Fuel and Useful Energy Data Base (PFUDB)
#'
#' PFU Data is disaggregated into different thermal and non-thermal Enduses
#' using FE Shares from Odyssee and IEA_ETP.
#'
#' This was adapted from EDGE function 'getPFUDB.R'.
#'
#' @returns magpie object
#'
#' @author Hagen Tockhorn
#'
#' @importFrom madrat readSource
#' @importFrom quitte aggregate_map
#'
#' @export


calcPFUDB <- function(){

  # FUNCTIONS ------------------------------------------------------------------

  # Sum to Carrier Level
  sum_df <- function(df, variables, newname) {
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
  carriersnames <- c(
    "Coal Products"      = "coal",
    "Electricity"        = "elec",
    "Natural Gas"        = "natgas",
    "Petroleum Products" = "petrol",
    "heat"               = "heat",
    "Hydro"              = "hydro",
    "Nuclear"            = "nuclear",
    "Other"              = "other",
    "Wind"               = "wind"
  )

  # Enduse-Carrier combinations which will be systematically excluded
  exclude <- c("appliances-natgas",
               "appliances-petrol",
               "appliances-biomod",
               "appliances-biotrad",
               "appliances-coal",
               "appliances-heat",
               "refrigerators-natgas",
               "refrigerators-petrol",
               "refrigerators-biomod",
               "refrigerators-biotrad",
               "refrigerators-coal",
               "refrigerators-heat",
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



  # READ-IN DATA ---------------------------------------------------------------

  pfu <- readSource("PFUDB") %>%
    as.quitte()


  # Relevant toolDisaggregate Input-Data

  sharesEU <- calcOutput("Shares",
                         subtype = "enduse_thermal",
                         aggregate=FALSE)  %>%
    as.quitte()

  etpEU <- calcOutput("Shares",
                      subtype = "enduse_thermal",
                      feOnly = TRUE,
                      aggregate = FALSE) %>%
              as.quitte()

  sharesOdyssee <- calcOutput("ShareOdyssee",
                              subtype = "enduse_carrier",
                              aggregate = FALSE) %>%
              as.quitte()


  # ETP mapping
  regmapping <- toolGetMapping("regionmappingIEA_ETP.csv", where = "mappingfolder", type = "regional")



  # PROCESS DATA ---------------------------------------------------------------

  # Generalize Heat Carriers
  pfu <- sum_df(pfu, c("Heat", "Geothermal", "Solar"), "heat")


  # Map Carrier Names and Convert Units
  pfu <- pfu %>%
    revalue.levels(carrier = carriersnames) %>%
    quitte::factor.data.frame() %>%
    mutate(value = replace_na(.data[["value"]],0))


  pfu <- select(pfu, -"model",-"scenario",-"variable")


  ## Disaggregate into Thermal and Non-Thermal Part ----------------------------

  # Enduse Mappings
  enduseMapping <- data.frame(enduse = c("Light", "Stationary power", "Other"),
                              newuse = "appliances_light")
  enduseMappingRef <- data.frame(enduse = c("refrigerators", "appliances_light"),
                                 new_use = "appliances_light")


  # Aggregate uses from PFU to appliances_light and keep the df as Non-thermal
  pfuNonTherm <- pfu %>%
    filter(enduse != "Low-T heat") %>%
    aggregate_map(mapping = enduseMapping,
                  by = "enduse",
                  variable = "enduse",
                  forceAggregation = TRUE)


  # Prepare toolDisaggregate Input

  sharesOdyssee <- sharesOdyssee %>%
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


  # Disaggregate Low-T Heat into different enduses
  pfuTherm <- pfu %>%
    filter(enduse == "Low-T heat") %>%
    select(-"enduse") %>%
    rename(variable = "carrier") %>%
    toolDisaggregate(sharesEU, etpEU, exclude = exclude, sharesReplace = sharesOdyssee) %>%
    select(colnames(pfuNonTherm))


  # Join Non-Thermal and Thermal Part
  pfu_res <- rbind(pfuTherm,pfuNonTherm)


  # Include "refrigerators" in "appliances_light"
  pfu_res <- rbind(
    pfu_res %>%
      filter(!(.data[["enduse"]] %in% enduseMappingRef[[1]])),
    pfu_res %>%
      aggregate_map(mapping = enduseMappingRef,
                    by = "enduse",
                    variable = "enduse",
                    forceAggregation = TRUE))


  # OUTPUT ---------------------------------------------------------------------

  pfu_res <- pfu_res %>%
    as.data.frame() %>%
    as.magpie()


  data <- list(
    x = pfu_res,
    weight = NULL,
    unit = "EJ",
    min = 0,
    description = "Primary Fuel and Useful Energy Data Base"
  )

  return(data)





}
